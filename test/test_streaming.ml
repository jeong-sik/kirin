(** Kirin Streaming Tests (Eio-native) *)

open Alcotest

let test_stream_response () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let producer yield =
    yield "chunk1";
    yield "chunk2";
    yield "chunk3"
  in
  let resp = Kirin.Stream.response producer in
  
  match Kirin.Response.body resp with
  | Kirin.Response.Producer p ->
    let stream = Eio.Stream.create 10 in
    let buf = Buffer.create 64 in
    
    (* Consumer fiber *)
    let consumer = Eio.Fiber.fork_promise ~sw (fun () ->
      let rec loop () =
        match Eio.Stream.take stream with
        | None -> Buffer.contents buf
        | Some chunk -> Buffer.add_string buf chunk; loop ()
      in
      loop ()
    ) in
    
    (* Producer execution *)
    p stream;
    
    let result = Eio.Promise.await consumer in
    (match result with
    | Ok s -> check string "body" "chunk1chunk2chunk3" s
    | Error e -> fail (Printexc.to_string e))
    
  | Kirin.Response.String _ -> fail "Expected Producer body, got String"
  | Kirin.Response.Stream _ -> fail "Expected Producer body, got Stream"

let test_stream_eos_none () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let producer yield =
    yield "data1";
    yield "data2"
  in
  let resp = Kirin.Stream.response producer in
  match Kirin.Response.body resp with
  | Kirin.Response.Producer p ->
    let stream = Eio.Stream.create 10 in
    let buf = Buffer.create 64 in
    let consumer = Eio.Fiber.fork_promise ~sw (fun () ->
      let rec loop () =
        match Eio.Stream.take stream with
        | None -> Buffer.contents buf
        | Some chunk -> Buffer.add_string buf chunk; loop ()
      in
      loop ()
    ) in
    p stream;
    let result = Eio.Promise.await consumer in
    (match result with
    | Ok s -> check string "eos none" "data1data2" s
    | Error e -> fail (Printexc.to_string e))
  | _ -> fail "Expected Producer body"

let test_empty_string_not_eos () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let producer yield =
    yield "before";
    yield "";  (* Empty string should NOT be EOS *)
    yield "after"
  in
  let resp = Kirin.Stream.response producer in
  match Kirin.Response.body resp with
  | Kirin.Response.Producer p ->
    let stream = Eio.Stream.create 10 in
    let buf = Buffer.create 64 in
    let consumer = Eio.Fiber.fork_promise ~sw (fun () ->
      let rec loop () =
        match Eio.Stream.take stream with
        | None -> Buffer.contents buf
        | Some chunk -> Buffer.add_string buf chunk; loop ()
      in
      loop ()
    ) in
    p stream;
    let result = Eio.Promise.await consumer in
    (match result with
    | Ok s -> check string "empty string passes through" "beforeafter" s
    | Error e -> fail (Printexc.to_string e))
  | _ -> fail "Expected Producer body"

let streaming_tests = [
  test_case "stream response" `Quick test_stream_response;
  test_case "EOS via None" `Quick test_stream_eos_none;
  test_case "empty string is not EOS" `Quick test_empty_string_not_eos;
]

(** {1 Producer Fiber Cancellation Tests (#42)}

    Verify that when a per-request switch ends (simulating client disconnect),
    the producer fiber is cancelled and does not outlive the request. *)

let test_producer_cancelled_on_switch_exit () =
  Eio_main.run @@ fun _env ->
  let cancelled = ref false in
  let producer_started = Eio.Promise.create () in

  let producer stream =
    Eio.Promise.resolve (snd producer_started) ();
    (* Simulate a slow/infinite producer *)
    (try
      for i = 0 to 1000 do
        Eio.Stream.add stream (Some (string_of_int i));
        (* Yield to allow cancellation to propagate *)
        Eio.Fiber.yield ()
      done
    with Eio.Cancel.Cancelled _ ->
      cancelled := true;
      raise (Eio.Cancel.Cancelled (Failure "test")));
    Eio.Stream.add stream None
  in

  (* Create a per-request sub-switch (same pattern as server.ml Producer case) *)
  (try
    Eio.Switch.run @@ fun request_sw ->
    let stream = Eio.Stream.create 16 in
    Eio.Fiber.fork ~sw:request_sw (fun () ->
      (try producer stream with
       | Eio.Cancel.Cancelled _ as exn -> raise exn
       | _ -> Eio.Stream.add stream None)
    );
    (* Wait for producer to start *)
    Eio.Promise.await (fst producer_started);
    (* Consume a few items *)
    let _ = Eio.Stream.take stream in
    let _ = Eio.Stream.take stream in
    (* Simulate client disconnect by failing the switch *)
    Eio.Switch.fail request_sw (Failure "client disconnected")
  with Failure msg when msg = "client disconnected" -> ());

  (* After the switch exits, the producer fiber should have been cancelled *)
  check bool "producer was cancelled" true !cancelled

let test_producer_normal_completion () =
  Eio_main.run @@ fun _env ->
  let completed = ref false in

  Eio.Switch.run @@ fun request_sw ->
  let stream = Eio.Stream.create 16 in
  Eio.Fiber.fork ~sw:request_sw (fun () ->
    (try
      Eio.Stream.add stream (Some "a");
      Eio.Stream.add stream (Some "b");
      Eio.Stream.add stream None;
      completed := true
    with
    | Eio.Cancel.Cancelled _ as exn -> raise exn
    | _ -> Eio.Stream.add stream None)
  );
  (* Consume all items *)
  let buf = Buffer.create 16 in
  let rec consume () =
    match Eio.Stream.take stream with
    | None -> ()
    | Some s -> Buffer.add_string buf s; consume ()
  in
  consume ();
  check string "all data received" "ab" (Buffer.contents buf);
  check bool "producer completed normally" true !completed

let cancellation_tests = [
  test_case "producer cancelled on switch exit" `Quick test_producer_cancelled_on_switch_exit;
  test_case "producer normal completion" `Quick test_producer_normal_completion;
]

(* Header-injection regression tests.

   [Kirin.Stream.file_response]/[file_inline] interpolate caller-supplied
   [filename] and [content_type] strings into HTTP response header
   values via [Printf.sprintf]. CR/LF/NUL in either is CWE-113 response
   splitting; a double-quote in [filename] terminates the
   ["attachment; filename=\"...\""] quoted-string and lets the caller
   inject extra Content-Disposition directives in the same header.

   These tests pin each disallowed byte individually so a future
   "simplification" that drops one case fails this suite instead of
   the regression sitting silent until exploited. *)
let expect_invalid_arg name f =
  match f () with
  | exception Invalid_argument _ -> ()
  | _ -> failf "%s: expected Invalid_argument, got success" name

let test_file_response_rejects_filename_crlf () =
  Eio_main.run @@ fun _env ->
  expect_invalid_arg "CR in filename"
    (fun () -> Kirin.Stream.file_response ~filename:"a\rb" "/tmp/test");
  expect_invalid_arg "LF in filename"
    (fun () -> Kirin.Stream.file_response ~filename:"a\nb" "/tmp/test");
  expect_invalid_arg "NUL in filename"
    (fun () -> Kirin.Stream.file_response ~filename:"a\x00b" "/tmp/test")

let test_file_response_rejects_filename_quote () =
  (* A bare quote closes the Content-Disposition quoted-string and lets
     the caller append directives. *)
  Eio_main.run @@ fun _env ->
  expect_invalid_arg "double-quote in filename"
    (fun () -> Kirin.Stream.file_response ~filename:"a\";x=y" "/tmp/test")

let test_file_response_rejects_content_type_crlf () =
  Eio_main.run @@ fun _env ->
  expect_invalid_arg "CRLF in content_type"
    (fun () ->
       Kirin.Stream.file_response
         ~filename:"safe.txt"
         ~content_type:"text/plain\r\nSet-Cookie: x=y"
         "/tmp/test")

let test_file_inline_rejects_content_type_crlf () =
  Eio_main.run @@ fun _env ->
  expect_invalid_arg "CRLF in inline content_type"
    (fun () ->
       Kirin.Stream.file_inline
         ~content_type:"text/plain\r\nSet-Cookie: x=y"
         "/tmp/test")

let test_file_response_accepts_safe_filename () =
  (* The validator must not be over-eager: ordinary names with spaces,
     dots, hyphens, parens and non-ASCII bytes must still pass. *)
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun _sw ->
  let resp =
    Kirin.Stream.file_response
      ~filename:"report (2026).pdf"
      ~content_type:"application/pdf"
      "/tmp/test"
  in
  (* Just observing that construction did not raise is the assertion;
     the producer body is not executed because we never read it. *)
  ignore resp;
  check bool "constructed" true true

let header_injection_tests = [
  test_case "file_response rejects CRLF/NUL in filename" `Quick
    test_file_response_rejects_filename_crlf;
  test_case "file_response rejects double-quote in filename" `Quick
    test_file_response_rejects_filename_quote;
  test_case "file_response rejects CRLF in content_type" `Quick
    test_file_response_rejects_content_type_crlf;
  test_case "file_inline rejects CRLF in content_type" `Quick
    test_file_inline_rejects_content_type_crlf;
  test_case "file_response accepts safe filename" `Quick
    test_file_response_accepts_safe_filename;
]

let () =
  run "Streaming" [
    "Eio Stream", streaming_tests;
    "Cancellation", cancellation_tests;
    "Header Injection", header_injection_tests;
  ]
