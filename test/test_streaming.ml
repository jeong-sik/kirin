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

let () =
  run "Streaming" [
    "Eio Stream", streaming_tests;
    "Cancellation", cancellation_tests;
  ]
