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

let () =
  run "Streaming" [
    "Eio Stream", streaming_tests;
  ]
