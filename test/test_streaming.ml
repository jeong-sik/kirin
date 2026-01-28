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
      try
        while true do
          let chunk = Eio.Stream.take stream in
          if chunk = "" then raise End_of_file;
          Buffer.add_string buf chunk
        done;
        Buffer.contents buf
      with End_of_file -> Buffer.contents buf
    ) in
    
    (* Producer execution *)
    p stream;
    
    let result = Eio.Promise.await consumer in
    (match result with
    | Ok s -> check string "body" "chunk1chunk2chunk3" s
    | Error e -> fail (Printexc.to_string e))
    
  | Kirin.Response.String _ -> fail "Expected Producer body, got String"
  | Kirin.Response.Stream _ -> fail "Expected Producer body, got Stream"

let streaming_tests = [
  test_case "stream response" `Quick test_stream_response;
]

let () =
  run "Streaming" [
    "Eio Stream", streaming_tests;
  ]
