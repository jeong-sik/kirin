(** SSE (Server-Sent Events) tests *)

open Alcotest
open Test_helpers

let test_sse_encode_simple () =
  let evt = Kirin.sse_data "hello world" in
  let encoded = Kirin.sse_encode evt in
  check string "simple event" "data: hello world\n\n" encoded
;;

let test_sse_encode_multiline () =
  let evt = Kirin.sse_data "line1\nline2\nline3" in
  let encoded = Kirin.sse_encode evt in
  check string "multiline event" "data: line1\ndata: line2\ndata: line3\n\n" encoded
;;

let test_sse_encode_with_type () =
  let evt = Kirin.sse_event "message" "test payload" in
  let encoded = Kirin.sse_encode evt in
  check string "typed event" "event: message\ndata: test payload\n\n" encoded
;;

let test_sse_encode_with_id () =
  let evt = Kirin.sse_data "data" |> Kirin.sse_with_id "evt-123" in
  let encoded = Kirin.sse_encode evt in
  check string "event with id" "data: data\nid: evt-123\n\n" encoded
;;

let test_sse_encode_with_retry () =
  let evt = Kirin.sse_data "reconnect" |> Kirin.sse_with_retry 5000 in
  let encoded = Kirin.sse_encode evt in
  check string "event with retry" "data: reconnect\nretry: 5000\n\n" encoded
;;

let test_sse_encode_full () =
  let evt =
    Kirin.sse_event "update" "new data"
    |> Kirin.sse_with_id "42"
    |> Kirin.sse_with_retry 3000
  in
  let encoded = Kirin.sse_encode evt in
  check
    string
    "full event"
    "event: update\ndata: new data\nid: 42\nretry: 3000\n\n"
    encoded
;;

let test_sse_response () =
  let events = [ Kirin.sse_data "first"; Kirin.sse_data "second" ] in
  let resp = Kirin.sse_response events in
  check
    (option string)
    "content-type"
    (Some "text/event-stream")
    (Kirin.Response.header "content-type" resp);
  check
    (option string)
    "cache-control"
    (Some "no-cache")
    (Kirin.Response.header "cache-control" resp);
  let body = response_body_to_string (Kirin.Response.body resp) in
  check bool "contains first" true (string_contains body "data: first");
  check bool "contains second" true (string_contains body "data: second")
;;

let test_sse_ping () =
  let ping = Kirin.sse_ping in
  check string "ping format" ": ping\n\n" ping
;;

let tests =
  [ test_case "encode simple" `Quick test_sse_encode_simple
  ; test_case "encode multiline" `Quick test_sse_encode_multiline
  ; test_case "encode with type" `Quick test_sse_encode_with_type
  ; test_case "encode with id" `Quick test_sse_encode_with_id
  ; test_case "encode with retry" `Quick test_sse_encode_with_retry
  ; test_case "encode full" `Quick test_sse_encode_full
  ; test_case "response headers" `Quick test_sse_response
  ; test_case "ping" `Quick test_sse_ping
  ]
;;
