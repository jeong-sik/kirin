(** SSE (Server-Sent Events) tests *)

open Alcotest
open Test_helpers

let test_sse_encode_simple () =
  let evt = Kirin.sse_data "hello world" in
  let encoded = Kirin.sse_encode evt in
  check string "simple event" "data: hello world\n\n" encoded

let test_sse_encode_multiline () =
  let evt = Kirin.sse_data "line1\nline2\nline3" in
  let encoded = Kirin.sse_encode evt in
  check string "multiline event" "data: line1\ndata: line2\ndata: line3\n\n" encoded

let test_sse_encode_with_type () =
  let evt = Kirin.sse_event "message" "test payload" in
  let encoded = Kirin.sse_encode evt in
  check string "typed event" "event: message\ndata: test payload\n\n" encoded

let test_sse_encode_with_id () =
  let evt = Kirin.sse_data "data" |> Kirin.sse_with_id "evt-123" in
  let encoded = Kirin.sse_encode evt in
  check string "event with id" "data: data\nid: evt-123\n\n" encoded

let test_sse_encode_with_retry () =
  let evt = Kirin.sse_data "reconnect" |> Kirin.sse_with_retry 5000 in
  let encoded = Kirin.sse_encode evt in
  check string "event with retry" "data: reconnect\nretry: 5000\n\n" encoded

let test_sse_encode_full () =
  let evt = Kirin.sse_event "update" "new data"
    |> Kirin.sse_with_id "42"
    |> Kirin.sse_with_retry 3000 in
  let encoded = Kirin.sse_encode evt in
  check string "full event" "event: update\ndata: new data\nid: 42\nretry: 3000\n\n" encoded

let test_sse_response () =
  let events = [
    Kirin.sse_data "first";
    Kirin.sse_data "second";
  ] in
  let resp = Kirin.sse_response events in

  check (option string) "content-type" (Some "text/event-stream")
    (Kirin.Response.header "content-type" resp);
  check (option string) "cache-control" (Some "no-cache")
    (Kirin.Response.header "cache-control" resp);

  let body = response_body_to_string (Kirin.Response.body resp) in
  check bool "contains first" true (string_contains body "data: first");
  check bool "contains second" true (string_contains body "data: second")

let test_sse_ping () =
  let ping = Kirin.sse_ping in
  check string "ping format" ": ping\n\n" ping

(* SSE wire format is line-oriented and the HTML spec treats CR,
   LF, and CRLF as line breaks.  Before this PR, [encode] passed
   the [event] and [id] fields through to the wire as-is, so a
   single string crossing the boundary could inject extra SSE
   rows — different [event] type, additional [data], a forged
   [id].  That is SSE protocol injection.  New contract:

   - [event] and [id] reject CR/LF at encode time
   - [data] is the only multi-line field, and splits on CR, LF,
     and CRLF — so a stray [\r] no longer escapes onto the wire
     as a receiver-side line break *)

let test_encode_rejects_event_type_lf () =
  check_raises "LF in event type"
    (Invalid_argument
       "Sse.encode: event field contains CR/LF; SSE protocol injection rejected")
    (fun () ->
       ignore
         (Kirin.sse_encode (Kirin.sse_event "msg\ndata: takeover" "x")))

let test_encode_rejects_event_type_cr () =
  check_raises "CR in event type"
    (Invalid_argument
       "Sse.encode: event field contains CR/LF; SSE protocol injection rejected")
    (fun () ->
       ignore (Kirin.sse_encode (Kirin.sse_event "msg\rxxx" "x")))

let test_encode_rejects_id_lf () =
  check_raises "LF in id"
    (Invalid_argument
       "Sse.encode: id field contains CR/LF; SSE protocol injection rejected")
    (fun () ->
       ignore
         (Kirin.sse_encode
            (Kirin.sse_data "d" |> Kirin.sse_with_id "1\ndata: forged")))

let test_encode_rejects_id_cr () =
  check_raises "CR in id"
    (Invalid_argument
       "Sse.encode: id field contains CR/LF; SSE protocol injection rejected")
    (fun () ->
       ignore
         (Kirin.sse_encode
            (Kirin.sse_data "d" |> Kirin.sse_with_id "1\revil")))

let test_encode_data_splits_on_cr () =
  (* A standalone CR in [data] used to escape onto the wire and
     be treated as a line break by the receiver — same injection
     surface as a literal newline.  Pin that we now split on CR
     and emit two data rows. *)
  let evt = Kirin.sse_data "alpha\rbeta" in
  check string "CR splits data"
    "data: alpha\ndata: beta\n\n"
    (Kirin.sse_encode evt)

let test_encode_data_splits_on_crlf () =
  let evt = Kirin.sse_data "line1\r\nline2\r\nline3" in
  check string "CRLF is one break, not two"
    "data: line1\ndata: line2\ndata: line3\n\n"
    (Kirin.sse_encode evt)

let test_encode_event_safe_alphanumerics_unchanged () =
  (* Pin that ordinary event/id values still pass through — the
     validator is not over-eager. *)
  let evt =
    Kirin.sse_event "update" "payload"
    |> Kirin.sse_with_id "abc-123"
    |> Kirin.sse_with_retry 1500
  in
  check string "safe event"
    "event: update\ndata: payload\nid: abc-123\nretry: 1500\n\n"
    (Kirin.sse_encode evt)

let tests = [
  test_case "encode simple" `Quick test_sse_encode_simple;
  test_case "encode multiline" `Quick test_sse_encode_multiline;
  test_case "encode with type" `Quick test_sse_encode_with_type;
  test_case "encode with id" `Quick test_sse_encode_with_id;
  test_case "encode with retry" `Quick test_sse_encode_with_retry;
  test_case "encode full" `Quick test_sse_encode_full;
  test_case "response headers" `Quick test_sse_response;
  test_case "ping" `Quick test_sse_ping;
  test_case "encode rejects LF in event type" `Quick test_encode_rejects_event_type_lf;
  test_case "encode rejects CR in event type" `Quick test_encode_rejects_event_type_cr;
  test_case "encode rejects LF in id" `Quick test_encode_rejects_id_lf;
  test_case "encode rejects CR in id" `Quick test_encode_rejects_id_cr;
  test_case "encode data splits on CR" `Quick test_encode_data_splits_on_cr;
  test_case "encode data splits on CRLF" `Quick test_encode_data_splits_on_crlf;
  test_case "encode safe alphanumerics unchanged" `Quick test_encode_event_safe_alphanumerics_unchanged;
]
