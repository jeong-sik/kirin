(** Server-Sent Events module tests *)

open Alcotest

(** Check if [sub] is a substring of [s]. *)
let contains s sub =
  let slen = String.length s in
  let sublen = String.length sub in
  if sublen > slen then false
  else
    let rec check i =
      if i > slen - sublen then false
      else if String.sub s i sublen = sub then true
      else check (i + 1)
    in
    check 0

(* -- event constructors -------------------------------------------- *)

let test_data_only () =
  let e = Kirin.Sse.data "hello" in
  check (option string) "no event type" None e.Kirin.Sse.event;
  check string "data" "hello" e.Kirin.Sse.data;
  check (option string) "no id" None e.Kirin.Sse.id;
  check (option int) "no retry" None e.Kirin.Sse.retry

let test_event_with_type () =
  let e = Kirin.Sse.event "message" "payload" in
  check (option string) "event type" (Some "message") e.Kirin.Sse.event;
  check string "data" "payload" e.Kirin.Sse.data

let test_event_typed () =
  let e = Kirin.Sse.event_typed ~event_type:"custom" "body" in
  check (option string) "event type" (Some "custom") e.Kirin.Sse.event;
  check string "data" "body" e.Kirin.Sse.data

let test_with_id () =
  let e = Kirin.Sse.data "test" |> Kirin.Sse.with_id "42" in
  check (option string) "id" (Some "42") e.Kirin.Sse.id

let test_with_retry () =
  let e = Kirin.Sse.data "test" |> Kirin.Sse.with_retry 5000 in
  check (option int) "retry" (Some 5000) e.Kirin.Sse.retry

let test_chained_builders () =
  let e = Kirin.Sse.event "tick" "data"
    |> Kirin.Sse.with_id "99"
    |> Kirin.Sse.with_retry 3000
  in
  check (option string) "event" (Some "tick") e.Kirin.Sse.event;
  check string "data" "data" e.Kirin.Sse.data;
  check (option string) "id" (Some "99") e.Kirin.Sse.id;
  check (option int) "retry" (Some 3000) e.Kirin.Sse.retry

let constructor_tests = [
  test_case "data only" `Quick test_data_only;
  test_case "event with type" `Quick test_event_with_type;
  test_case "event_typed" `Quick test_event_typed;
  test_case "with_id" `Quick test_with_id;
  test_case "with_retry" `Quick test_with_retry;
  test_case "chained builders" `Quick test_chained_builders;
]

(* -- encode -------------------------------------------------------- *)

let test_encode_data_only () =
  let e = Kirin.Sse.data "hello" in
  let encoded = Kirin.Sse.encode e in
  check string "data-only" "data: hello\n\n" encoded

let test_encode_with_event_type () =
  let e = Kirin.Sse.event "update" "new data" in
  let encoded = Kirin.Sse.encode e in
  check bool "has event field" true (contains encoded "event: update\n");
  check bool "has data field" true (contains encoded "data: new data\n");
  check bool "ends with double newline" true
    (let len = String.length encoded in
     len >= 2 && String.sub encoded (len - 2) 2 = "\n\n")

let test_encode_with_id () =
  let e = Kirin.Sse.data "msg" |> Kirin.Sse.with_id "7" in
  let encoded = Kirin.Sse.encode e in
  check bool "has id field" true (contains encoded "id: 7\n")

let test_encode_with_retry () =
  let e = Kirin.Sse.data "msg" |> Kirin.Sse.with_retry 1000 in
  let encoded = Kirin.Sse.encode e in
  check bool "has retry field" true (contains encoded "retry: 1000\n")

let test_encode_multiline_data () =
  let e = Kirin.Sse.data "line1\nline2\nline3" in
  let encoded = Kirin.Sse.encode e in
  check bool "first line" true (contains encoded "data: line1\n");
  check bool "second line" true (contains encoded "data: line2\n");
  check bool "third line" true (contains encoded "data: line3\n")

let test_encode_full_event () =
  let e = Kirin.Sse.event "msg" "payload"
    |> Kirin.Sse.with_id "42"
    |> Kirin.Sse.with_retry 5000
  in
  let encoded = Kirin.Sse.encode e in
  check bool "has event" true (contains encoded "event: msg\n");
  check bool "has data" true (contains encoded "data: payload\n");
  check bool "has id" true (contains encoded "id: 42\n");
  check bool "has retry" true (contains encoded "retry: 5000\n")

let encode_tests = [
  test_case "data only" `Quick test_encode_data_only;
  test_case "with event type" `Quick test_encode_with_event_type;
  test_case "with id" `Quick test_encode_with_id;
  test_case "with retry" `Quick test_encode_with_retry;
  test_case "multiline data" `Quick test_encode_multiline_data;
  test_case "full event" `Quick test_encode_full_event;
]

(* -- ping ---------------------------------------------------------- *)

let test_ping () =
  check string "ping format" ": ping\n\n" Kirin.Sse.ping

let ping_tests = [
  test_case "ping comment" `Quick test_ping;
]

(* -- broadcaster --------------------------------------------------- *)

let test_broadcaster_subscribe_unsubscribe () =
  Eio_main.run @@ fun _env ->
  let b = Kirin.Sse.Broadcaster.create () in
  let (id, _stream) = Kirin.Sse.Broadcaster.subscribe b in
  check bool "id >= 0" true (id >= 0);
  Kirin.Sse.Broadcaster.unsubscribe b id

let test_broadcaster_broadcast () =
  Eio_main.run @@ fun _env ->
  let b = Kirin.Sse.Broadcaster.create () in
  let (_id1, stream1) = Kirin.Sse.Broadcaster.subscribe b in
  let (_id2, stream2) = Kirin.Sse.Broadcaster.subscribe b in
  let evt = Kirin.Sse.data "broadcast-test" in
  Kirin.Sse.Broadcaster.broadcast b evt;
  let received1 = Eio.Stream.take_nonblocking stream1 in
  let received2 = Eio.Stream.take_nonblocking stream2 in
  check bool "client1 received" true (Option.is_some received1);
  check bool "client2 received" true (Option.is_some received2);
  check string "client1 data" "broadcast-test" (Option.get received1).Kirin.Sse.data;
  check string "client2 data" "broadcast-test" (Option.get received2).Kirin.Sse.data

let test_broadcaster_unsubscribed_not_receive () =
  Eio_main.run @@ fun _env ->
  let b = Kirin.Sse.Broadcaster.create () in
  let (id1, stream1) = Kirin.Sse.Broadcaster.subscribe b in
  Kirin.Sse.Broadcaster.unsubscribe b id1;
  Kirin.Sse.Broadcaster.broadcast b (Kirin.Sse.data "after-unsub");
  let received = Eio.Stream.take_nonblocking stream1 in
  check bool "unsubscribed gets nothing" true (Option.is_none received)

let broadcaster_tests = [
  test_case "subscribe/unsubscribe" `Quick test_broadcaster_subscribe_unsubscribe;
  test_case "broadcast to all" `Quick test_broadcaster_broadcast;
  test_case "unsubscribed skipped" `Quick test_broadcaster_unsubscribed_not_receive;
]

(* -- response_legacy ----------------------------------------------- *)

let test_response_legacy_empty () =
  let resp = Kirin.Sse.response_legacy [] in
  check int "200 status" 200 (Kirin.Response.status_code resp);
  check (option string) "content-type" (Some "text/event-stream")
    (Kirin.Response.header "Content-Type" resp)

let test_response_legacy_events () =
  let events = [Kirin.Sse.data "a"; Kirin.Sse.data "b"] in
  let resp = Kirin.Sse.response_legacy events in
  match Kirin.Response.body resp with
  | Kirin.Response.String body ->
    check bool "contains data: a" true (contains body "data: a\n");
    check bool "contains data: b" true (contains body "data: b\n")
  | _ -> fail "expected String body"

let legacy_tests = [
  test_case "empty legacy response" `Quick test_response_legacy_empty;
  test_case "legacy with events" `Quick test_response_legacy_events;
]

(* -- run ----------------------------------------------------------- *)

let () =
  Alcotest.run "SSE" [
    ("constructors", constructor_tests);
    ("encode", encode_tests);
    ("ping", ping_tests);
    ("broadcaster", broadcaster_tests);
    ("response_legacy", legacy_tests);
  ]
