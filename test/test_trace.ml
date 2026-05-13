(** Trace module tests - W3C Trace Context, span operations, exporters *)

open Alcotest

(* ============================================================
   Span Operations
   ============================================================ *)

let test_start_creates_span () =
  let span = Kirin.Trace.start ~name:"test-op" () in
  check int "trace_id length" 32 (String.length span.trace_id);
  check int "span_id length" 16 (String.length span.span_id);
  check string "name" "test-op" span.name;
  check (option reject) "no end_time" None span.end_time;
  check bool "status ok" true (span.status = `Ok);
  check (list (pair string string)) "no attributes" [] span.attributes

let test_start_with_trace_id () =
  let tid = "0123456789abcdef0123456789abcdef" in
  let span = Kirin.Trace.start ~name:"child" ~trace_id:tid () in
  check string "trace_id" tid span.trace_id

let test_start_with_parent_id () =
  let pid = "abcdef0123456789" in
  let span = Kirin.Trace.start ~name:"child" ~parent_id:pid () in
  check (option string) "parent_id" (Some pid) span.parent_id

let test_finish_sets_end_time () =
  let span = Kirin.Trace.start ~name:"op" () in
  check (option reject) "before finish" None span.end_time;
  Kirin.Trace.finish span;
  check bool "has end_time" true (Option.is_some span.end_time)

let test_set_attr () =
  let span = Kirin.Trace.start ~name:"op" () in
  Kirin.Trace.set_attr span ~key:"http.method" ~value:"GET";
  Kirin.Trace.set_attr span ~key:"http.path" ~value:"/api";
  check int "2 attributes" 2 (List.length span.attributes);
  check (option string) "method attr" (Some "GET")
    (List.assoc_opt "http.method" span.attributes);
  check (option string) "path attr" (Some "/api")
    (List.assoc_opt "http.path" span.attributes)

let test_set_error () =
  let span = Kirin.Trace.start ~name:"op" () in
  check bool "initially ok" true (span.status = `Ok);
  Kirin.Trace.set_error span "timeout";
  check bool "now error" true (span.status = `Error "timeout")

let test_duration () =
  Eio_main.run @@ fun env ->
  let span = Kirin.Trace.start ~name:"op" () in
  Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
  Kirin.Trace.finish span;
  let dur = Kirin.Trace.duration span in
  check bool "duration > 0" true (dur > 0.0);
  check bool "duration < 1s" true (dur < 1.0)

(* ============================================================
   W3C Trace Context
   ============================================================ *)

let test_parse_traceparent_valid () =
  let header = "00-0123456789abcdef0123456789abcdef-abcdef0123456789-01" in
  match Kirin.Trace.parse_traceparent header with
  | Some (tid, pid) ->
    check string "trace_id" "0123456789abcdef0123456789abcdef" tid;
    check string "parent_id" "abcdef0123456789" pid
  | None -> fail "expected Some"

let test_parse_traceparent_invalid_short_tid () =
  let header = "00-short-abcdef0123456789-01" in
  check (option reject) "invalid tid" None
    (Kirin.Trace.parse_traceparent header)

let test_parse_traceparent_invalid_format () =
  check (option reject) "empty" None (Kirin.Trace.parse_traceparent "");
  check (option reject) "no dashes" None (Kirin.Trace.parse_traceparent "garbage")

let test_parse_traceparent_valid_w3c () =
  let header = "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01" in
  match Kirin.Trace.parse_traceparent header with
  | Some (tid, pid) ->
    check string "trace_id" "0af7651916cd43dd8448eb211c80319c" tid;
    check string "parent_id" "b7ad6b7169203331" pid
  | None -> fail "expected Some for valid W3C traceparent"

let test_parse_traceparent_reject_uppercase () =
  let header = "00-0AF7651916CD43DD8448EB211C80319C-b7ad6b7169203331-01" in
  check (option reject) "uppercase trace_id" None
    (Kirin.Trace.parse_traceparent header)

let test_parse_traceparent_reject_non_hex () =
  let header = "00-zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz-b7ad6b7169203331-01" in
  check (option reject) "non-hex chars" None
    (Kirin.Trace.parse_traceparent header)

let test_parse_traceparent_reject_wrong_length () =
  let header = "00-0af765-b7ad6b7169203331-01" in
  check (option reject) "short trace_id" None
    (Kirin.Trace.parse_traceparent header)

let test_parse_traceparent_all_zeros () =
  let header = "00-00000000000000000000000000000000-0000000000000000-00" in
  match Kirin.Trace.parse_traceparent header with
  | Some (tid, pid) ->
    check string "zero trace_id" "00000000000000000000000000000000" tid;
    check string "zero parent_id" "0000000000000000" pid
  | None -> fail "expected Some for all-zeros traceparent"

let test_to_traceparent () =
  let span = Kirin.Trace.start ~name:"op"
    ~trace_id:"0123456789abcdef0123456789abcdef" () in
  let tp = Kirin.Trace.to_traceparent span in
  check bool "starts with 00-" true (String.length tp > 3 && String.sub tp 0 3 = "00-");
  check bool "contains trace_id" true
    (String.length tp >= 36 &&
     String.sub tp 3 32 = "0123456789abcdef0123456789abcdef");
  check bool "ends with -01" true
    (String.length tp >= 2 &&
     String.sub tp (String.length tp - 2) 2 = "01")

let test_traceparent_roundtrip () =
  let span = Kirin.Trace.start ~name:"op" () in
  let tp = Kirin.Trace.to_traceparent span in
  match Kirin.Trace.parse_traceparent tp with
  | Some (tid, pid) ->
    check string "trace_id roundtrip" span.trace_id tid;
    check string "span_id roundtrip" span.span_id pid
  | None -> fail "roundtrip parse failed"

(* ============================================================
   ID Generation
   ============================================================ *)

let test_new_trace_id_length () =
  let id = Kirin.Trace.new_trace_id () in
  check int "trace_id 32 chars" 32 (String.length id)

let test_new_span_id_length () =
  let id = Kirin.Trace.new_span_id () in
  check int "span_id 16 chars" 16 (String.length id)

let test_ids_are_unique () =
  let id1 = Kirin.Trace.new_trace_id () in
  let id2 = Kirin.Trace.new_trace_id () in
  check bool "different trace_ids" true (id1 <> id2)

let test_ids_are_hex () =
  let id = Kirin.Trace.new_trace_id () in
  let is_hex c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') in
  String.iter (fun c ->
    check bool (Printf.sprintf "hex char '%c'" c) true (is_hex c)
  ) id

(* ============================================================
   Context Integration (Hmap)
   ============================================================ *)

let test_span_key_store_retrieve () =
  let span = Kirin.Trace.start ~name:"test" () in
  let ctx = Hmap.add Kirin.Trace.span_key span Hmap.empty in
  match Hmap.find Kirin.Trace.span_key ctx with
  | Some s -> check string "same span" span.span_id s.span_id
  | None -> fail "span not found in context"

(* ============================================================
   Exporter Interface
   ============================================================ *)

let test_stderr_exporter_no_crash () =
  let span = Kirin.Trace.start ~name:"test-export" () in
  Kirin.Trace.set_attr span ~key:"test" ~value:"true";
  Kirin.Trace.finish span;
  let module E = Kirin.Trace.Stderr_exporter in
  E.export span

let test_json_exporter_no_crash () =
  let span = Kirin.Trace.start ~name:"test-json" () in
  Kirin.Trace.set_attr span ~key:"k1" ~value:"v1";
  Kirin.Trace.set_error span "test error";
  Kirin.Trace.finish span;
  let module E = Kirin.Trace.Json_exporter in
  E.export span

let test_json_exporter_no_attrs () =
  let span = Kirin.Trace.start ~name:"empty-attrs" () in
  Kirin.Trace.finish span;
  let module E = Kirin.Trace.Json_exporter in
  E.export span

(* ============================================================
   Test Suite
   ============================================================ *)

let () =
  run "Trace" [
    "span_operations", [
      test_case "start creates span" `Quick test_start_creates_span;
      test_case "start with trace_id" `Quick test_start_with_trace_id;
      test_case "start with parent_id" `Quick test_start_with_parent_id;
      test_case "finish sets end_time" `Quick test_finish_sets_end_time;
      test_case "set_attr" `Quick test_set_attr;
      test_case "set_error" `Quick test_set_error;
      test_case "duration" `Quick test_duration;
    ];
    "w3c_trace_context", [
      test_case "parse valid traceparent" `Quick test_parse_traceparent_valid;
      test_case "reject short trace_id" `Quick test_parse_traceparent_invalid_short_tid;
      test_case "reject invalid format" `Quick test_parse_traceparent_invalid_format;
      test_case "valid W3C example" `Quick test_parse_traceparent_valid_w3c;
      test_case "reject uppercase hex" `Quick test_parse_traceparent_reject_uppercase;
      test_case "reject non-hex chars" `Quick test_parse_traceparent_reject_non_hex;
      test_case "reject wrong length" `Quick test_parse_traceparent_reject_wrong_length;
      test_case "all zeros valid" `Quick test_parse_traceparent_all_zeros;
      test_case "to_traceparent format" `Quick test_to_traceparent;
      test_case "traceparent roundtrip" `Quick test_traceparent_roundtrip;
    ];
    "id_generation", [
      test_case "trace_id length" `Quick test_new_trace_id_length;
      test_case "span_id length" `Quick test_new_span_id_length;
      test_case "ids are unique" `Quick test_ids_are_unique;
      test_case "ids are hex" `Quick test_ids_are_hex;
    ];
    "context_integration", [
      test_case "span_key store/retrieve" `Quick test_span_key_store_retrieve;
    ];
    "exporters", [
      test_case "stderr exporter no crash" `Quick test_stderr_exporter_no_crash;
      test_case "json exporter no crash" `Quick test_json_exporter_no_crash;
      test_case "json exporter empty attrs" `Quick test_json_exporter_no_attrs;
    ];
  ]
