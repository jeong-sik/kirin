(** ETag tests *)

open Alcotest
open Test_helpers

let test_etag_parse_strong () =
  let etag = Kirin.Etag.parse "\"abc123\"" in
  match etag with
  | Kirin.Etag.Strong s -> check string "strong etag value" "abc123" s
  | Kirin.Etag.Weak _ -> failwith "expected strong etag"

let test_etag_parse_weak () =
  let etag = Kirin.Etag.parse "W/\"xyz789\"" in
  match etag with
  | Kirin.Etag.Weak w -> check string "weak etag value" "xyz789" w
  | Kirin.Etag.Strong _ -> failwith "expected weak etag"

let test_etag_to_string () =
  let strong = Kirin.Etag.Strong "abc" in
  let weak = Kirin.Etag.Weak "xyz" in
  check string "strong to string" "\"abc\"" (Kirin.Etag.to_string strong);
  check string "weak to string" "W/\"xyz\"" (Kirin.Etag.to_string weak)

let test_etag_generate () =
  let content = "Hello, World!" in
  let etag = Kirin.Etag.generate content in
  match etag with
  | Kirin.Etag.Strong s ->
    check bool "hash not empty" true (String.length s > 0);
    (* Same content should produce same hash *)
    let etag2 = Kirin.Etag.generate content in
    (match etag2 with
    | Kirin.Etag.Strong s2 -> check string "deterministic hash" s s2
    | _ -> failwith "expected strong")
  | _ -> failwith "expected strong etag"

let test_etag_matches () =
  let e1 = Kirin.Etag.Strong "abc" in
  let e2 = Kirin.Etag.Strong "abc" in
  let e3 = Kirin.Etag.Strong "xyz" in
  let e4 = Kirin.Etag.Weak "abc" in
  check bool "same strong match" true (Kirin.Etag.matches e1 e2);
  check bool "different strong no match" false (Kirin.Etag.matches e1 e3);
  check bool "weak comparison match" true (Kirin.Etag.matches ~weak_comparison:true e1 e4)

let test_etag_middleware_adds_header () =
  let handler _req = Kirin.html "<h1>Test</h1>" in
  let with_etag = Kirin.etag handler in
  let req = make_test_request "/" in
  let resp = with_etag req in
  let etag_header = Kirin.Response.header "etag" resp in
  check bool "has etag header" true (Option.is_some etag_header)

let test_etag_middleware_304 () =
  let content = "<h1>Test</h1>" in
  let handler _req = Kirin.html content in

  (* First, get the ETag *)
  let with_etag = Kirin.etag handler in
  let req1 = make_test_request "/" in
  let resp1 = with_etag req1 in
  let etag_value = Kirin.Response.header "etag" resp1 in

  (* Now make request with If-None-Match *)
  match etag_value with
  | Some etag ->
    let req2 = make_test_request
      ~headers:[("if-none-match", etag)]
      ~meth:`GET
      "/" in
    let resp2 = with_etag req2 in
    check int "304 status" 304 (Kirin.Response.status_code resp2)
  | None -> failwith "no etag header"

(* RFC 7232 §3.1 — If-Match MUST use strong comparison.

   Before the fix, [check_if_match] reused [parse_if_none_match] +
   [any_match] which defaulted to weak comparison, so a client could
   pass W/"v" against Strong "v" and bypass the precondition. These
   tests pin each side of the new strong-only semantics. *)

let test_etag_if_match_strong_passes () =
  let req = make_test_request
    ~headers:[("if-match", "\"abc\"")]
    ~meth:`PUT
    "/" in
  let result = Kirin.Etag.check_if_match req (Kirin.Etag.Strong "abc") in
  check bool "strong client + strong server passes" true (result = `Ok)

let test_etag_if_match_weak_rejected () =
  (* RFC says: W/"v" against Strong "v" must NOT satisfy If-Match.
     Two clients holding weak validators of "same" semantic content
     cannot use that to coordinate a lost-update race. *)
  let req = make_test_request
    ~headers:[("if-match", "W/\"abc\"")]
    ~meth:`PUT
    "/" in
  let result = Kirin.Etag.check_if_match req (Kirin.Etag.Strong "abc") in
  check bool "weak client + strong server rejected" true
    (result = `Precondition_failed)

let test_etag_if_match_weak_to_weak_rejected () =
  (* Even weak/weak is rejected under strong comparison: weak ETags
     are explicitly outside the allowed set for If-Match. *)
  let req = make_test_request
    ~headers:[("if-match", "W/\"abc\"")]
    ~meth:`PUT
    "/" in
  let result = Kirin.Etag.check_if_match req (Kirin.Etag.Weak "abc") in
  check bool "weak/weak rejected for If-Match" true
    (result = `Precondition_failed)

let test_etag_if_match_wildcard_passes () =
  let req = make_test_request
    ~headers:[("if-match", "*")]
    ~meth:`PUT
    "/" in
  let result = Kirin.Etag.check_if_match req (Kirin.Etag.Strong "anything") in
  check bool "wildcard passes" true (result = `Ok);
  let result_weak = Kirin.Etag.check_if_match req (Kirin.Etag.Weak "anything") in
  check bool "wildcard passes even for weak server etag" true (result_weak = `Ok)

let test_etag_if_match_absent_passes () =
  let req = make_test_request ~meth:`PUT "/" in
  let result = Kirin.Etag.check_if_match req (Kirin.Etag.Strong "anything") in
  check bool "no header => no precondition" true (result = `Ok)

let test_etag_generate_uses_sha256 () =
  (* Pin the digest output size. SHA-256 hex is 64 chars; MD5 was 32.
     A future refactor that quietly drops back to MD5 (smaller hash,
     attractive perf number) would trip this. The pin makes the
     digest choice load-bearing rather than incidental. *)
  let etag = Kirin.Etag.generate "Hello, World!" in
  match etag with
  | Kirin.Etag.Strong s ->
    check int "SHA-256 hex length" 64 (String.length s)
  | _ -> failwith "expected strong etag"

let tests = [
  test_case "parse strong etag" `Quick test_etag_parse_strong;
  test_case "parse weak etag" `Quick test_etag_parse_weak;
  test_case "etag to string" `Quick test_etag_to_string;
  test_case "generate etag" `Quick test_etag_generate;
  test_case "etag matches" `Quick test_etag_matches;
  test_case "middleware adds etag" `Quick test_etag_middleware_adds_header;
  test_case "middleware 304" `Quick test_etag_middleware_304;
  test_case "if-match strong passes" `Quick test_etag_if_match_strong_passes;
  test_case "if-match weak rejected" `Quick test_etag_if_match_weak_rejected;
  test_case "if-match weak/weak rejected" `Quick test_etag_if_match_weak_to_weak_rejected;
  test_case "if-match wildcard passes" `Quick test_etag_if_match_wildcard_passes;
  test_case "if-match absent passes" `Quick test_etag_if_match_absent_passes;
  test_case "generate uses SHA-256" `Quick test_etag_generate_uses_sha256;
]
