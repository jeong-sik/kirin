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

let tests = [
  test_case "parse strong etag" `Quick test_etag_parse_strong;
  test_case "parse weak etag" `Quick test_etag_parse_weak;
  test_case "etag to string" `Quick test_etag_to_string;
  test_case "generate etag" `Quick test_etag_generate;
  test_case "etag matches" `Quick test_etag_matches;
  test_case "middleware adds etag" `Quick test_etag_middleware_adds_header;
  test_case "middleware 304" `Quick test_etag_middleware_304;
]
