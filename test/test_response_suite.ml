(** Response tests - can test without a running server *)

open Alcotest
open Test_helpers

let test_response_html () =
  let resp = Kirin.html "<h1>Hello</h1>" in
  check string "html body" "<h1>Hello</h1>" (response_body_to_string (Kirin.Response.body resp));
  check (option string) "content-type" (Some "text/html; charset=utf-8")
    (Kirin.Response.header "content-type" resp)

let test_response_json () =
  let json = `Assoc [("status", `String "ok"); ("count", `Int 42)] in
  let resp = Kirin.json json in
  check string "json body" {|{"status":"ok","count":42}|} (response_body_to_string (Kirin.Response.body resp));
  check (option string) "content-type" (Some "application/json; charset=utf-8")
    (Kirin.Response.header "content-type" resp)

let test_response_text () =
  let resp = Kirin.text "plain text" in
  check string "text body" "plain text" (response_body_to_string (Kirin.Response.body resp));
  check (option string) "content-type" (Some "text/plain; charset=utf-8")
    (Kirin.Response.header "content-type" resp)

let test_response_redirect () =
  let resp = Kirin.redirect "/new-location" in
  check int "status" 302 (Kirin.Response.status_code resp);
  check (option string) "location" (Some "/new-location")
    (Kirin.Response.header "location" resp)

let test_response_not_found () =
  let resp = Kirin.not_found () in
  check int "status" 404 (Kirin.Response.status_code resp)

let test_response_with_header () =
  let resp = Kirin.html "<p>test</p>"
    |> Kirin.with_header "x-custom" "value" in
  check (option string) "custom header" (Some "value")
    (Kirin.Response.header "x-custom" resp)

let tests = [
  test_case "html response" `Quick test_response_html;
  test_case "json response" `Quick test_response_json;
  test_case "text response" `Quick test_response_text;
  test_case "redirect response" `Quick test_response_redirect;
  test_case "not found response" `Quick test_response_not_found;
  test_case "with header" `Quick test_response_with_header;
]
