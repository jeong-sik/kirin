(** Cookie tests *)

open Alcotest
open Test_helpers

let test_cookie_parse () =
  let cookies = Kirin.Cookie.parse_cookies "session=abc123; user=john" in
  check int "cookie count" 2 (List.length cookies);
  check (option string) "session cookie" (Some "abc123") (List.assoc_opt "session" cookies);
  check (option string) "user cookie" (Some "john") (List.assoc_opt "user" cookies)

let test_cookie_parse_empty () =
  let cookies = Kirin.Cookie.parse_cookies "" in
  check int "empty cookie" 0 (List.length cookies)

let test_cookie_build_simple () =
  let header = Kirin.Cookie.build_set_cookie "session" "xyz" Kirin.Cookie.default_attributes in
  check bool "contains name=value" true (String.length header > 0);
  check bool "has HttpOnly" true (string_contains header "; HttpOnly");
  check bool "has SameSite=Lax" true (string_contains header "; SameSite=Lax")

let test_cookie_set_on_response () =
  let resp = Kirin.html "<h1>Test</h1>"
    |> Kirin.set_cookie "token" "abc123" in
  let set_cookie = Kirin.Response.header "set-cookie" resp in
  check bool "has set-cookie header" true (Option.is_some set_cookie);
  match set_cookie with
  | Some v -> check bool "contains token" true (string_contains v "token=")
  | None -> failwith "no set-cookie header"

let test_cookie_delete () =
  let resp = Kirin.html "<h1>Test</h1>"
    |> Kirin.delete_cookie "session" in
  let set_cookie = Kirin.Response.header "set-cookie" resp in
  match set_cookie with
  | Some v ->
    check bool "has Max-Age=0" true (string_contains v "; Max-Age=0");
    check bool "has Expires" true (string_contains v "; Expires=")
  | None -> failwith "no set-cookie header"

let test_cookie_signed () =
  (* Set up secret key *)
  Kirin.set_cookie_secret "this-is-a-very-secure-secret-key-at-least-32-chars";

  (* Sign a value *)
  let signed = Kirin.Cookie.sign "hello" in
  check bool "signature not empty" true (String.length signed > 0);

  (* Verify the signed value *)
  let signed_cookie = "hello." ^ signed in
  let verified = Kirin.Cookie.verify signed_cookie in
  check (option string) "verified value" (Some "hello") verified;

  (* Tampered value should fail *)
  let tampered = "world." ^ signed in
  let tampered_result = Kirin.Cookie.verify tampered in
  check (option string) "tampered should fail" None tampered_result

let tests = [
  test_case "parse cookies" `Quick test_cookie_parse;
  test_case "parse empty cookies" `Quick test_cookie_parse_empty;
  test_case "build set-cookie header" `Quick test_cookie_build_simple;
  test_case "set cookie on response" `Quick test_cookie_set_on_response;
  test_case "delete cookie" `Quick test_cookie_delete;
  test_case "signed cookies" `Quick test_cookie_signed;
]
