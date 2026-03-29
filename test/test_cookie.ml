(** Cookie module tests *)

open Alcotest

let make_req ?(headers = []) path =
  let raw = Http.Request.make ~meth:`GET ~headers:(Http.Header.of_list headers) path in
  let body_source = Eio.Flow.string_source "" |> Eio.Buf_read.of_flow ~max_size:1024 in
  Kirin.Request.make ~raw ~body_source
;;

(** Check if [sub] is a substring of [s]. *)
let contains_substring s sub =
  let slen = String.length s in
  let sublen = String.length sub in
  if sublen > slen
  then false
  else (
    let rec check i =
      if i > slen - sublen
      then false
      else if String.sub s i sublen = sub
      then true
      else check (i + 1)
    in
    check 0)
;;

(* -- parse_cookies ------------------------------------------------- *)

let test_parse_single () =
  let cookies = Kirin.Cookie.parse_cookies "name=value" in
  check int "one cookie" 1 (List.length cookies);
  check (option string) "name" (Some "value") (List.assoc_opt "name" cookies)
;;

let test_parse_multiple () =
  let cookies = Kirin.Cookie.parse_cookies "a=1; b=2; c=3" in
  check int "three cookies" 3 (List.length cookies);
  check (option string) "a" (Some "1") (List.assoc_opt "a" cookies);
  check (option string) "b" (Some "2") (List.assoc_opt "b" cookies);
  check (option string) "c" (Some "3") (List.assoc_opt "c" cookies)
;;

let test_parse_with_spaces () =
  let cookies = Kirin.Cookie.parse_cookies "  foo = bar ; baz = qux  " in
  check (option string) "foo" (Some "bar") (List.assoc_opt "foo" cookies);
  check (option string) "baz" (Some "qux") (List.assoc_opt "baz" cookies)
;;

let test_parse_empty () =
  let cookies = Kirin.Cookie.parse_cookies "" in
  check int "no cookies" 0 (List.length cookies)
;;

let parse_tests =
  [ test_case "single cookie" `Quick test_parse_single
  ; test_case "multiple cookies" `Quick test_parse_multiple
  ; test_case "cookies with spaces" `Quick test_parse_with_spaces
  ; test_case "empty header" `Quick test_parse_empty
  ]
;;

(* -- get from request ---------------------------------------------- *)

let test_get_cookie_from_req () =
  let req = make_req ~headers:[ "cookie", "session=abc123; lang=en" ] "/" in
  check (option string) "session" (Some "abc123") (Kirin.Cookie.get "session" req);
  check (option string) "lang" (Some "en") (Kirin.Cookie.get "lang" req);
  check (option string) "missing" None (Kirin.Cookie.get "nope" req)
;;

let test_get_all () =
  let req = make_req ~headers:[ "cookie", "x=1; y=2" ] "/" in
  let all = Kirin.Cookie.get_all req in
  check int "count" 2 (List.length all)
;;

let test_get_no_cookie_header () =
  let req = make_req "/" in
  check (option string) "no header" None (Kirin.Cookie.get "anything" req)
;;

let get_tests =
  [ test_case "get specific cookie" `Quick test_get_cookie_from_req
  ; test_case "get all cookies" `Quick test_get_all
  ; test_case "no cookie header" `Quick test_get_no_cookie_header
  ]
;;

(* -- build_set_cookie / serialization ------------------------------ *)

let test_build_minimal () =
  let attrs =
    { Kirin.Cookie.default_attributes with
      path = None
    ; http_only = false
    ; same_site = None
    }
  in
  let s = Kirin.Cookie.build_set_cookie "k" "v" attrs in
  check bool "starts with k=" true (contains_substring s "k=")
;;

let test_build_all_attributes () =
  let attrs =
    { Kirin.Cookie.max_age = Some 3600
    ; expires = Some "Thu, 01 Jan 2026 00:00:00 GMT"
    ; domain = Some ".example.com"
    ; path = Some "/app"
    ; secure = true
    ; http_only = true
    ; same_site = Some `Strict
    }
  in
  let s = Kirin.Cookie.build_set_cookie "token" "xyz" attrs in
  check bool "has Max-Age" true (contains_substring s "Max-Age=3600");
  check bool "has Expires" true (contains_substring s "Expires=");
  check bool "has Domain" true (contains_substring s "Domain=.example.com");
  check bool "has Path" true (contains_substring s "Path=/app");
  check bool "has Secure" true (contains_substring s "Secure");
  check bool "has HttpOnly" true (contains_substring s "HttpOnly");
  check bool "has SameSite=Strict" true (contains_substring s "SameSite=Strict")
;;

let test_build_samesite_lax () =
  let attrs = { Kirin.Cookie.default_attributes with same_site = Some `Lax } in
  let s = Kirin.Cookie.build_set_cookie "k" "v" attrs in
  check bool "SameSite=Lax" true (contains_substring s "SameSite=Lax")
;;

let test_build_samesite_none () =
  let attrs = { Kirin.Cookie.default_attributes with same_site = Some `None } in
  let s = Kirin.Cookie.build_set_cookie "k" "v" attrs in
  check bool "SameSite=None" true (contains_substring s "SameSite=None")
;;

let build_tests =
  [ test_case "minimal cookie" `Quick test_build_minimal
  ; test_case "all attributes" `Quick test_build_all_attributes
  ; test_case "samesite lax" `Quick test_build_samesite_lax
  ; test_case "samesite none" `Quick test_build_samesite_none
  ]
;;

(* -- set / delete on response -------------------------------------- *)

let test_set_cookie_on_response () =
  let resp = Kirin.Response.text "ok" in
  let resp = Kirin.Cookie.set "sid" "abc" resp in
  let h = Kirin.Response.header "set-cookie" resp in
  check bool "has set-cookie" true (Option.is_some h);
  let v = Option.get h in
  check bool "contains sid=" true (contains_substring v "sid=")
;;

let test_delete_cookie () =
  let resp = Kirin.Response.text "ok" in
  let resp = Kirin.Cookie.delete "sid" resp in
  let h = Kirin.Response.header "set-cookie" resp in
  check bool "has set-cookie" true (Option.is_some h);
  let v = Option.get h in
  check bool "Max-Age=0" true (contains_substring v "Max-Age=0")
;;

let set_tests =
  [ test_case "set cookie on response" `Quick test_set_cookie_on_response
  ; test_case "delete cookie" `Quick test_delete_cookie
  ]
;;

(* -- signed cookies ------------------------------------------------ *)

let test_set_secret_too_short () =
  check_raises
    "short secret"
    (Failure "Cookie secret key must be at least 32 characters")
    (fun () -> Kirin.Cookie.set_secret "short")
;;

let test_sign_and_verify () =
  Kirin.Cookie.set_secret "this-is-a-very-long-secret-key-for-testing-purposes";
  let sig_value = Kirin.Cookie.sign "hello" in
  check bool "signature is non-empty" true (String.length sig_value > 0);
  let signed = "hello." ^ sig_value in
  let result = Kirin.Cookie.verify signed in
  check (option string) "verified" (Some "hello") result
;;

let test_verify_tampered () =
  Kirin.Cookie.set_secret "this-is-a-very-long-secret-key-for-testing-purposes";
  let sig_value = Kirin.Cookie.sign "hello" in
  let tampered = "tampered." ^ sig_value in
  let result = Kirin.Cookie.verify tampered in
  check (option string) "tampered fails" None result
;;

let test_verify_no_dot () =
  Kirin.Cookie.set_secret "this-is-a-very-long-secret-key-for-testing-purposes";
  let result = Kirin.Cookie.verify "noseparator" in
  check (option string) "no dot fails" None result
;;

let signed_tests =
  [ test_case "secret too short" `Quick test_set_secret_too_short
  ; test_case "sign and verify" `Quick test_sign_and_verify
  ; test_case "tampered value" `Quick test_verify_tampered
  ; test_case "no separator" `Quick test_verify_no_dot
  ]
;;

(* -- run ----------------------------------------------------------- *)

let () =
  Eio_main.run
  @@ fun _env ->
  Alcotest.run
    "Cookie"
    [ "parse", parse_tests
    ; "get", get_tests
    ; "build_set_cookie", build_tests
    ; "set/delete", set_tests
    ; "signed", signed_tests
    ]
;;
