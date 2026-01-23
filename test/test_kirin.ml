(** Kirin Test Suite
    Tests the public API through Kirin module
*)

open Alcotest

(* ============================================================
   Response Tests (can test without a running server)
   ============================================================ *)

let test_response_html () =
  let resp = Kirin.html "<h1>Hello</h1>" in
  check string "html body" "<h1>Hello</h1>" (Kirin.Response.body resp);
  check (option string) "content-type" (Some "text/html; charset=utf-8")
    (Kirin.Response.header "content-type" resp)

let test_response_json () =
  let json = `Assoc [("status", `String "ok"); ("count", `Int 42)] in
  let resp = Kirin.json json in
  check string "json body" {|{"status":"ok","count":42}|} (Kirin.Response.body resp);
  check (option string) "content-type" (Some "application/json; charset=utf-8")
    (Kirin.Response.header "content-type" resp)

let test_response_text () =
  let resp = Kirin.text "plain text" in
  check string "text body" "plain text" (Kirin.Response.body resp);
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

let response_tests = [
  test_case "html response" `Quick test_response_html;
  test_case "json response" `Quick test_response_json;
  test_case "text response" `Quick test_response_text;
  test_case "redirect response" `Quick test_response_redirect;
  test_case "not found response" `Quick test_response_not_found;
  test_case "with header" `Quick test_response_with_header;
]

(* ============================================================
   Router Tests (test routing logic)
   ============================================================ *)

let make_test_request ?(meth=`GET) path =
  let raw = Http.Request.make ~meth path in
  Kirin.Request.make ~raw ~body:""

let test_router_static_match () =
  let handler _req = Kirin.text "matched" in
  let routes = [Kirin.get "/" handler; Kirin.get "/about" handler] in
  let router = Kirin.router routes in

  let req = make_test_request "/" in
  let resp = router req in

  check string "static match body" "matched" (Kirin.Response.body resp)

let test_router_param_extraction () =
  let handler req =
    let id = Kirin.param "id" req in
    Kirin.text ("User: " ^ id)
  in
  let routes = [Kirin.get "/users/:id" handler] in
  let router = Kirin.router routes in

  let req = make_test_request "/users/123" in
  let resp = router req in

  check string "param extraction" "User: 123" (Kirin.Response.body resp)

let test_router_multi_params () =
  let handler req =
    let user_id = Kirin.param "user_id" req in
    let post_id = Kirin.param "post_id" req in
    Kirin.text (Printf.sprintf "User %s, Post %s" user_id post_id)
  in
  let routes = [Kirin.get "/users/:user_id/posts/:post_id" handler] in
  let router = Kirin.router routes in

  let req = make_test_request "/users/42/posts/99" in
  let resp = router req in

  check string "multi params" "User 42, Post 99" (Kirin.Response.body resp)

let test_router_method_matching () =
  let get_handler _req = Kirin.text "GET" in
  let post_handler _req = Kirin.text "POST" in
  let routes = [
    Kirin.get "/resource" get_handler;
    Kirin.post "/resource" post_handler;
  ] in
  let router = Kirin.router routes in

  let get_resp = router (make_test_request ~meth:`GET "/resource") in
  let post_resp = router (make_test_request ~meth:`POST "/resource") in

  check string "GET method" "GET" (Kirin.Response.body get_resp);
  check string "POST method" "POST" (Kirin.Response.body post_resp)

let test_router_404 () =
  let handler _req = Kirin.text "found" in
  let routes = [Kirin.get "/exists" handler] in
  let router = Kirin.router routes in

  let req = make_test_request "/not-exists" in
  let resp = router req in

  check int "404 status" 404 (Kirin.Response.status_code resp)

let router_tests = [
  test_case "static route match" `Quick test_router_static_match;
  test_case "param extraction" `Quick test_router_param_extraction;
  test_case "multi params" `Quick test_router_multi_params;
  test_case "method matching" `Quick test_router_method_matching;
  test_case "404 for unknown" `Quick test_router_404;
]

(* ============================================================
   Handler Tests (integration style)
   ============================================================ *)

let test_json_api () =
  let handler req =
    match Kirin.query_opt "format" req with
    | Some "compact" -> Kirin.json (`Assoc [("ok", `Bool true)])
    | _ -> Kirin.json (`Assoc [("status", `String "success"); ("data", `Null)])
  in
  let routes = [Kirin.get "/api/status" handler] in
  let router = Kirin.router routes in

  let req = make_test_request "/api/status" in
  let resp = router req in

  check (option string) "json content-type" (Some "application/json; charset=utf-8")
    (Kirin.Response.header "content-type" resp)

let handler_tests = [
  test_case "json api handler" `Quick test_json_api;
]

(* ============================================================
   Static Tests
   ============================================================ *)

let test_static_mime_type () =
  check string "html" "text/html; charset=utf-8" (Kirin.Static.get_mime_type "index.html");
  check string "css" "text/css; charset=utf-8" (Kirin.Static.get_mime_type "style.css");
  check string "js" "application/javascript; charset=utf-8" (Kirin.Static.get_mime_type "app.js");
  check string "png" "image/png" (Kirin.Static.get_mime_type "logo.png");
  check string "json" "application/json; charset=utf-8" (Kirin.Static.get_mime_type "data.json");
  check string "unknown" "application/octet-stream" (Kirin.Static.get_mime_type "file.xyz")

let test_static_safe_path () =
  check bool "normal path" true (Kirin.Static.is_safe_path "css/style.css");
  check bool "root file" true (Kirin.Static.is_safe_path "index.html");
  check bool "nested" true (Kirin.Static.is_safe_path "assets/js/app.js");
  check bool "traversal" false (Kirin.Static.is_safe_path "../etc/passwd");
  check bool "hidden traversal" false (Kirin.Static.is_safe_path "foo/../../bar")

let static_tests = [
  test_case "mime type detection" `Quick test_static_mime_type;
  test_case "path safety check" `Quick test_static_safe_path;
]

(* ============================================================
   Cookie Tests
   ============================================================ *)

(* String.is_substring helper - using simple search *)
let string_contains haystack needle =
  let needle_len = String.length needle in
  let haystack_len = String.length haystack in
  if needle_len > haystack_len then false
  else
    let rec check i =
      if i > haystack_len - needle_len then false
      else if String.sub haystack i needle_len = needle then true
      else check (i + 1)
    in
    check 0

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

let cookie_tests = [
  test_case "parse cookies" `Quick test_cookie_parse;
  test_case "parse empty cookies" `Quick test_cookie_parse_empty;
  test_case "build set-cookie header" `Quick test_cookie_build_simple;
  test_case "set cookie on response" `Quick test_cookie_set_on_response;
  test_case "delete cookie" `Quick test_cookie_delete;
  test_case "signed cookies" `Quick test_cookie_signed;
]

(* ============================================================
   ETag Tests
   ============================================================ *)

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
    let raw = Http.Request.make
      ~headers:(Http.Header.of_list [("if-none-match", etag)])
      ~meth:`GET "/" in
    let req2 = Kirin.Request.make ~raw ~body:"" in
    let resp2 = with_etag req2 in
    check int "304 status" 304 (Kirin.Response.status_code resp2)
  | None -> failwith "no etag header"

let etag_tests = [
  test_case "parse strong etag" `Quick test_etag_parse_strong;
  test_case "parse weak etag" `Quick test_etag_parse_weak;
  test_case "etag to string" `Quick test_etag_to_string;
  test_case "generate etag" `Quick test_etag_generate;
  test_case "etag matches" `Quick test_etag_matches;
  test_case "middleware adds etag" `Quick test_etag_middleware_adds_header;
  test_case "middleware 304" `Quick test_etag_middleware_304;
]

(* ============================================================
   Multipart Tests (RFC 7578)
   ============================================================ *)

let test_multipart_extract_boundary () =
  let ct = "multipart/form-data; boundary=----WebKitFormBoundary7MA4YWxk" in
  let boundary = Kirin.Multipart.extract_boundary ct in
  check (option string) "boundary" (Some "----WebKitFormBoundary7MA4YWxk") boundary

let test_multipart_parse_simple () =
  let boundary = "----boundary123" in
  let body =
    "------boundary123\r\n" ^
    "Content-Disposition: form-data; name=\"username\"\r\n" ^
    "\r\n" ^
    "john_doe\r\n" ^
    "------boundary123\r\n" ^
    "Content-Disposition: form-data; name=\"email\"\r\n" ^
    "\r\n" ^
    "john@example.com\r\n" ^
    "------boundary123--\r\n"
  in
  let result = Kirin.Multipart.parse ~boundary body in
  let fields = Kirin.Multipart.fields result in
  check int "field count" 2 (List.length fields);
  check (option string) "username" (Some "john_doe") (Kirin.Multipart.field "username" result);
  check (option string) "email" (Some "john@example.com") (Kirin.Multipart.field "email" result)

let test_multipart_parse_file () =
  let boundary = "----boundary456" in
  let body =
    "------boundary456\r\n" ^
    "Content-Disposition: form-data; name=\"document\"; filename=\"test.txt\"\r\n" ^
    "Content-Type: text/plain\r\n" ^
    "\r\n" ^
    "Hello, World!\r\n" ^
    "------boundary456--\r\n"
  in
  let result = Kirin.Multipart.parse ~boundary body in
  let files = Kirin.Multipart.files result in
  check int "file count" 1 (List.length files);
  match Kirin.Multipart.file "document" result with
  | Some file ->
    check (option string) "filename" (Some "test.txt") file.filename;
    check (option string) "content-type" (Some "text/plain") file.content_type;
    check string "content" "Hello, World!" file.content
  | None -> failwith "file not found"

let test_multipart_mixed () =
  let boundary = "----boundary789" in
  let body =
    "------boundary789\r\n" ^
    "Content-Disposition: form-data; name=\"title\"\r\n" ^
    "\r\n" ^
    "My Document\r\n" ^
    "------boundary789\r\n" ^
    "Content-Disposition: form-data; name=\"file\"; filename=\"doc.pdf\"\r\n" ^
    "Content-Type: application/pdf\r\n" ^
    "\r\n" ^
    "PDF content here\r\n" ^
    "------boundary789--\r\n"
  in
  let result = Kirin.Multipart.parse ~boundary body in
  check (option string) "title field" (Some "My Document") (Kirin.Multipart.field "title" result);
  check int "files count" 1 (List.length (Kirin.Multipart.files result));
  check int "fields count" 1 (List.length (Kirin.Multipart.fields result))

let multipart_tests = [
  test_case "extract boundary" `Quick test_multipart_extract_boundary;
  test_case "parse simple form" `Quick test_multipart_parse_simple;
  test_case "parse file upload" `Quick test_multipart_parse_file;
  test_case "parse mixed form" `Quick test_multipart_mixed;
]

(* ============================================================
   Compression Tests
   ============================================================ *)

let test_compress_parse_accept_encoding () =
  check (option Alcotest.(testable (fun fmt -> function
    | Kirin.Compress.Gzip -> Format.fprintf fmt "Gzip"
    | Kirin.Compress.Deflate -> Format.fprintf fmt "Deflate"
  ) (=)))
    "gzip preferred" (Some Kirin.Compress.Gzip)
    (Kirin.Compress.parse_accept_encoding "gzip, deflate, br");

  check (option Alcotest.(testable (fun fmt -> function
    | Kirin.Compress.Gzip -> Format.fprintf fmt "Gzip"
    | Kirin.Compress.Deflate -> Format.fprintf fmt "Deflate"
  ) (=)))
    "deflate only" (Some Kirin.Compress.Deflate)
    (Kirin.Compress.parse_accept_encoding "deflate")

let test_compress_skip_for_images () =
  check bool "skip images" true (Kirin.Compress.skip_compression_for "image/png");
  check bool "skip video" true (Kirin.Compress.skip_compression_for "video/mp4");
  check bool "skip gzip" true (Kirin.Compress.skip_compression_for "application/gzip");
  check bool "compress text" false (Kirin.Compress.skip_compression_for "text/html");
  check bool "compress json" false (Kirin.Compress.skip_compression_for "application/json")

let test_compress_gzip () =
  (* Create a large enough input to compress *)
  let input = String.make 2000 'A' in
  let compressed = Kirin.compress_gzip input in
  check bool "compressed smaller" true (String.length compressed < String.length input);
  (* Gzip magic bytes: 0x1f 0x8b *)
  check bool "gzip magic" true
    (String.length compressed >= 2 &&
     Char.code compressed.[0] = 0x1f &&
     Char.code compressed.[1] = 0x8b)

let test_compress_deflate () =
  let input = String.make 2000 'B' in
  let compressed = Kirin.compress_deflate input in
  check bool "compressed smaller" true (String.length compressed < String.length input)

let test_compress_middleware () =
  (* Create a handler that returns large content *)
  let large_content = String.make 2000 'X' in
  let handler _req = Kirin.html large_content in
  let with_compress = Kirin.compress handler in

  (* Request with Accept-Encoding: gzip *)
  let raw = Http.Request.make
    ~headers:(Http.Header.of_list [("accept-encoding", "gzip, deflate")])
    ~meth:`GET "/" in
  let req = Kirin.Request.make ~raw ~body:"" in
  let resp = with_compress req in

  check (option string) "content-encoding" (Some "gzip") (Kirin.Response.header "content-encoding" resp);
  check bool "response compressed" true (String.length (Kirin.Response.body resp) < 2000)

let compress_tests = [
  test_case "parse accept-encoding" `Quick test_compress_parse_accept_encoding;
  test_case "skip compression for" `Quick test_compress_skip_for_images;
  test_case "gzip compression" `Quick test_compress_gzip;
  test_case "deflate compression" `Quick test_compress_deflate;
  test_case "compression middleware" `Quick test_compress_middleware;
]

(* ============================================================
   Rate Limit Tests
   ============================================================ *)

let test_ratelimit_default_config () =
  let config = Kirin.default_rate_limit_config in
  check (float 0.001) "requests per second" 10.0 config.requests_per_second;
  check int "burst size" 20 config.burst_size

let test_ratelimit_middleware_allows () =
  let config : Kirin.rate_limit_config = {
    requests_per_second = 100.0;  (* High rate for test *)
    burst_size = 100;
  } in
  let handler _req = Kirin.text "OK" in
  let with_limit = Kirin.rate_limit ~config handler in

  let raw = Http.Request.make
    ~headers:(Http.Header.of_list [("x-forwarded-for", "127.0.0.1")])
    ~meth:`GET "/" in
  let req = Kirin.Request.make ~raw ~body:"" in
  let resp = with_limit req in

  check int "status 200" 200 (Kirin.Response.status_code resp);
  check bool "has ratelimit header" true
    (Option.is_some (Kirin.Response.header "x-ratelimit-limit" resp))

let test_ratelimit_middleware_limits () =
  let config : Kirin.rate_limit_config = {
    requests_per_second = 0.001;  (* Very low rate *)
    burst_size = 1;               (* Only 1 allowed *)
  } in
  let handler _req = Kirin.text "OK" in
  let with_limit = Kirin.rate_limit ~config
    ~get_key:(fun _ -> "test-limit-client")  (* Fixed key for test *)
    handler in

  let make_req () =
    let raw = Http.Request.make
      ~headers:(Http.Header.of_list [("x-forwarded-for", "192.168.1.100")])
      ~meth:`GET "/" in
    Kirin.Request.make ~raw ~body:""
  in

  (* First request should pass *)
  let resp1 = with_limit (make_req ()) in
  check int "first request 200" 200 (Kirin.Response.status_code resp1);

  (* Second request should be rate limited *)
  let resp2 = with_limit (make_req ()) in
  check int "second request 429" 429 (Kirin.Response.status_code resp2);
  check bool "has retry-after" true
    (Option.is_some (Kirin.Response.header "retry-after" resp2))

let ratelimit_tests = [
  test_case "default config" `Quick test_ratelimit_default_config;
  test_case "middleware allows" `Quick test_ratelimit_middleware_allows;
  test_case "middleware limits" `Quick test_ratelimit_middleware_limits;
]

(* ============================================================
   Main
   ============================================================ *)

let () =
  run "Kirin" [
    ("Response", response_tests);
    ("Router", router_tests);
    ("Handler", handler_tests);
    ("Static", static_tests);
    ("Cookie", cookie_tests);
    ("ETag", etag_tests);
    ("Multipart", multipart_tests);
    ("Compress", compress_tests);
    ("RateLimit", ratelimit_tests);
  ]
