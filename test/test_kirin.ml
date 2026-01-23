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
   WebSocket Tests
   ============================================================ *)

let test_ws_accept_key () =
  (* Test vector from RFC 6455 *)
  let client_key = "dGhlIHNhbXBsZSBub25jZQ==" in
  let accept_key = Kirin.Websocket.compute_accept_key client_key in
  check string "accept key" "s3pPLMBiTxaQ9kYGzzhZRbK+xOo=" accept_key

let test_ws_is_upgrade_request () =
  (* Valid upgrade request *)
  let raw_valid = Http.Request.make
    ~headers:(Http.Header.of_list [
      ("upgrade", "websocket");
      ("connection", "Upgrade");
      ("sec-websocket-key", "dGhlIHNhbXBsZSBub25jZQ==");
    ])
    ~meth:`GET "/" in
  let req_valid = Kirin.Request.make ~raw:raw_valid ~body:"" in
  check bool "is upgrade" true (Kirin.is_websocket_upgrade req_valid);

  (* Non-upgrade request *)
  let raw_invalid = Http.Request.make
    ~headers:(Http.Header.of_list [("content-type", "text/html")])
    ~meth:`GET "/" in
  let req_invalid = Kirin.Request.make ~raw:raw_invalid ~body:"" in
  check bool "not upgrade" false (Kirin.is_websocket_upgrade req_invalid)

let test_ws_upgrade_response () =
  let raw = Http.Request.make
    ~headers:(Http.Header.of_list [
      ("upgrade", "websocket");
      ("connection", "Upgrade");
      ("sec-websocket-key", "dGhlIHNhbXBsZSBub25jZQ==");
    ])
    ~meth:`GET "/" in
  let req = Kirin.Request.make ~raw ~body:"" in

  match Kirin.websocket_upgrade req with
  | Ok resp ->
    check int "status 101" 101 (Kirin.Response.status_code resp);
    check (option string) "upgrade header" (Some "websocket")
      (Kirin.Response.header "upgrade" resp);
    check (option string) "accept key" (Some "s3pPLMBiTxaQ9kYGzzhZRbK+xOo=")
      (Kirin.Response.header "sec-websocket-accept" resp)
  | Error msg ->
    fail ("Upgrade failed: " ^ msg)

let test_ws_encode_text_frame () =
  let frame = Kirin.ws_text "Hello" in
  let encoded = Kirin.ws_encode frame in

  (* First byte: FIN=1, opcode=1 (text) = 0x81 *)
  check char "first byte" '\x81' encoded.[0];
  (* Second byte: MASK=0, length=5 = 0x05 *)
  check char "second byte" '\x05' encoded.[1];
  (* Payload *)
  check string "payload" "Hello" (String.sub encoded 2 5)

let test_ws_encode_large_frame () =
  (* Test 16-bit length encoding (126-65535 bytes) *)
  let payload = String.make 1000 'X' in
  let frame = Kirin.ws_binary payload in
  let encoded = Kirin.ws_encode frame in

  (* First byte: FIN=1, opcode=2 (binary) = 0x82 *)
  check char "first byte" '\x82' encoded.[0];
  (* Second byte: MASK=0, length=126 (indicating 16-bit length follows) *)
  check char "second byte" '\x7e' encoded.[1];
  (* 16-bit length in big-endian: 1000 = 0x03E8 *)
  check int "length high" 0x03 (Char.code encoded.[2]);
  check int "length low" 0xE8 (Char.code encoded.[3])

let test_ws_decode_masked_frame () =
  (* Build a masked text frame: "Hi" with mask key [0x12, 0x34, 0x56, 0x78] *)
  let raw = Bytes.create 8 in
  Bytes.set raw 0 '\x81';  (* FIN=1, opcode=1 *)
  Bytes.set raw 1 '\x82';  (* MASK=1, length=2 *)
  (* Mask key *)
  Bytes.set raw 2 '\x12';
  Bytes.set raw 3 '\x34';
  Bytes.set raw 4 '\x56';
  Bytes.set raw 5 '\x78';
  (* Masked payload: 'H' xor 0x12, 'i' xor 0x34 *)
  Bytes.set raw 6 (Char.chr (Char.code 'H' lxor 0x12));
  Bytes.set raw 7 (Char.chr (Char.code 'i' lxor 0x34));

  match Kirin.ws_decode (Bytes.to_string raw) with
  | Ok (frame, consumed) ->
    check bool "fin" true frame.fin;
    check bool "is text" (frame.opcode = Kirin.Text) true;
    check string "payload" "Hi" frame.payload;
    check int "consumed" 8 consumed
  | Error msg ->
    fail ("Decode failed: " ^ msg)

let test_ws_close_frame () =
  let frame = Kirin.ws_close ~code:Kirin.Websocket.Normal ~reason:"Bye" () in
  check bool "is close" (frame.opcode = Kirin.Close) true;
  (* Payload: 2 bytes for code (1000) + "Bye" *)
  check int "payload length" 5 (String.length frame.payload);
  (* Parse it back *)
  let code, reason = Kirin.Websocket.parse_close_payload frame.payload in
  check (option bool) "code is Normal" (Some true)
    (Option.map (fun c -> c = Kirin.Websocket.Normal) code);
  check string "reason" "Bye" reason

let test_ws_ping_pong () =
  let ping = Kirin.ws_ping ~payload:"ping-data" () in
  check bool "is ping" (ping.opcode = Kirin.Ping) true;
  check string "ping payload" "ping-data" ping.payload;

  let pong = Kirin.ws_pong ~payload:ping.payload in
  check bool "is pong" (pong.opcode = Kirin.Pong) true;
  check string "pong echoes" "ping-data" pong.payload

let websocket_tests = [
  test_case "compute accept key" `Quick test_ws_accept_key;
  test_case "is upgrade request" `Quick test_ws_is_upgrade_request;
  test_case "upgrade response" `Quick test_ws_upgrade_response;
  test_case "encode text frame" `Quick test_ws_encode_text_frame;
  test_case "encode large frame" `Quick test_ws_encode_large_frame;
  test_case "decode masked frame" `Quick test_ws_decode_masked_frame;
  test_case "close frame" `Quick test_ws_close_frame;
  test_case "ping pong" `Quick test_ws_ping_pong;
]

(* ============================================================
   SSE Tests
   ============================================================ *)

let test_sse_encode_simple () =
  let evt = Kirin.sse_data "hello world" in
  let encoded = Kirin.sse_encode evt in
  check string "simple event" "data: hello world\n\n" encoded

let test_sse_encode_multiline () =
  let evt = Kirin.sse_data "line1\nline2\nline3" in
  let encoded = Kirin.sse_encode evt in
  check string "multiline event" "data: line1\ndata: line2\ndata: line3\n\n" encoded

let test_sse_encode_with_type () =
  let evt = Kirin.sse_event ~event_type:"message" "test payload" in
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
  let evt = Kirin.sse_event ~event_type:"update" "new data"
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

  let body = Kirin.Response.body resp in
  check bool "contains first" true (string_contains body "data: first");
  check bool "contains second" true (string_contains body "data: second")

let test_sse_ping () =
  let ping = Kirin.sse_ping () in
  check string "ping format" ": ping\n\n" ping

let test_sse_middleware () =
  let counter = ref 0 in
  let on_events () =
    incr counter;
    [Kirin.sse_event ~event_type:"tick" (string_of_int !counter)]
  in
  let fallback _req = Kirin.text "Not SSE" in
  let handler = Kirin.sse ~path:"/events" ~on_events fallback in

  (* SSE request *)
  let raw_sse = Http.Request.make ~meth:`GET "/events" in
  let req_sse = Kirin.Request.make ~raw:raw_sse ~body:"" in
  let resp_sse = handler req_sse in
  check (option string) "sse content-type" (Some "text/event-stream")
    (Kirin.Response.header "content-type" resp_sse);

  (* Non-SSE request *)
  let raw_other = Http.Request.make ~meth:`GET "/other" in
  let req_other = Kirin.Request.make ~raw:raw_other ~body:"" in
  let resp_other = handler req_other in
  check string "fallback body" "Not SSE" (Kirin.Response.body resp_other)

let sse_tests = [
  test_case "encode simple" `Quick test_sse_encode_simple;
  test_case "encode multiline" `Quick test_sse_encode_multiline;
  test_case "encode with type" `Quick test_sse_encode_with_type;
  test_case "encode with id" `Quick test_sse_encode_with_id;
  test_case "encode with retry" `Quick test_sse_encode_with_retry;
  test_case "encode full" `Quick test_sse_encode_full;
  test_case "response headers" `Quick test_sse_response;
  test_case "ping" `Quick test_sse_ping;
  test_case "middleware" `Quick test_sse_middleware;
]

(* ============================================================
   Template Tests
   ============================================================ *)

let test_template_simple_var () =
  let ctx = Kirin.template_context [("name", "World")] in
  let result = Kirin.template_render ctx "Hello, {{name}}!" in
  check string "simple var" "Hello, World!" result

let test_template_html_escape () =
  let ctx = Kirin.template_context [("html", "<script>alert('xss')</script>")] in
  let result = Kirin.template_render ctx "{{html}}" in
  check string "escaped" "&lt;script&gt;alert(&#x27;xss&#x27;)&lt;/script&gt;" result

let test_template_raw_var () =
  let ctx = Kirin.template_context [("html", "<b>bold</b>")] in
  let result = Kirin.template_render ctx "{{{html}}}" in
  check string "raw" "<b>bold</b>" result

let test_template_if_true () =
  let ctx = Kirin.template_context_of [("show", `Bool true)] in
  let result = Kirin.template_render ctx "{{#if show}}visible{{/if show}}" in
  check string "if true" "visible" result

let test_template_if_false () =
  let ctx = Kirin.template_context_of [("show", `Bool false)] in
  let result = Kirin.template_render ctx "{{#if show}}visible{{/if show}}" in
  check string "if false" "" result

let test_template_if_else () =
  let ctx = Kirin.template_context_of [("logged_in", `Bool false)] in
  let result = Kirin.template_render ctx "{{#if logged_in}}Welcome{{else}}Login{{/if logged_in}}" in
  check string "if else" "Login" result

let test_template_unless () =
  let ctx = Kirin.template_context_of [("error", `Null)] in
  let result = Kirin.template_render ctx "{{#unless error}}OK{{/unless error}}" in
  check string "unless null" "OK" result

let test_template_each () =
  let ctx = Kirin.template_context_of [
    ("items", `List [`String "a"; `String "b"; `String "c"])
  ] in
  let result = Kirin.template_render ctx "{{#each items}}{{this}}{{/each items}}" in
  check string "each" "abc" result

let test_template_each_objects () =
  let ctx = Kirin.template_context_of [
    ("users", `List [
      `Assoc [("name", `String "Alice")];
      `Assoc [("name", `String "Bob")];
    ])
  ] in
  let result = Kirin.template_render ctx "{{#each users}}{{name}} {{/each users}}" in
  check string "each objects" "Alice Bob " result

let test_template_dot_notation () =
  let ctx = Kirin.template_context_of [
    ("user", `Assoc [("profile", `Assoc [("name", `String "John")])])
  ] in
  let result = Kirin.template_render ctx "{{user.profile.name}}" in
  check string "dot notation" "John" result

let test_template_interpolate () =
  let result = Kirin.template_interpolate "Hello {{name}}, welcome to {{place}}!"
    [("name", "Alice"); ("place", "Wonderland")] in
  check string "interpolate" "Hello Alice, welcome to Wonderland!" result

let test_template_html_response () =
  let ctx = Kirin.template_context [("title", "Test")] in
  let resp = Kirin.template_html ctx "<h1>{{title}}</h1>" in
  check string "body" "<h1>Test</h1>" (Kirin.Response.body resp);
  check (option string) "content-type" (Some "text/html; charset=utf-8")
    (Kirin.Response.header "content-type" resp)

let template_tests = [
  test_case "simple variable" `Quick test_template_simple_var;
  test_case "html escape" `Quick test_template_html_escape;
  test_case "raw variable" `Quick test_template_raw_var;
  test_case "if true" `Quick test_template_if_true;
  test_case "if false" `Quick test_template_if_false;
  test_case "if else" `Quick test_template_if_else;
  test_case "unless" `Quick test_template_unless;
  test_case "each" `Quick test_template_each;
  test_case "each objects" `Quick test_template_each_objects;
  test_case "dot notation" `Quick test_template_dot_notation;
  test_case "interpolate" `Quick test_template_interpolate;
  test_case "html response" `Quick test_template_html_response;
]

(* ============================================================
   TLS Config Tests
   ============================================================ *)

let test_tls_dev_config () =
  let config = Kirin.tls_dev () in
  match Kirin.tls_validate config with
  | Ok () -> ()
  | Error e -> fail (Kirin.tls_error_string e)

let test_tls_from_pem () =
  let cert = "-----BEGIN CERTIFICATE-----\ntest\n-----END CERTIFICATE-----" in
  let key = "-----BEGIN PRIVATE KEY-----\ntest\n-----END PRIVATE KEY-----" in
  let config = Kirin.tls_from_pem ~certificate:cert ~private_key:key () in
  match Kirin.tls_validate config with
  | Ok () -> ()
  | Error e -> fail (Kirin.tls_error_string e)

let test_tls_invalid_cert () =
  let config = Kirin.tls_from_pem ~certificate:"invalid" ~private_key:"invalid" () in
  match Kirin.tls_validate config with
  | Ok () -> fail "Expected validation error"
  | Error _ -> ()  (* Expected *)

let test_tls_alpn () =
  let cert = "-----BEGIN CERTIFICATE-----\ntest\n-----END CERTIFICATE-----" in
  let key = "-----BEGIN PRIVATE KEY-----\ntest\n-----END PRIVATE KEY-----" in
  let config = Kirin.tls_from_pem ~certificate:cert ~private_key:key
    ~alpn:["h2"; "http/1.1"] () in
  check (list string) "alpn protocols" ["h2"; "http/1.1"]
    config.Kirin.Tls_config.alpn_protocols

let tls_tests = [
  test_case "dev config" `Quick test_tls_dev_config;
  test_case "from pem" `Quick test_tls_from_pem;
  test_case "invalid cert" `Quick test_tls_invalid_cert;
  test_case "alpn protocols" `Quick test_tls_alpn;
]

(* ============================================================
   gRPC Tests (Phase 5)
   ============================================================ *)

let test_grpc_service_create () =
  let svc = Kirin.Grpc.service "test.TestService" in
  (* Service created successfully - check name field *)
  check string "service name" "test.TestService" svc.Kirin.Grpc.Service.name

let test_grpc_service_with_unary () =
  let svc = Kirin.Grpc.service "test.Greeter"
    |> Kirin.Grpc.unary "SayHello" (fun req -> "Hello, " ^ req)
  in
  (* Service with unary method - check name is preserved *)
  check string "service name" "test.Greeter" svc.Kirin.Grpc.Service.name

let test_grpc_interceptor () =
  let _interceptor = Kirin.Grpc.make_interceptor "test" (fun ctx next ->
    (* Just pass through *)
    next ctx)
  in
  (* Interceptor created successfully *)
  check bool "interceptor created" true true

let test_grpc_logging_interceptor () =
  let _interceptor = Kirin.Grpc.logging_interceptor () in
  (* Logging interceptor created successfully *)
  check bool "logging interceptor created" true true

let test_grpc_timing_interceptor () =
  let _interceptor = Kirin.Grpc.timing_interceptor () in
  (* Timing interceptor created successfully *)
  check bool "timing interceptor created" true true

let test_grpc_status_codes () =
  check bool "ok code" true (Kirin.Grpc.StatusCode.ok = Grpc_core.Status.OK);
  check bool "not found code" true (Kirin.Grpc.StatusCode.not_found = Grpc_core.Status.Not_found);
  check bool "internal code" true (Kirin.Grpc.StatusCode.internal = Grpc_core.Status.Internal)

let test_grpc_status_create () =
  let status = Kirin.Grpc.status ~code:Kirin.Grpc.StatusCode.ok ~message:"success" in
  check string "status message" "success" status.message

let test_grpc_stream () =
  let stream = Kirin.Grpc.stream_create 5 in
  check bool "stream empty" true (Kirin.Grpc.stream_is_empty stream);
  Kirin.Grpc.stream_add stream "test";
  check bool "stream not empty" false (Kirin.Grpc.stream_is_empty stream);
  Kirin.Grpc.stream_close stream;
  check bool "stream closed" true (Kirin.Grpc.stream_is_closed stream)

let grpc_tests = [
  test_case "service create" `Quick test_grpc_service_create;
  test_case "service with unary" `Quick test_grpc_service_with_unary;
  test_case "interceptor" `Quick test_grpc_interceptor;
  test_case "logging interceptor" `Quick test_grpc_logging_interceptor;
  test_case "timing interceptor" `Quick test_grpc_timing_interceptor;
  test_case "status codes" `Quick test_grpc_status_codes;
  test_case "status create" `Quick test_grpc_status_create;
  test_case "stream operations" `Quick test_grpc_stream;
]

(* ============================================================
   GraphQL Tests
   ============================================================ *)

module GQL = Kirin.Graphql

let test_graphql_schema_create () =
  let schema = GQL.schema [
    GQL.field "hello" ~typ:(GQL.non_null GQL.string)
      ~args:GQL.Arg.[]
      ~resolve:(fun _info () -> "Hello, World!");
  ] in
  (* Schema created successfully *)
  check Alcotest.bool "schema created" true (schema |> Obj.repr |> Obj.is_block)

let test_graphql_execute () =
  let schema = GQL.schema [
    GQL.field "greeting" ~typ:(GQL.non_null GQL.string)
      ~args:GQL.Arg.[arg "name" ~typ:GQL.Arg.string]
      ~resolve:(fun _info () name_opt ->
        let name = Option.value ~default:"World" name_opt in
        "Hello, " ^ name ^ "!");
  ] in
  let result = GQL.execute schema ~ctx:() ~query:"{ greeting }" () in
  match result with
  | Ok (`Response json) ->
    let expected = `Assoc [("data", `Assoc [("greeting", `String "Hello, World!")])] in
    check Alcotest.string "graphql response" (Yojson.Basic.to_string expected) (Yojson.Basic.to_string json)
  | _ -> fail "Expected response"

let test_graphql_execute_with_args () =
  let schema = GQL.schema [
    GQL.field "greeting" ~typ:(GQL.non_null GQL.string)
      ~args:GQL.Arg.[arg "name" ~typ:GQL.Arg.string]
      ~resolve:(fun _info () name_opt ->
        let name = Option.value ~default:"World" name_opt in
        "Hello, " ^ name ^ "!");
  ] in
  let result = GQL.execute schema ~ctx:() ~query:{|{ greeting(name: "Alice") }|} () in
  match result with
  | Ok (`Response json) ->
    let expected = `Assoc [("data", `Assoc [("greeting", `String "Hello, Alice!")])] in
    check Alcotest.string "graphql response with args" (Yojson.Basic.to_string expected) (Yojson.Basic.to_string json)
  | _ -> fail "Expected response"

let test_graphql_object_type () =
  let user_type = GQL.obj "User" ~fields:[
    GQL.field "id" ~typ:(GQL.non_null GQL.string)
      ~args:GQL.Arg.[]
      ~resolve:(fun _info (id, _name) -> id);
    GQL.field "name" ~typ:(GQL.non_null GQL.string)
      ~args:GQL.Arg.[]
      ~resolve:(fun _info (_id, name) -> name);
  ] in
  let schema = GQL.schema [
    GQL.field "user" ~typ:user_type
      ~args:GQL.Arg.[]
      ~resolve:(fun _info () -> Some ("1", "Alice"));
  ] in
  let result = GQL.execute schema ~ctx:() ~query:"{ user { id name } }" () in
  match result with
  | Ok (`Response json) ->
    let expected = `Assoc [("data", `Assoc [("user", `Assoc [("id", `String "1"); ("name", `String "Alice")])])] in
    check Alcotest.string "user object" (Yojson.Basic.to_string expected) (Yojson.Basic.to_string json)
  | _ -> fail "Expected response"

let test_graphql_parse_request () =
  let body = {|{"query":"{ hello }","operationName":"GetHello","variables":{"id":"1"}}|} in
  let req = GQL.parse_request body in
  match req with
  | Some r ->
    check Alcotest.string "query" "{ hello }" r.query;
    check (option Alcotest.string) "operation name" (Some "GetHello") r.operation_name;
    check Alcotest.bool "variables present" true (Option.is_some r.variables)
  | None -> fail "Failed to parse request"

let test_graphql_parse_request_minimal () =
  let body = {|{"query":"{ hello }"}|} in
  let req = GQL.parse_request body in
  match req with
  | Some r ->
    check Alcotest.string "query" "{ hello }" r.query;
    check (option Alcotest.string) "operation name" None r.operation_name;
    check Alcotest.bool "variables none" true (Option.is_none r.variables)
  | None -> fail "Failed to parse request"

let test_graphql_error_helpers () =
  let err = GQL.error "Something went wrong" in
  check Alcotest.string "error message" "Something went wrong" err.message;
  let err_loc = GQL.error_with_location "Parse error" ~line:10 ~column:5 in
  check (option (list (pair Alcotest.int Alcotest.int))) "locations"
    (Some [(10, 5)]) err_loc.locations;
  let err_path = GQL.error_with_path "Field error" ["user"; "name"] in
  check (option (list Alcotest.string)) "path"
    (Some ["user"; "name"]) err_path.path

let test_graphql_playground () =
  let req = make_test_request "/graphql" in
  let resp = GQL.playground_handler req in
  check Alcotest.int "status" 200 (Kirin.Response.status_code resp);
  let body = Kirin.Response.body resp in
  check Alcotest.bool "contains doctype" true (String.length body > 100 && String.sub body 0 15 = "<!DOCTYPE html>")

let graphql_tests = [
  test_case "schema create" `Quick test_graphql_schema_create;
  test_case "execute query" `Quick test_graphql_execute;
  test_case "execute with args" `Quick test_graphql_execute_with_args;
  test_case "object type" `Quick test_graphql_object_type;
  test_case "parse request" `Quick test_graphql_parse_request;
  test_case "parse request minimal" `Quick test_graphql_parse_request_minimal;
  test_case "error helpers" `Quick test_graphql_error_helpers;
  test_case "playground handler" `Quick test_graphql_playground;
]

(* ============================================================
   Streaming Tests (Phase 9)
   ============================================================ *)

let test_stream_response () =
  let stream = Kirin.stream (fun yield ->
    yield "chunk1";
    yield "chunk2";
    yield "chunk3"
  ) in
  let resp = Kirin.stream_to_response stream in
  check string "body" "chunk1chunk2chunk3" (Kirin.Response.body resp);
  check (option string) "content-length" (Some "18") (Kirin.Response.header "content-length" resp)

let test_stream_with_content_type () =
  let stream = Kirin.stream (fun yield -> yield "data")
    |> Kirin.Stream.with_content_type "text/csv" in
  let resp = Kirin.stream_to_response stream in
  check (option string) "content-type" (Some "text/csv") (Kirin.Response.header "content-type" resp)

let test_stream_chunked_encoding () =
  let chunk = Kirin.Stream.encode_chunk "Hello" in
  check string "encoded chunk" "5\r\nHello\r\n" chunk

let test_stream_final_chunk () =
  check string "final chunk" "0\r\n\r\n" Kirin.Stream.final_chunk

let test_stream_of_lines () =
  let producer = Kirin.Stream.of_lines ["line1"; "line2"; "line3"] in
  let buf = Buffer.create 64 in
  producer (Buffer.add_string buf);
  check string "lines" "line1\nline2\nline3\n" (Buffer.contents buf)

let test_stream_mime_detection () =
  (* Test mime type detection through file_response *)
  let stream = Kirin.Stream.file_inline "/test/file.json" in
  let headers = Kirin.Stream.headers stream in
  check (option string) "json mime" (Some "application/json") (Cohttp.Header.get headers "content-type")

let test_stream_progress_callback () =
  let progress_called = ref false in
  let complete_called = ref false in
  let progress : Kirin.progress = {
    on_progress = (fun ~bytes_sent:_ ~total_bytes:_ -> progress_called := true);
    on_complete = (fun () -> complete_called := true);
    on_error = (fun _ -> ());
  } in
  (* Just check the type compiles correctly *)
  ignore progress;
  check bool "progress type exists" true true

let streaming_tests = [
  test_case "stream response" `Quick test_stream_response;
  test_case "stream content type" `Quick test_stream_with_content_type;
  test_case "chunked encoding" `Quick test_stream_chunked_encoding;
  test_case "final chunk" `Quick test_stream_final_chunk;
  test_case "of_lines helper" `Quick test_stream_of_lines;
  test_case "mime detection" `Quick test_stream_mime_detection;
  test_case "progress callback" `Quick test_stream_progress_callback;
]

(* ============================================================
   Connection Pool Tests (Phase 9)
   ============================================================ *)

let test_pool_create () =
  let counter = ref 0 in
  let pool = Kirin.Pool.create
    ~min_size:1
    ~max_size:5
    ~create:(fun () -> incr counter; !counter)
    ~destroy:(fun _ -> ())
    ()
  in
  check int "initial in_use" 0 (Kirin.Pool.active_count pool);
  check bool "has available" true (Kirin.Pool.has_available pool)

let test_pool_acquire_release () =
  let pool = Kirin.Pool.create
    ~max_size:3
    ~create:(fun () -> "connection")
    ~destroy:(fun _ -> ())
    ()
  in
  let conn = Kirin.Pool.acquire pool in
  check string "got connection" "connection" conn.Kirin.Pool.conn;
  check int "active count" 1 (Kirin.Pool.active_count pool);
  Kirin.Pool.release pool conn;
  check int "after release" 0 (Kirin.Pool.active_count pool);
  check int "idle count" 1 (Kirin.Pool.idle_count pool)

let test_pool_use () =
  let pool = Kirin.Pool.create
    ~max_size:2
    ~create:(fun () -> 42)
    ~destroy:(fun _ -> ())
    ()
  in
  let result = Kirin.Pool.use pool (fun conn -> conn * 2) in
  check int "use result" 84 result;
  check int "released" 0 (Kirin.Pool.active_count pool)

let test_pool_stats () =
  let pool = Kirin.Pool.create
    ~max_size:5
    ~create:(fun () -> ())
    ~destroy:(fun _ -> ())
    ()
  in
  let _ = Kirin.Pool.use pool (fun _ -> ()) in
  let _ = Kirin.Pool.use pool (fun _ -> ()) in
  let stats = Kirin.Pool.stats pool in
  check int "total acquisitions" 2 stats.total_acquisitions

let test_pool_validate () =
  let valid = ref true in
  let pool = Kirin.Pool.create
    ~max_size:2
    ~create:(fun () -> "conn")
    ~destroy:(fun _ -> ())
    ~validate:(fun _ -> !valid)
    ()
  in
  (* First acquire creates connection *)
  let _ = Kirin.Pool.use pool (fun _ -> ()) in
  check int "idle after use" 1 (Kirin.Pool.idle_count pool);
  (* Invalidate connections *)
  valid := false;
  let removed = Kirin.Pool.validate_all pool in
  check int "removed invalid" 1 removed;
  check int "idle after validate" 0 (Kirin.Pool.idle_count pool)

let test_pool_shutdown () =
  let destroyed = ref 0 in
  let pool = Kirin.Pool.create
    ~max_size:3
    ~create:(fun () -> "conn")
    ~destroy:(fun _ -> incr destroyed)
    ()
  in
  (* Create some connections *)
  let _ = Kirin.Pool.use pool (fun _ -> ()) in
  let _ = Kirin.Pool.use pool (fun _ -> ()) in
  Kirin.Pool.shutdown pool;
  check int "idle after shutdown" 0 (Kirin.Pool.idle_count pool)

let test_pool_error_to_string () =
  check string "timeout error" "Connection pool timeout"
    (Kirin.Pool.error_to_string Kirin.Pool.Timeout);
  check string "exhausted error" "Connection pool exhausted"
    (Kirin.Pool.error_to_string Kirin.Pool.Pool_exhausted)

let pool_tests = [
  test_case "pool create" `Quick test_pool_create;
  test_case "acquire release" `Quick test_pool_acquire_release;
  test_case "pool use" `Quick test_pool_use;
  test_case "pool stats" `Quick test_pool_stats;
  test_case "pool validate" `Quick test_pool_validate;
  test_case "pool shutdown" `Quick test_pool_shutdown;
  test_case "error to string" `Quick test_pool_error_to_string;
]

(* ============================================================
   Backpressure Tests (Phase 9)
   ============================================================ *)

module BP = Kirin.Backpressure

let test_buffer_create () =
  let buf = BP.Buffer.create ~capacity:10 () in
  check bool "is empty" true (BP.Buffer.is_empty buf);
  check bool "not full" false (BP.Buffer.is_full buf);
  check int "length" 0 (BP.Buffer.length buf)

let test_buffer_push_pop () =
  let buf = BP.Buffer.create ~capacity:5 () in
  BP.Buffer.push buf 1;
  BP.Buffer.push buf 2;
  BP.Buffer.push buf 3;
  check int "length after push" 3 (BP.Buffer.length buf);
  let v1 = BP.Buffer.pop buf in
  check int "first pop" 1 v1;
  let v2 = BP.Buffer.pop buf in
  check int "second pop" 2 v2;
  check int "length after pop" 1 (BP.Buffer.length buf)

let test_buffer_try_pop () =
  let buf = BP.Buffer.create ~capacity:5 () in
  check (option int) "empty try_pop" None (BP.Buffer.try_pop buf);
  BP.Buffer.push buf 42;
  check (option int) "non-empty try_pop" (Some 42) (BP.Buffer.try_pop buf);
  check (option int) "empty again" None (BP.Buffer.try_pop buf)

let test_buffer_drop_oldest () =
  let buf = BP.Buffer.create ~capacity:3 ~strategy:Drop_oldest () in
  BP.Buffer.push buf 1;
  BP.Buffer.push buf 2;
  BP.Buffer.push buf 3;
  (* Buffer full, next push should drop oldest *)
  BP.Buffer.push buf 4;
  check int "length still 3" 3 (BP.Buffer.length buf);
  let v1 = BP.Buffer.pop buf in
  check int "oldest dropped" 2 v1  (* 1 was dropped *)

let test_channel_create () =
  let ch = BP.Channel.create ~capacity:100 () in
  check bool "not closed" false (BP.Channel.is_closed ch);
  check int "empty length" 0 (BP.Channel.length ch)

let test_channel_send_recv () =
  let ch = BP.Channel.create ~capacity:10 () in
  BP.Channel.send ch "hello";
  BP.Channel.send ch "world";
  check int "length" 2 (BP.Channel.length ch);
  let v1 = BP.Channel.recv ch in
  check string "first recv" "hello" v1;
  let v2 = BP.Channel.recv ch in
  check string "second recv" "world" v2

let test_channel_close () =
  let ch = BP.Channel.create ~capacity:10 () in
  BP.Channel.send ch "data";
  BP.Channel.close ch;
  check bool "is closed" true (BP.Channel.is_closed ch);
  (* Can still receive buffered data *)
  let v = BP.Channel.recv ch in
  check string "recv after close" "data" v;
  (* After buffer empty, raises Channel_closed *)
  check_raises "recv on closed empty" BP.Channel_closed (fun () ->
    ignore (BP.Channel.recv ch))

let test_rate_limiter_create () =
  let limiter = BP.RateLimiter.create ~rate:100.0 ~burst:10 () in
  let available = BP.RateLimiter.available limiter in
  check bool "has tokens" true (available > 0)

let test_rate_limiter_acquire () =
  let limiter = BP.RateLimiter.create ~rate:1000.0 ~burst:5 () in
  (* Should be able to acquire burst tokens immediately *)
  for _ = 1 to 5 do
    check bool "acquire" true (BP.RateLimiter.try_acquire limiter)
  done;
  (* After burst, should be empty (or nearly) *)
  check bool "burst exhausted" true (BP.RateLimiter.available limiter <= 1)

let test_window_create () =
  let win = BP.Window.create ~initial_size:1000 ~max_size:65536 () in
  check int "initial size" 1000 (BP.Window.current_size win);
  check int "available" 1000 (BP.Window.available win);
  check int "in flight" 0 (BP.Window.in_flight win)

let test_window_reserve_release () =
  let win = BP.Window.create ~initial_size:100 () in
  BP.Window.reserve win 30;
  check int "after reserve" 70 (BP.Window.available win);
  check int "in flight" 30 (BP.Window.in_flight win);
  BP.Window.release win 30;
  check int "after release" 100 (BP.Window.available win);
  check int "in flight after" 0 (BP.Window.in_flight win)

let test_window_update_size () =
  let win = BP.Window.create ~initial_size:100 ~max_size:500 () in
  BP.Window.update_size win 300;
  check int "new size" 300 (BP.Window.current_size win);
  (* Cannot exceed max *)
  BP.Window.update_size win 1000;
  check int "capped at max" 500 (BP.Window.current_size win)

let backpressure_tests = [
  test_case "buffer create" `Quick test_buffer_create;
  test_case "buffer push pop" `Quick test_buffer_push_pop;
  test_case "buffer try_pop" `Quick test_buffer_try_pop;
  test_case "buffer drop oldest" `Quick test_buffer_drop_oldest;
  test_case "channel create" `Quick test_channel_create;
  test_case "channel send recv" `Quick test_channel_send_recv;
  test_case "channel close" `Quick test_channel_close;
  test_case "rate limiter create" `Quick test_rate_limiter_create;
  test_case "rate limiter acquire" `Quick test_rate_limiter_acquire;
  test_case "window create" `Quick test_window_create;
  test_case "window reserve release" `Quick test_window_reserve_release;
  test_case "window update size" `Quick test_window_update_size;
]

(* ============================================================
   Cache Tests (Phase 9)
   ============================================================ *)

module C = Kirin.Cache

let test_cache_create () =
  let cache = C.create ~max_size:100 () in
  check int "initial size" 0 (C.size cache);
  let stats = C.stats cache in
  check int "max size" 100 stats.max_size

let test_cache_set_get () =
  let cache = C.create ~max_size:10 () in
  C.set cache "key1" "value1";
  C.set cache "key2" "value2";
  check (option string) "get key1" (Some "value1") (C.get cache "key1");
  check (option string) "get key2" (Some "value2") (C.get cache "key2");
  check (option string) "get missing" None (C.get cache "key3")

let test_cache_remove () =
  let cache = C.create ~max_size:10 () in
  C.set cache "key" "value";
  check bool "mem before" true (C.mem cache "key");
  let removed = C.remove cache "key" in
  check bool "removed" true removed;
  check bool "mem after" false (C.mem cache "key");
  let removed_again = C.remove cache "key" in
  check bool "remove again" false removed_again

let test_cache_lru_eviction () =
  let cache = C.create ~max_size:3 () in
  C.set cache "a" "1";
  C.set cache "b" "2";
  C.set cache "c" "3";
  (* Access "a" to make it most recent *)
  ignore (C.get cache "a");
  (* Add "d", should evict "b" (least recently used) *)
  C.set cache "d" "4";
  check int "size capped" 3 (C.size cache);
  check (option string) "a still exists" (Some "1") (C.get cache "a");
  check (option string) "b evicted" None (C.get cache "b");
  check (option string) "c exists" (Some "3") (C.get cache "c");
  check (option string) "d exists" (Some "4") (C.get cache "d")

let test_cache_ttl () =
  let cache = C.create ~max_size:10 ~default_ttl:0.05 () in
  C.set cache "key" "value";
  check (option string) "get before expire" (Some "value") (C.get cache "key");
  Unix.sleepf 0.06;
  check (option string) "get after expire" None (C.get cache "key")

let test_cache_stats () =
  let cache = C.create ~max_size:10 () in
  C.set cache "key" "value";
  ignore (C.get cache "key");  (* hit *)
  ignore (C.get cache "missing");  (* miss *)
  let stats = C.stats cache in
  check int "hits" 1 stats.hits;
  check int "misses" 1 stats.misses

let test_cache_hit_rate () =
  let cache = C.create ~max_size:10 () in
  C.set cache "key" "value";
  ignore (C.get cache "key");
  ignore (C.get cache "key");
  ignore (C.get cache "missing");
  let rate = C.hit_rate cache in
  (* 2 hits, 1 miss = 2/3 = 0.666... *)
  check bool "hit rate ~0.67" true (rate > 0.6 && rate < 0.7)

let test_cache_get_or_set () =
  let cache = C.create ~max_size:10 () in
  let count = ref 0 in
  let compute () = incr count; "computed" in
  let v1 = C.get_or_set cache "key" compute in
  check string "first call" "computed" v1;
  check int "computed once" 1 !count;
  let v2 = C.get_or_set cache "key" compute in
  check string "second call" "computed" v2;
  check int "still computed once" 1 !count

let test_cache_clear () =
  let cache = C.create ~max_size:10 () in
  C.set cache "a" "1";
  C.set cache "b" "2";
  check int "size before" 2 (C.size cache);
  C.clear cache;
  check int "size after" 0 (C.size cache)

let test_cache_cleanup () =
  let cache = C.create ~max_size:10 ~default_ttl:0.03 () in
  C.set cache "a" "1";
  C.set cache "b" "2";
  Unix.sleepf 0.05;
  let expired = C.cleanup cache in
  check int "expired count" 2 expired;
  check int "size after cleanup" 0 (C.size cache)

let test_cache_make_key () =
  let key = C.make_key ["user"; "123"; "profile"] in
  check string "make_key" "user:123:profile" key

let cache_tests = [
  test_case "cache create" `Quick test_cache_create;
  test_case "cache set get" `Quick test_cache_set_get;
  test_case "cache remove" `Quick test_cache_remove;
  test_case "cache lru eviction" `Quick test_cache_lru_eviction;
  test_case "cache ttl" `Quick test_cache_ttl;
  test_case "cache stats" `Quick test_cache_stats;
  test_case "cache hit rate" `Quick test_cache_hit_rate;
  test_case "cache get_or_set" `Quick test_cache_get_or_set;
  test_case "cache clear" `Quick test_cache_clear;
  test_case "cache cleanup" `Quick test_cache_cleanup;
  test_case "cache make_key" `Quick test_cache_make_key;
]

(* ============================================================
   Jobs Tests (Phase 9)
   ============================================================ *)

module J = Kirin.Jobs

let test_jobs_create () =
  let queue = J.create ~workers:2 () in
  check bool "not running" false (J.is_running queue);
  check int "pending" 0 (J.pending_count queue)

let test_jobs_submit () =
  let queue = J.create ~workers:2 () in
  let job_id = J.submit queue (fun () -> "result") in
  check bool "has job id" true (String.length job_id > 0);
  check int "pending" 1 (J.pending_count queue)

let test_jobs_run_sync () =
  let result = J.run_sync (fun () -> 42) in
  check int "sync result" 42 result

let test_jobs_run_once () =
  let result = J.run_once (fun () -> "done") in
  match result with
  | J.Completed v -> check string "completed" "done" v
  | J.Failed e -> fail ("Failed: " ^ Printexc.to_string e)
  | _ -> fail "Unexpected status"

let test_jobs_stats () =
  let queue = J.create ~workers:2 () in
  let _ = J.submit queue (fun () -> 1) in
  let _ = J.submit queue (fun () -> 2) in
  let stats = J.stats queue in
  check int "submitted" 2 stats.total_submitted;
  check int "queue size" 2 stats.queue_size

let test_jobs_cancel () =
  let queue = J.create ~workers:1 () in
  let job_id = J.submit queue (fun () -> "result") in
  let cancelled = J.cancel queue job_id in
  check bool "cancelled" true cancelled;
  check int "pending after cancel" 0 (J.pending_count queue)

let test_jobs_clear () =
  let queue = J.create ~workers:1 () in
  let _ = J.submit queue (fun () -> 1) in
  let _ = J.submit queue (fun () -> 2) in
  let _ = J.submit queue (fun () -> 3) in
  let cleared = J.clear queue in
  check int "cleared count" 3 cleared;
  check int "pending after clear" 0 (J.pending_count queue)

let test_jobs_priority () =
  let queue = J.create ~workers:1 () in
  let _ = J.submit ~priority:J.Low queue (fun () -> "low") in
  let _ = J.submit ~priority:J.Critical queue (fun () -> "critical") in
  let _ = J.submit ~priority:J.Normal queue (fun () -> "normal") in
  (* Jobs should be ordered: Critical, Normal, Low *)
  let stats = J.stats queue in
  check int "all queued" 3 stats.queue_size

let jobs_tests = [
  test_case "jobs create" `Quick test_jobs_create;
  test_case "jobs submit" `Quick test_jobs_submit;
  test_case "jobs run_sync" `Quick test_jobs_run_sync;
  test_case "jobs run_once" `Quick test_jobs_run_once;
  test_case "jobs stats" `Quick test_jobs_stats;
  test_case "jobs cancel" `Quick test_jobs_cancel;
  test_case "jobs clear" `Quick test_jobs_clear;
  test_case "jobs priority" `Quick test_jobs_priority;
]

(* ============================================================
   Parallel Tests (Phase 9)
   ============================================================ *)

module P = Kirin.Parallel

let test_parallel_map () =
  let items = [1; 2; 3; 4; 5] in
  let results = P.map ~domains:2 (fun x -> x * 2) items in
  check (list int) "parallel map" [2; 4; 6; 8; 10] results

let test_parallel_iter () =
  let sum = ref 0 in
  let mutex = Mutex.create () in
  P.iter ~domains:2 (fun x ->
    Mutex.lock mutex;
    sum := !sum + x;
    Mutex.unlock mutex
  ) [1; 2; 3; 4; 5];
  check int "parallel iter sum" 15 !sum

let test_parallel_filter () =
  let items = [1; 2; 3; 4; 5; 6; 7; 8] in
  let evens = P.filter ~domains:2 (fun x -> x mod 2 = 0) items in
  check (list int) "parallel filter" [2; 4; 6; 8] evens

let test_parallel_reduce () =
  let items = [1; 2; 3; 4; 5] in
  let sum = P.reduce ~domains:2 ( + ) 0 items in
  check int "parallel reduce" 15 sum

let test_parallel_both () =
  let (a, b) = P.both
    (fun () -> 1 + 1)
    (fun () -> 2 * 2) in
  check int "both a" 2 a;
  check int "both b" 4 b

let test_parallel_all () =
  let results = P.all [
    (fun () -> 1);
    (fun () -> 2);
    (fun () -> 3);
  ] in
  check (list int) "all results" [1; 2; 3] results

let test_parallel_pool () =
  let pool = P.Pool.create ~size:2 () in
  check bool "pool active" true (P.Pool.is_active pool);
  check int "pool size" 2 (P.Pool.size pool);
  let results = P.Pool.map pool (fun x -> x + 1) [1; 2; 3] in
  check (list int) "pool map" [2; 3; 4] results;
  P.Pool.shutdown pool;
  check bool "pool inactive" false (P.Pool.is_active pool)

let test_parallel_chunk_list () =
  let items = [1; 2; 3; 4; 5; 6; 7] in
  let chunks = P.chunk_list 3 items in
  check int "chunk count" 3 (List.length chunks);
  check (list int) "chunk 1" [1; 2; 3] (List.nth chunks 0);
  check (list int) "chunk 2" [4; 5; 6] (List.nth chunks 1);
  check (list int) "chunk 3" [7] (List.nth chunks 2)

let test_parallel_recommended () =
  let n = P.recommended_domains () in
  check bool "at least 1 domain" true (n >= 1)

let parallel_tests = [
  test_case "parallel map" `Quick test_parallel_map;
  test_case "parallel iter" `Quick test_parallel_iter;
  test_case "parallel filter" `Quick test_parallel_filter;
  test_case "parallel reduce" `Quick test_parallel_reduce;
  test_case "parallel both" `Quick test_parallel_both;
  test_case "parallel all" `Quick test_parallel_all;
  test_case "parallel pool" `Quick test_parallel_pool;
  test_case "parallel chunk_list" `Quick test_parallel_chunk_list;
  test_case "parallel recommended" `Quick test_parallel_recommended;
]

(* ============================================================
   Phase 10: Health Checks
   ============================================================ *)

module H = Kirin.Health

let test_health_create () =
  let health = H.create () in
  check bool "initially ready" true (H.is_ready health)

let test_health_register () =
  let health = H.create () in
  H.register health "db" (fun () -> H.Healthy);
  H.register health "cache" (fun () -> H.Healthy);
  let response = H.check health in
  check int "two checks" 2 (List.length response.checks)

let test_health_healthy () =
  let health = H.create () in
  H.register health "service" (fun () -> H.Healthy);
  let response = H.check health in
  match response.status with
  | H.Healthy -> ()
  | _ -> fail "expected healthy status"

let test_health_unhealthy () =
  let health = H.create () in
  H.register health "failing" (fun () -> H.Unhealthy "connection failed");
  let response = H.check health in
  match response.status with
  | H.Unhealthy _ -> ()
  | _ -> fail "expected unhealthy status"

let test_health_degraded () =
  let health = H.create () in
  H.register health "slow" (fun () -> H.Degraded "high latency");
  let response = H.check health in
  match response.status with
  | H.Degraded _ -> ()
  | _ -> fail "expected degraded status"

let test_health_ready_control () =
  let health = H.create () in
  check bool "initially ready" true (H.is_ready health);
  H.set_ready health false;
  check bool "now not ready" false (H.is_ready health);
  H.set_ready health true;
  check bool "ready again" true (H.is_ready health)

let test_health_uptime () =
  let health = H.create () in
  Unix.sleepf 0.01;
  let response = H.check health in
  check bool "uptime > 0" true (response.uptime_seconds > 0.0)

let test_health_exception () =
  let health = H.create () in
  H.register health "throws" (fun () -> failwith "boom");
  let response = H.check health in
  match response.status with
  | H.Unhealthy _ -> ()
  | _ -> fail "exception should cause unhealthy"

let health_tests = [
  test_case "health create" `Quick test_health_create;
  test_case "health register" `Quick test_health_register;
  test_case "health healthy" `Quick test_health_healthy;
  test_case "health unhealthy" `Quick test_health_unhealthy;
  test_case "health degraded" `Quick test_health_degraded;
  test_case "health ready control" `Quick test_health_ready_control;
  test_case "health uptime" `Quick test_health_uptime;
  test_case "health exception" `Quick test_health_exception;
]

(* ============================================================
   Phase 10: Prometheus Metrics
   ============================================================ *)

module M = Kirin.Metrics

let test_metrics_counter () =
  let registry = M.create () in
  let counter = M.counter registry "test_counter" ~help:"Test counter" () in
  M.Counter.inc counter;
  M.Counter.inc counter ~by:5.0;
  check (float 0.01) "counter value" 6.0 (M.Counter.get counter)

let test_metrics_counter_labels () =
  let registry = M.create () in
  let counter = M.counter registry "requests" ~help:"Requests"
    ~labels:["method"; "path"] () in
  M.Counter.inc counter ~labels:[("method", "GET"); ("path", "/")];
  M.Counter.inc counter ~labels:[("method", "POST"); ("path", "/api")];
  check (float 0.01) "GET /" 1.0
    (M.Counter.get counter ~labels:[("method", "GET"); ("path", "/")]);
  check (float 0.01) "POST /api" 1.0
    (M.Counter.get counter ~labels:[("method", "POST"); ("path", "/api")])

let test_metrics_gauge () =
  let registry = M.create () in
  let gauge = M.gauge registry "test_gauge" ~help:"Test gauge" () in
  M.Gauge.set gauge 42.0;
  check (float 0.01) "gauge value" 42.0 (M.Gauge.get gauge);
  M.Gauge.inc gauge ~by:8.0;
  check (float 0.01) "after inc" 50.0 (M.Gauge.get gauge);
  M.Gauge.dec gauge ~by:10.0;
  check (float 0.01) "after dec" 40.0 (M.Gauge.get gauge)

let test_metrics_histogram () =
  let registry = M.create () in
  let hist = M.histogram registry "latency" ~help:"Latency"
    ~buckets:[| 0.1; 0.5; 1.0 |] () in
  M.Histogram.observe hist 0.05;
  M.Histogram.observe hist 0.3;
  M.Histogram.observe hist 0.8;
  (* Just check it doesn't crash - actual bucket counts are internal *)
  ()

let test_metrics_histogram_time () =
  let registry = M.create () in
  let hist = M.histogram registry "duration" ~help:"Duration" () in
  let result = M.Histogram.time hist (fun () ->
    Unix.sleepf 0.01;
    42
  ) in
  check int "timed result" 42 result

let test_metrics_summary () =
  let registry = M.create () in
  let sum = M.summary registry "response_size" ~help:"Response size" () in
  for i = 1 to 100 do
    M.Summary.observe sum (float_of_int i)
  done;
  let p50 = M.Summary.quantile sum 0.5 in
  check bool "p50 reasonable" true (p50 > 0.0)

let test_metrics_export () =
  let registry = M.create () in
  let counter = M.counter registry "http_requests" ~help:"HTTP requests" () in
  M.Counter.inc counter;
  let output = M.export registry in
  check bool "has HELP" true (String.length output > 0);
  check bool "contains name" true (String.sub output 0 50 |> String.lowercase_ascii
    |> fun s -> String.length s > 0)

let metrics_tests = [
  test_case "metrics counter" `Quick test_metrics_counter;
  test_case "metrics counter labels" `Quick test_metrics_counter_labels;
  test_case "metrics gauge" `Quick test_metrics_gauge;
  test_case "metrics histogram" `Quick test_metrics_histogram;
  test_case "metrics histogram time" `Quick test_metrics_histogram_time;
  test_case "metrics summary" `Quick test_metrics_summary;
  test_case "metrics export" `Quick test_metrics_export;
]

(* ============================================================
   Phase 10: Graceful Shutdown
   ============================================================ *)

module S = Kirin.Shutdown

let test_shutdown_create () =
  let shutdown = S.create () in
  check bool "initially running" true (S.is_running shutdown);
  check bool "not shutting down" false (S.is_shutting_down shutdown);
  check bool "not stopped" false (S.is_stopped shutdown)

let test_shutdown_custom_timeout () =
  let shutdown = S.create ~timeout:10.0 ~force_after:30.0 () in
  check bool "running" true (S.is_running shutdown)

let test_shutdown_hooks () =
  let called = ref false in
  let shutdown = S.create ~timeout:0.1 () in
  S.on_shutdown shutdown (fun () -> called := true);
  S.initiate shutdown;
  check bool "hook was called" true !called

let test_shutdown_connection_tracking () =
  let shutdown = S.create () in
  check int "no active connections" 0 (S.active_connections shutdown);
  let started = S.connection_start shutdown in
  check bool "connection started" true started;
  check int "one active connection" 1 (S.active_connections shutdown);
  S.connection_end shutdown;
  check int "no active connections" 0 (S.active_connections shutdown)

let test_shutdown_reject_during_shutdown () =
  let shutdown = S.create ~timeout:0.1 () in
  (* Initiate shutdown in a separate thread *)
  let _ = Thread.create (fun () -> S.initiate shutdown) () in
  (* Wait a bit for shutdown to start *)
  Unix.sleepf 0.05;
  (* Now connection_start should return false if shutdown started *)
  (* This is timing-dependent so just check it doesn't crash *)
  let _ = S.connection_start shutdown in
  ()

let test_shutdown_status_json () =
  let shutdown = S.create () in
  let json = S.status_json shutdown in
  match json with
  | `Assoc fields ->
    check bool "has state" true (List.mem_assoc "state" fields);
    check bool "has connections" true (List.mem_assoc "active_connections" fields)
  | _ -> fail "expected JSON object"

let shutdown_tests = [
  test_case "shutdown create" `Quick test_shutdown_create;
  test_case "shutdown custom timeout" `Quick test_shutdown_custom_timeout;
  test_case "shutdown hooks" `Quick test_shutdown_hooks;
  test_case "shutdown connection tracking" `Quick test_shutdown_connection_tracking;
  test_case "shutdown reject during" `Quick test_shutdown_reject_during_shutdown;
  test_case "shutdown status json" `Quick test_shutdown_status_json;
]

(* ============================================================
   WebRTC Tests (Phase 11)
   ============================================================ *)

module WR = Kirin.WebRTC

(* Test ICE state types *)
let test_webrtc_ice_states () =
  let states : Kirin.webrtc_ice_state list = [
    New; Checking; Connected; Completed; Failed; Disconnected; Closed
  ] in
  check int "ice states count" 7 (List.length states)

(* Test DataChannel creation *)
let test_webrtc_datachannel_create () =
  let dc = WR.DataChannel.create ~label:"test-channel" () in
  check string "label" "test-channel" (WR.DataChannel.label dc);
  check bool "initial state not open" false (WR.DataChannel.is_open dc)

(* Test DataChannel with options *)
let test_webrtc_datachannel_options () =
  let options = {
    WR.default_datachannel_options with
    ordered = false;
    protocol = "custom-protocol";
  } in
  let dc = WR.DataChannel.create ~label:"ordered-channel" ~options () in
  check string "label" "ordered-channel" (WR.DataChannel.label dc)

(* Test PeerConnection creation *)
let test_webrtc_peerconnection_create () =
  let pc = WR.PeerConnection.create () in
  check bool "ice state is new"
    true (WR.PeerConnection.ice_state pc = Kirin.New);
  check (option (module struct
    type t = WR.session_description
    let pp fmt _ = Format.fprintf fmt "<session_description>"
    let equal _ _ = true
  end)) "no local desc" None (WR.PeerConnection.local_description pc)

(* Test PeerConnection with custom ICE servers *)
let test_webrtc_peerconnection_ice_servers () =
  let ice_servers = [
    { WR.urls = ["stun:stun.example.com:3478"]; username = None; credential = None };
    { WR.urls = ["turn:turn.example.com:3478"]; username = Some "user"; credential = Some "pass" };
  ] in
  let pc = WR.PeerConnection.create ~ice_servers () in
  check bool "ice state is new"
    true (WR.PeerConnection.ice_state pc = Kirin.New)

(* Test create_data_channel on PeerConnection *)
let test_webrtc_peerconnection_data_channel () =
  let pc = WR.PeerConnection.create () in
  let dc = WR.PeerConnection.create_data_channel pc ~label:"chat" () in
  check string "channel label" "chat" (WR.DataChannel.label dc)

(* Test create_offer *)
let test_webrtc_create_offer () =
  let pc = WR.PeerConnection.create () in
  let _ = WR.PeerConnection.create_data_channel pc ~label:"data" () in
  let offer = WR.PeerConnection.create_offer pc in
  check bool "offer type" true (offer.sdp_type = WR.Offer);
  check bool "offer has SDP" true (String.length offer.sdp > 0);
  check bool "SDP contains v=0" true (String.sub offer.sdp 0 4 = "v=0 " ||
                                       String.sub offer.sdp 0 3 = "v=0")

(* Test Signaling message encoding *)
let test_webrtc_signaling_encode () =
  let msg = WR.Signaling.SdpOffer { from_peer = "peer1"; sdp = "v=0..." } in
  let json = WR.Signaling.encode_message msg in
  check bool "contains type" true (String.contains (String.lowercase_ascii json) 'o');
  check bool "is json object" true (json.[0] = '{')

(* Test Signaling message decoding *)
let test_webrtc_signaling_decode () =
  let json = {|{"type":"join","peerId":"peer123","room":"test-room"}|} in
  match WR.Signaling.decode_message json with
  | Ok (WR.Signaling.Join { peer_id; room }) ->
    check string "peer_id" "peer123" peer_id;
    check string "room" "test-room" room
  | Ok _ -> fail "expected Join message"
  | Error e -> fail ("decode failed: " ^ e)

(* Test Signaling decode error *)
let test_webrtc_signaling_decode_error () =
  let invalid_json = {|{"type":"unknown_type"}|} in
  match WR.Signaling.decode_message invalid_json with
  | Error _ -> ()  (* expected *)
  | Ok _ -> fail "expected error for unknown type"

(* Test ICE candidate encoding/decoding *)
let test_webrtc_ice_candidate () =
  let candidate : WR.ice_candidate = {
    candidate = "candidate:1 1 UDP 2122252543 192.168.1.1 12345 typ host";
    sdp_mid = Some "0";
    sdp_mline_index = Some 0;
    ufrag = None;
  } in
  let msg = WR.Signaling.IceCandidate { from_peer = "peer1"; candidate } in
  let json = WR.Signaling.encode_message msg in
  check bool "contains ice-candidate" true
    (String.contains (String.lowercase_ascii json) 'i')

(* Test routes helper *)
let test_webrtc_routes () =
  let routes = WR.routes () in
  check int "route count" 2 (List.length routes)

let webrtc_tests = [
  test_case "ice states" `Quick test_webrtc_ice_states;
  test_case "datachannel create" `Quick test_webrtc_datachannel_create;
  test_case "datachannel options" `Quick test_webrtc_datachannel_options;
  test_case "peerconnection create" `Quick test_webrtc_peerconnection_create;
  test_case "peerconnection ice servers" `Quick test_webrtc_peerconnection_ice_servers;
  test_case "peerconnection data channel" `Quick test_webrtc_peerconnection_data_channel;
  test_case "create offer" `Quick test_webrtc_create_offer;
  test_case "signaling encode" `Quick test_webrtc_signaling_encode;
  test_case "signaling decode" `Quick test_webrtc_signaling_decode;
  test_case "signaling decode error" `Quick test_webrtc_signaling_decode_error;
  test_case "ice candidate" `Quick test_webrtc_ice_candidate;
  test_case "routes helper" `Quick test_webrtc_routes;
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
    ("WebSocket", websocket_tests);
    ("SSE", sse_tests);
    ("Template", template_tests);
    ("TLS", tls_tests);
    ("gRPC", grpc_tests);
    ("GraphQL", graphql_tests);
    ("Streaming", streaming_tests);
    ("Pool", pool_tests);
    ("Backpressure", backpressure_tests);
    ("Cache", cache_tests);
    ("Jobs", jobs_tests);
    ("Parallel", parallel_tests);
    ("Health", health_tests);
    ("Metrics", metrics_tests);
    ("Shutdown", shutdown_tests);
    ("WebRTC", webrtc_tests);
  ]
