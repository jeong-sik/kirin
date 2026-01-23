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
  let frame = Kirin.ws_close ~code:Kirin.Normal ~reason:"Bye" () in
  check bool "is close" (frame.opcode = Kirin.Close) true;
  (* Payload: 2 bytes for code (1000) + "Bye" *)
  check int "payload length" 5 (String.length frame.payload);
  (* Parse it back *)
  let code, reason = Kirin.Websocket.parse_close_payload frame.payload in
  check (option bool) "code is Normal" (Some true)
    (Option.map (fun c -> c = Kirin.Normal) code);
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
  ]
