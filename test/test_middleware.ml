(** Middleware module tests *)

open Alcotest

let body_to_string = function
  | Kirin.Response.String s -> s
  | Kirin.Response.Stream _ -> "<stream>"
  | Kirin.Response.Producer _ -> "<producer>"

let make_req ?(meth = `GET) ?(headers = []) path =
  let raw = Http.Request.make ~meth ~headers:(Http.Header.of_list headers) path in
  let body_source = Eio.Flow.string_source "" |> Eio.Buf_read.of_flow ~max_size:1024 in
  Kirin.Request.make ~raw ~body_source

let base_handler _req = Kirin.Response.text "base"

(* -- identity middleware ------------------------------------------- *)

let test_id () =
  let handler = Kirin.Middleware.apply Kirin.Middleware.id base_handler in
  let resp = handler (make_req "/") in
  check string "passthrough" "base" (body_to_string (Kirin.Response.body resp))

(* -- compose ------------------------------------------------------- *)

let test_compose_order () =
  let m1 : Kirin.Middleware.t = fun handler req ->
    let resp = handler req in
    Kirin.Response.with_header "x-m1" "1" resp
  in
  let m2 : Kirin.Middleware.t = fun handler req ->
    let resp = handler req in
    Kirin.Response.with_header "x-m2" "2" resp
  in
  let composed = Kirin.Middleware.compose m1 m2 in
  let handler = Kirin.Middleware.apply composed base_handler in
  let resp = handler (make_req "/") in
  check (option string) "m1 applied" (Some "1") (Kirin.Response.header "x-m1" resp);
  check (option string) "m2 applied" (Some "2") (Kirin.Response.header "x-m2" resp)

let test_infix_compose () =
  let m1 : Kirin.Middleware.t = fun handler req ->
    let resp = handler req in
    Kirin.Response.with_header "x-a" "a" resp
  in
  let m2 : Kirin.Middleware.t = fun handler req ->
    let resp = handler req in
    Kirin.Response.with_header "x-b" "b" resp
  in
  let composed = Kirin.Middleware.(m1 >> m2) in
  let handler = Kirin.Middleware.apply composed base_handler in
  let resp = handler (make_req "/") in
  check (option string) "a" (Some "a") (Kirin.Response.header "x-a" resp);
  check (option string) "b" (Some "b") (Kirin.Response.header "x-b" resp)

(* -- pipeline ------------------------------------------------------ *)

let test_pipeline_empty () =
  let handler = Kirin.Middleware.apply (Kirin.Middleware.pipeline []) base_handler in
  let resp = handler (make_req "/") in
  check string "empty pipeline" "base" (body_to_string (Kirin.Response.body resp))

let test_pipeline_multiple () =
  let append_header name value : Kirin.Middleware.t = fun handler req ->
    let resp = handler req in
    Kirin.Response.with_header name value resp
  in
  let pipe = Kirin.Middleware.pipeline [
    append_header "x-first" "1";
    append_header "x-second" "2";
    append_header "x-third" "3";
  ] in
  let handler = Kirin.Middleware.apply pipe base_handler in
  let resp = handler (make_req "/") in
  check (option string) "first" (Some "1") (Kirin.Response.header "x-first" resp);
  check (option string) "second" (Some "2") (Kirin.Response.header "x-second" resp);
  check (option string) "third" (Some "3") (Kirin.Response.header "x-third" resp)

(* -- short circuit (early return) ---------------------------------- *)

let test_short_circuit () =
  let blocker : Kirin.Middleware.t = fun _handler _req ->
    Kirin.Response.text ~status:`Forbidden "blocked"
  in
  let handler = Kirin.Middleware.apply blocker base_handler in
  let resp = handler (make_req "/") in
  check int "403 status" 403 (Kirin.Response.status_code resp);
  check string "blocked body" "blocked" (body_to_string (Kirin.Response.body resp))

(* -- catch --------------------------------------------------------- *)

let test_catch_handles_exception () =
  let failing_handler _req = failwith "boom" in
  let error_handler _exn _req = Kirin.Response.text ~status:`Internal_server_error "caught" in
  let handler = Kirin.Middleware.apply (Kirin.Middleware.catch error_handler) failing_handler in
  let resp = handler (make_req "/") in
  check int "500 status" 500 (Kirin.Response.status_code resp);
  check string "caught body" "caught" (body_to_string (Kirin.Response.body resp))

let test_catch_default () =
  let failing_handler _req = failwith "unexpected" in
  let handler = Kirin.Middleware.apply Kirin.Middleware.catch_default failing_handler in
  let resp = handler (make_req "/") in
  check int "500 status" 500 (Kirin.Response.status_code resp)

let test_catch_passthrough_on_success () =
  let handler = Kirin.Middleware.apply Kirin.Middleware.catch_default base_handler in
  let resp = handler (make_req "/") in
  check string "normal response" "base" (body_to_string (Kirin.Response.body resp))

(* -- with_headers -------------------------------------------------- *)

let test_with_headers () =
  let mw = Kirin.Middleware.with_headers [("x-custom", "val"); ("x-other", "o")] in
  let handler = Kirin.Middleware.apply mw base_handler in
  let resp = handler (make_req "/") in
  check (option string) "custom" (Some "val") (Kirin.Response.header "x-custom" resp);
  check (option string) "other" (Some "o") (Kirin.Response.header "x-other" resp)

(* -- timing -------------------------------------------------------- *)

let test_timing_adds_header () =
  let handler = Kirin.Middleware.apply Kirin.Middleware.timing base_handler in
  let resp = handler (make_req "/") in
  let h = Kirin.Response.header "X-Response-Time" resp in
  check bool "has timing header" true (Option.is_some h)

(* -- cors ---------------------------------------------------------- *)

let test_cors_default_preflight () =
  let handler = Kirin.Middleware.apply (Kirin.Middleware.cors ()) base_handler in
  let req = make_req ~meth:`OPTIONS ~headers:[("origin", "http://example.com")] "/" in
  let resp = handler req in
  check int "204 for preflight" 204 (Kirin.Response.status_code resp);
  check (option string) "allow origin" (Some "http://example.com")
    (Kirin.Response.header "Access-Control-Allow-Origin" resp)

let test_cors_normal_request () =
  let handler = Kirin.Middleware.apply (Kirin.Middleware.cors ()) base_handler in
  let req = make_req ~headers:[("origin", "http://example.com")] "/" in
  let resp = handler req in
  check int "200 for normal" 200 (Kirin.Response.status_code resp);
  check (option string) "allow origin" (Some "http://example.com")
    (Kirin.Response.header "Access-Control-Allow-Origin" resp)

let test_cors_restricted_origins () =
  let config = { Kirin.Middleware.default_cors_config with origins = ["http://allowed.com"] } in
  let handler = Kirin.Middleware.apply (Kirin.Middleware.cors ~config ()) base_handler in
  let req_allowed = make_req ~headers:[("origin", "http://allowed.com")] "/" in
  let resp_allowed = handler req_allowed in
  check (option string) "allowed origin" (Some "http://allowed.com")
    (Kirin.Response.header "Access-Control-Allow-Origin" resp_allowed);
  let req_denied = make_req ~headers:[("origin", "http://denied.com")] "/" in
  let resp_denied = handler req_denied in
  check (option string) "denied origin" (Some "")
    (Kirin.Response.header "Access-Control-Allow-Origin" resp_denied)

(* -- run ----------------------------------------------------------- *)

let () =
  Eio_main.run @@ fun _env ->
  Alcotest.run "Middleware" [
    ("identity", [test_case "id passthrough" `Quick test_id]);
    ("compose", [
      test_case "compose order" `Quick test_compose_order;
      test_case "infix compose" `Quick test_infix_compose;
    ]);
    ("pipeline", [
      test_case "empty pipeline" `Quick test_pipeline_empty;
      test_case "multiple middlewares" `Quick test_pipeline_multiple;
    ]);
    ("short-circuit", [test_case "early return" `Quick test_short_circuit]);
    ("catch", [
      test_case "catches exception" `Quick test_catch_handles_exception;
      test_case "catch_default" `Quick test_catch_default;
      test_case "passthrough on success" `Quick test_catch_passthrough_on_success;
    ]);
    ("with_headers", [test_case "adds headers" `Quick test_with_headers]);
    ("timing", [test_case "adds timing header" `Quick test_timing_adds_header]);
    ("cors", [
      test_case "preflight response" `Quick test_cors_default_preflight;
      test_case "normal request cors" `Quick test_cors_normal_request;
      test_case "restricted origins" `Quick test_cors_restricted_origins;
    ]);
  ]
