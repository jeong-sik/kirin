(** Router module tests *)

open Alcotest

let body_to_string = function
  | Kirin.Response.String s -> s
  | Kirin.Response.Stream _ -> "<stream>"
  | Kirin.Response.Producer _ -> "<producer>"

let make_req ?(meth = `GET) ?(headers = []) path =
  let raw = Http.Request.make ~meth ~headers:(Http.Header.of_list headers) path in
  let body_source = Eio.Flow.string_source "" |> Eio.Buf_read.of_flow ~max_size:1024 in
  Kirin.Request.make ~raw ~body_source

(* -- parse_pattern ------------------------------------------------- *)

let test_parse_static () =
  let segs = Kirin.Router.parse_pattern "/users/list" in
  check int "segment count" 2 (List.length segs);
  check bool "first is Static" true
    (match List.nth segs 0 with Kirin.Router.Static "users" -> true | _ -> false);
  check bool "second is Static" true
    (match List.nth segs 1 with Kirin.Router.Static "list" -> true | _ -> false)

let test_parse_param () =
  let segs = Kirin.Router.parse_pattern "/users/:id" in
  check int "segment count" 2 (List.length segs);
  check bool "second is Param" true
    (match List.nth segs 1 with Kirin.Router.Param "id" -> true | _ -> false)

let test_parse_wildcard () =
  let segs = Kirin.Router.parse_pattern "/files/*" in
  check int "segment count" 2 (List.length segs);
  check bool "second is Wildcard" true
    (match List.nth segs 1 with Kirin.Router.Wildcard -> true | _ -> false)

let test_parse_root () =
  let segs = Kirin.Router.parse_pattern "/" in
  check int "root has no segments" 0 (List.length segs)

let parse_tests = [
  test_case "static segments" `Quick test_parse_static;
  test_case "param segments" `Quick test_parse_param;
  test_case "wildcard segment" `Quick test_parse_wildcard;
  test_case "root pattern" `Quick test_parse_root;
]

(* -- match_segments ------------------------------------------------ *)

let test_match_static () =
  let segs = [Kirin.Router.Static "users"; Kirin.Router.Static "list"] in
  let result = Kirin.Router.match_segments segs ["users"; "list"] in
  check bool "matches" true (Option.is_some result);
  check int "no params" 0 (List.length (Option.get result))

let test_match_param_extraction () =
  let segs = [Kirin.Router.Static "users"; Kirin.Router.Param "id"] in
  let result = Kirin.Router.match_segments segs ["users"; "42"] in
  check bool "matches" true (Option.is_some result);
  let params = Option.get result in
  check (option string) "id param" (Some "42") (List.assoc_opt "id" params)

let test_match_multi_params () =
  let segs = [Kirin.Router.Static "users"; Kirin.Router.Param "uid";
              Kirin.Router.Static "posts"; Kirin.Router.Param "pid"] in
  let result = Kirin.Router.match_segments segs ["users"; "7"; "posts"; "99"] in
  check bool "matches" true (Option.is_some result);
  let params = Option.get result in
  check (option string) "uid" (Some "7") (List.assoc_opt "uid" params);
  check (option string) "pid" (Some "99") (List.assoc_opt "pid" params)

let test_match_wildcard () =
  let segs = [Kirin.Router.Static "files"; Kirin.Router.Wildcard] in
  let result = Kirin.Router.match_segments segs ["files"; "a"; "b"; "c"] in
  check bool "wildcard matches rest" true (Option.is_some result)

let test_match_fail_extra_parts () =
  let segs = [Kirin.Router.Static "users"] in
  let result = Kirin.Router.match_segments segs ["users"; "extra"] in
  check bool "no match with extra" true (Option.is_none result)

let test_match_fail_missing_parts () =
  let segs = [Kirin.Router.Static "users"; Kirin.Router.Static "list"] in
  let result = Kirin.Router.match_segments segs ["users"] in
  check bool "no match when missing" true (Option.is_none result)

let test_match_fail_wrong_static () =
  let segs = [Kirin.Router.Static "users"] in
  let result = Kirin.Router.match_segments segs ["posts"] in
  check bool "no match wrong static" true (Option.is_none result)

let match_tests = [
  test_case "static match" `Quick test_match_static;
  test_case "param extraction" `Quick test_match_param_extraction;
  test_case "multi param extraction" `Quick test_match_multi_params;
  test_case "wildcard matches rest" `Quick test_match_wildcard;
  test_case "fail on extra path parts" `Quick test_match_fail_extra_parts;
  test_case "fail on missing parts" `Quick test_match_fail_missing_parts;
  test_case "fail on wrong static" `Quick test_match_fail_wrong_static;
]

(* -- dispatch / find_route ----------------------------------------- *)

let handler_ok _req = Kirin.Response.text "ok"

let test_dispatch_get () =
  let routes = [Kirin.Router.get "/hello" handler_ok] in
  let req = make_req "/hello" in
  let resp = Kirin.Router.dispatch routes req in
  check int "200 status" 200 (Kirin.Response.status_code resp);
  check string "body" "ok" (body_to_string (Kirin.Response.body resp))

let test_dispatch_post () =
  let routes = [Kirin.Router.post "/items" (fun _req -> Kirin.Response.text "created")] in
  let req = make_req ~meth:`POST "/items" in
  let resp = Kirin.Router.dispatch routes req in
  check string "body" "created" (body_to_string (Kirin.Response.body resp))

let test_dispatch_put () =
  let routes = [Kirin.Router.put "/items/:id" (fun req ->
    let id = match Kirin.Request.param "id" req with Some v -> v | None -> "?" in
    Kirin.Response.text ("updated:" ^ id)
  )] in
  let req = make_req ~meth:`PUT "/items/5" in
  let resp = Kirin.Router.dispatch routes req in
  check string "body" "updated:5" (body_to_string (Kirin.Response.body resp))

let test_dispatch_delete () =
  let routes = [Kirin.Router.delete "/items/:id" (fun _req -> Kirin.Response.text "deleted")] in
  let req = make_req ~meth:`DELETE "/items/3" in
  let resp = Kirin.Router.dispatch routes req in
  check string "body" "deleted" (body_to_string (Kirin.Response.body resp))

let test_dispatch_404 () =
  let routes = [Kirin.Router.get "/exists" handler_ok] in
  let req = make_req "/missing" in
  let resp = Kirin.Router.dispatch routes req in
  check int "404" 404 (Kirin.Response.status_code resp)

let test_dispatch_method_mismatch () =
  let routes = [Kirin.Router.get "/only-get" handler_ok] in
  let req = make_req ~meth:`POST "/only-get" in
  let resp = Kirin.Router.dispatch routes req in
  check int "404 on wrong method" 404 (Kirin.Response.status_code resp)

let test_dispatch_first_match_wins () =
  let routes = [
    Kirin.Router.get "/test" (fun _req -> Kirin.Response.text "first");
    Kirin.Router.get "/test" (fun _req -> Kirin.Response.text "second");
  ] in
  let req = make_req "/test" in
  let resp = Kirin.Router.dispatch routes req in
  check string "first match" "first" (body_to_string (Kirin.Response.body resp))

let test_router_function () =
  let routes = [Kirin.Router.get "/" handler_ok] in
  let handler = Kirin.Router.router routes in
  let req = make_req "/" in
  let resp = handler req in
  check int "status" 200 (Kirin.Response.status_code resp)

let dispatch_tests = [
  test_case "GET dispatch" `Quick test_dispatch_get;
  test_case "POST dispatch" `Quick test_dispatch_post;
  test_case "PUT with param" `Quick test_dispatch_put;
  test_case "DELETE dispatch" `Quick test_dispatch_delete;
  test_case "404 for unmatched" `Quick test_dispatch_404;
  test_case "404 on method mismatch" `Quick test_dispatch_method_mismatch;
  test_case "first match wins" `Quick test_dispatch_first_match_wins;
  test_case "router function" `Quick test_router_function;
]

(* -- scope --------------------------------------------------------- *)

let test_scope_prefix () =
  let inner = [Kirin.Router.get "/items" (fun _req -> Kirin.Response.text "scoped")] in
  let scoped = Kirin.Router.scope "/api" [] inner in
  let req = make_req "/api/items" in
  let resp = Kirin.Router.dispatch scoped req in
  check string "scoped body" "scoped" (body_to_string (Kirin.Response.body resp))

let test_scope_with_middleware () =
  let add_header : Kirin.Middleware.t = fun handler req ->
    let resp = handler req in
    Kirin.Response.with_header "x-scoped" "yes" resp
  in
  let inner = [Kirin.Router.get "/data" (fun _req -> Kirin.Response.text "data")] in
  let scoped = Kirin.Router.scope "/v1" [add_header] inner in
  let req = make_req "/v1/data" in
  let resp = Kirin.Router.dispatch scoped req in
  check (option string) "middleware header" (Some "yes")
    (Kirin.Response.header "x-scoped" resp)

let scope_tests = [
  test_case "scope adds prefix" `Quick test_scope_prefix;
  test_case "scope applies middleware" `Quick test_scope_with_middleware;
]

(* -- run ----------------------------------------------------------- *)

let () =
  Eio_main.run @@ fun _env ->
  Alcotest.run "Router" [
    ("parse_pattern", parse_tests);
    ("match_segments", match_tests);
    ("dispatch", dispatch_tests);
    ("scope", scope_tests);
  ]
