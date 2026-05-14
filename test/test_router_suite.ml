(** Router, Handler, and Static file tests *)

open Alcotest
open Test_helpers

(* Router Tests *)

let test_router_static_match () =
  let handler _req = Kirin.text "matched" in
  let routes = [Kirin.get "/" handler; Kirin.get "/about" handler] in
  let router = Kirin.router routes in

  let req = make_test_request "/" in
  let resp = router req in

  check string "static match body" "matched" (response_body_to_string (Kirin.Response.body resp))

let test_router_param_extraction () =
  let handler req =
    let id = Kirin.param "id" req in
    Kirin.text ("User: " ^ id)
  in
  let routes = [Kirin.get "/users/:id" handler] in
  let router = Kirin.router routes in

  let req = make_test_request "/users/123" in
  let resp = router req in

  check string "param extraction" "User: 123" (response_body_to_string (Kirin.Response.body resp))

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

  check string "multi params" "User 42, Post 99" (response_body_to_string (Kirin.Response.body resp))

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

  check string "GET method" "GET" (response_body_to_string (Kirin.Response.body get_resp));
  check string "POST method" "POST" (response_body_to_string (Kirin.Response.body post_resp))

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

(* Handler Tests *)

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

(* Static Tests *)

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

(* Symlink-escape regression tests. The textual [..] check cannot see
   filesystem-level symlinks, so [resolve_under] is the guard that
   actually keeps responses inside [dir]. These tests must not be
   deleted in a "tidy up" pass: they pin the guard against decay. *)
let with_temp_dir f =
  let stem = Filename.temp_file "kirin_static_" "" in
  Unix.unlink stem;
  Unix.mkdir stem 0o755;
  let cleanup () =
    let rec rm p =
      match (Unix.lstat p).st_kind with
      | S_DIR ->
        Array.iter (fun child -> rm (Filename.concat p child)) (Sys.readdir p);
        Unix.rmdir p
      | _ -> Unix.unlink p
    in
    try rm stem with _ -> ()
  in
  Fun.protect ~finally:cleanup (fun () -> f stem)

let write_file path content =
  let oc = open_out path in
  output_string oc content;
  close_out oc

let test_static_resolve_regular_file () =
  with_temp_dir (fun dir ->
    let path = Filename.concat dir "hello.txt" in
    write_file path "hi";
    match Kirin.Static.resolve_under ~dir "hello.txt" with
    | Some _ -> ()
    | None -> Alcotest.fail "expected regular file to resolve")

let test_static_resolve_missing_file () =
  with_temp_dir (fun dir ->
    check (option string) "missing file falls through (not 403)"
      None (Kirin.Static.resolve_under ~dir "does-not-exist.txt"))

let test_static_resolve_symlink_within_dir () =
  with_temp_dir (fun dir ->
    let real = Filename.concat dir "real.txt" in
    let link = Filename.concat dir "alias.txt" in
    write_file real "ok";
    Unix.symlink "real.txt" link;
    match Kirin.Static.resolve_under ~dir "alias.txt" with
    | Some _ -> ()
    | None -> Alcotest.fail "symlink that stays inside dir must resolve")

let test_static_resolve_symlink_escape () =
  with_temp_dir (fun dir ->
    with_temp_dir (fun outside ->
      let secret = Filename.concat outside "secret.txt" in
      write_file secret "PASSWORD";
      let link = Filename.concat dir "innocent.txt" in
      Unix.symlink secret link;
      check (option string) "symlink that escapes dir is rejected"
        None (Kirin.Static.resolve_under ~dir "innocent.txt")))

let static_tests = [
  test_case "mime type detection" `Quick test_static_mime_type;
  test_case "path safety check" `Quick test_static_safe_path;
  test_case "resolve regular file" `Quick test_static_resolve_regular_file;
  test_case "resolve missing falls through" `Quick test_static_resolve_missing_file;
  test_case "resolve symlink within dir" `Quick test_static_resolve_symlink_within_dir;
  test_case "resolve symlink escape rejected" `Quick test_static_resolve_symlink_escape;
]
