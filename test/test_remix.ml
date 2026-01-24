(** Remix SSR Tests *)

open Alcotest

(** {1 Test Helpers} *)

let contains haystack needle =
  try
    let _ = Str.search_forward (Str.regexp_string needle) haystack 0 in
    true
  with Not_found -> false

let json_string = testable Yojson.Safe.pp Yojson.Safe.equal

(** {1 Loader Tests} *)

let test_loader_context () =
  let ctx : Kirin_remix.Loader.loader_context = {
    request_url = "/users/123";
    request_method = "GET";
    params = [("id", "123")];
    headers = [("Accept", "application/json")];
    cookies = [("session", "abc")];
  } in
  check string "url" "/users/123" ctx.request_url;
  check string "method" "GET" ctx.request_method;
  check (option string) "param" (Some "123") (List.assoc_opt "id" ctx.params)

let test_loader_with_params () =
  let ctx : Kirin_remix.Loader.loader_context = {
    request_url = "/users";
    request_method = "GET";
    params = [];
    headers = [];
    cookies = [];
  } in
  let ctx' = Kirin_remix.Loader.with_params [("id", "456")] ctx in
  check (option string) "param added" (Some "456") (List.assoc_opt "id" ctx'.params)

let test_loader_pure () =
  let ctx : Kirin_remix.Loader.loader_context = {
    request_url = "/"; request_method = "GET";
    params = []; headers = []; cookies = [];
  } in
  let loader = Kirin_remix.Loader.pure (`String "hello") in
  match loader ctx with
  | Kirin_remix.Loader.Data json ->
    check json_string "data" (`String "hello") json
  | _ -> fail "expected Data"

let test_loader_redirect () =
  let ctx : Kirin_remix.Loader.loader_context = {
    request_url = "/"; request_method = "GET";
    params = []; headers = []; cookies = [];
  } in
  let loader = Kirin_remix.Loader.redirect "/login" in
  match loader ctx with
  | Kirin_remix.Loader.Redirect (url, status) ->
    check string "url" "/login" url;
    check int "status" 302 status
  | _ -> fail "expected Redirect"

let test_loader_not_found () =
  let ctx : Kirin_remix.Loader.loader_context = {
    request_url = "/"; request_method = "GET";
    params = []; headers = []; cookies = [];
  } in
  let loader = Kirin_remix.Loader.not_found in
  match loader ctx with
  | Kirin_remix.Loader.NotFound -> ()
  | _ -> fail "expected NotFound"

let test_loader_error () =
  let ctx : Kirin_remix.Loader.loader_context = {
    request_url = "/"; request_method = "GET";
    params = []; headers = []; cookies = [];
  } in
  let loader = Kirin_remix.Loader.error "Something went wrong" in
  match loader ctx with
  | Kirin_remix.Loader.ServerError msg ->
    check string "error" "Something went wrong" msg
  | _ -> fail "expected ServerError"

let test_loader_map () =
  let result = Kirin_remix.Loader.Data 42 in
  match Kirin_remix.Loader.map (fun x -> x * 2) result with
  | Kirin_remix.Loader.Data n -> check int "mapped" 84 n
  | _ -> fail "expected Data"

let test_loader_parallel () =
  let ctx : Kirin_remix.Loader.loader_context = {
    request_url = "/"; request_method = "GET";
    params = []; headers = []; cookies = [];
  } in
  let loader1 = Kirin_remix.Loader.pure 1 in
  let loader2 = Kirin_remix.Loader.pure 2 in
  let combined = Kirin_remix.Loader.parallel loader1 loader2 in
  match combined ctx with
  | Kirin_remix.Loader.Data (a, b) ->
    check int "first" 1 a;
    check int "second" 2 b
  | _ -> fail "expected Data tuple"

let test_loader_optional () =
  let ctx : Kirin_remix.Loader.loader_context = {
    request_url = "/"; request_method = "GET";
    params = []; headers = []; cookies = [];
  } in
  let loader = Kirin_remix.Loader.optional Kirin_remix.Loader.not_found in
  match loader ctx with
  | Kirin_remix.Loader.Data None -> ()
  | _ -> fail "expected Data None"

let test_loader_to_json () =
  let result = Kirin_remix.Loader.to_json_result (Kirin_remix.Loader.Data (`String "test")) in
  check int "status" 200 result.status;
  check (option json_string) "data" (Some (`String "test")) result.data

let test_loader_context_json () =
  let ctx : Kirin_remix.Loader.loader_context = {
    request_url = "/users";
    request_method = "GET";
    params = [("id", "1")];
    headers = [];
    cookies = [];
  } in
  let json = Kirin_remix.Loader.context_to_json ctx in
  let str = Yojson.Safe.to_string json in
  check bool "has url" true (contains str "users");
  check bool "has method" true (contains str "GET")

(** {1 Action Tests} *)

let test_action_context () =
  let ctx : Kirin_remix.Action.action_context = {
    request_url = "/posts/new";
    request_method = "POST";
    params = [];
    headers = [];
    form_data = [("title", "Hello"); ("body", "World")];
  } in
  check string "url" "/posts/new" ctx.request_url;
  check string "method" "POST" ctx.request_method

let test_action_field () =
  let ctx : Kirin_remix.Action.action_context = {
    request_url = "/";
    request_method = "POST";
    params = [];
    headers = [];
    form_data = [("name", "John"); ("email", "john@example.com")];
  } in
  check (option string) "name" (Some "John") (Kirin_remix.Action.field ctx "name");
  check (option string) "missing" None (Kirin_remix.Action.field ctx "missing")

let test_action_required_field () =
  let ctx : Kirin_remix.Action.action_context = {
    request_url = "/";
    request_method = "POST";
    params = [];
    headers = [];
    form_data = [("name", "John"); ("empty", "")];
  } in
  check (option string) "present" (Some "John") (Kirin_remix.Action.required_field ctx "name");
  check (option string) "empty" None (Kirin_remix.Action.required_field ctx "empty")

let test_action_field_values () =
  let ctx : Kirin_remix.Action.action_context = {
    request_url = "/";
    request_method = "POST";
    params = [];
    headers = [];
    form_data = [("tags", "a"); ("tags", "b"); ("tags", "c")];
  } in
  let values = Kirin_remix.Action.field_values ctx "tags" in
  check (list string) "multi values" ["a"; "b"; "c"] values

let test_action_succeed () =
  let ctx : Kirin_remix.Action.action_context = {
    request_url = "/"; request_method = "POST";
    params = []; headers = []; form_data = [];
  } in
  let action = Kirin_remix.Action.succeed "ok" in
  match action ctx with
  | Kirin_remix.Action.Success s -> check string "success" "ok" s
  | _ -> fail "expected Success"

let test_action_redirect () =
  let ctx : Kirin_remix.Action.action_context = {
    request_url = "/"; request_method = "POST";
    params = []; headers = []; form_data = [];
  } in
  let action = Kirin_remix.Action.redirect_action "/success" in
  match action ctx with
  | Kirin_remix.Action.Redirect (url, status) ->
    check string "url" "/success" url;
    check int "status" 302 status
  | _ -> fail "expected Redirect"

let test_action_validate () =
  let ctx : Kirin_remix.Action.action_context = {
    request_url = "/"; request_method = "POST";
    params = []; headers = [];
    form_data = [("email", "test@example.com"); ("name", "")];
  } in
  let validators = [
    ("email", Kirin_remix.Action.is_email, "Invalid email");
    ("name", Kirin_remix.Action.not_empty, "Name required");
  ] in
  match Kirin_remix.Action.validate ~validators ctx with
  | Kirin_remix.Action.ValidationError errors ->
    check int "error count" 1 (List.length errors);
    check (option string) "name error" (Some "Name required") (List.assoc_opt "name" errors)
  | _ -> fail "expected ValidationError"

let test_action_validators () =
  check bool "not_empty" true (Kirin_remix.Action.not_empty "hello");
  check bool "not_empty empty" false (Kirin_remix.Action.not_empty "");
  check bool "min_length" true (Kirin_remix.Action.min_length 3 "hello");
  check bool "min_length fail" false (Kirin_remix.Action.min_length 10 "hello");
  check bool "max_length" true (Kirin_remix.Action.max_length 10 "hello");
  check bool "max_length fail" false (Kirin_remix.Action.max_length 3 "hello");
  check bool "is_email" true (Kirin_remix.Action.is_email "a@b.com");
  check bool "is_email fail" false (Kirin_remix.Action.is_email "invalid");
  check bool "is_numeric" true (Kirin_remix.Action.is_numeric "12345");
  check bool "is_numeric fail" false (Kirin_remix.Action.is_numeric "abc")

let test_action_map () =
  let result = Kirin_remix.Action.Success 10 in
  match Kirin_remix.Action.map (fun x -> x + 5) result with
  | Kirin_remix.Action.Success n -> check int "mapped" 15 n
  | _ -> fail "expected Success"

let test_action_result_json () =
  let result = Kirin_remix.Action.Success (`String "data") in
  let json = Kirin_remix.Action.result_to_json (fun x -> x) result in
  let str = Yojson.Safe.to_string json in
  check bool "ok true" true (contains str "\"ok\":true")

let test_action_validation_json () =
  let result = Kirin_remix.Action.ValidationError [("email", "Invalid")] in
  let json = Kirin_remix.Action.result_to_json (fun _ -> `Null) result in
  let str = Yojson.Safe.to_string json in
  check bool "ok false" true (contains str "\"ok\":false");
  check bool "has errors" true (contains str "errors")

let test_action_context_json () =
  let ctx : Kirin_remix.Action.action_context = {
    request_url = "/submit";
    request_method = "POST";
    params = [];
    headers = [];
    form_data = [("field", "value")];
  } in
  let json = Kirin_remix.Action.context_to_json ctx in
  let str = Yojson.Safe.to_string json in
  check bool "has url" true (contains str "submit");
  check bool "has method" true (contains str "POST")

(** {1 Route Tests} *)

let test_route_create () =
  let route = Kirin_remix.Route.create "/users/:id" in
  check string "path" "/users/:id" route.path;
  check int "segments" 2 (List.length route.segments)

let test_route_segments () =
  let route = Kirin_remix.Route.create "/posts/:slug/*" in
  match route.segments with
  | [Kirin_remix.Route.Static "posts"; Kirin_remix.Route.Dynamic "slug"; Kirin_remix.Route.Splat] -> ()
  | _ -> fail "unexpected segments"

let test_route_optional_segment () =
  let route = Kirin_remix.Route.create "/users/:id?" in
  match route.segments with
  | [Kirin_remix.Route.Static "users"; Kirin_remix.Route.Optional "id"] -> ()
  | _ -> fail "expected optional segment"

let test_route_index () =
  let route = Kirin_remix.Route.index () in
  check bool "is index" true route.index;
  check string "empty path" "" route.path

let test_route_layout () =
  let child = Kirin_remix.Route.create "/child" in
  let layout = Kirin_remix.Route.layout [child] in
  check int "children" 1 (List.length layout.children)

let test_route_with_loader () =
  let route = Kirin_remix.Route.create "/test" in
  let loader (ctx : Kirin_remix.Loader.loader_context) = Kirin_remix.Loader.Data (`String ctx.request_url) in
  let route' = Kirin_remix.Route.with_loader loader route in
  check bool "has loader" true (Option.is_some route'.loader)

let test_route_with_action () =
  let route = Kirin_remix.Route.create "/test" in
  let action _ctx = Kirin_remix.Action.Success `Null in
  let route' = Kirin_remix.Route.with_action action route in
  check bool "has action" true (Option.is_some route'.action)

let test_route_with_error_boundary () =
  let route = Kirin_remix.Route.create "/test" in
  let route' = Kirin_remix.Route.with_error_boundary route in
  check bool "error boundary" true route'.error_boundary

let test_route_match_static () =
  let route = Kirin_remix.Route.create "/users/list" in
  match Kirin_remix.Route.find_match [route] ["users"; "list"] with
  | Some m -> check string "pathname" "/users/list" m.pathname
  | None -> fail "expected match"

let test_route_match_dynamic () =
  let route = Kirin_remix.Route.create "/users/:id" in
  match Kirin_remix.Route.find_match [route] ["users"; "123"] with
  | Some m ->
    check (option string) "id param" (Some "123") (List.assoc_opt "id" m.params)
  | None -> fail "expected match"

let test_route_match_splat () =
  let route = Kirin_remix.Route.create "/files/*" in
  match Kirin_remix.Route.find_match [route] ["files"; "a"; "b"; "c.txt"] with
  | Some m ->
    check (option string) "splat" (Some "a/b/c.txt") (List.assoc_opt "*" m.params)
  | None -> fail "expected match"

let test_route_no_match () =
  let route = Kirin_remix.Route.create "/users" in
  match Kirin_remix.Route.find_match [route] ["posts"] with
  | None -> ()
  | Some _ -> fail "expected no match"

let test_route_find_matches () =
  let parent = Kirin_remix.Route.create "/users" in
  let matches = Kirin_remix.Route.find_matches [parent] ["users"] [] in
  check int "match count" 1 (List.length matches)

let test_route_generate_url () =
  let route = Kirin_remix.Route.create "/users/:id/posts/:postId" in
  let url = Kirin_remix.Route.generate_url route [("id", "1"); ("postId", "42")] in
  check string "generated url" "users/1/posts/42" url

let test_route_to_json () =
  let route = Kirin_remix.Route.create "/test" in
  let json = Kirin_remix.Route.to_json route in
  let str = Yojson.Safe.to_string json in
  check bool "has path" true (contains str "test")

let test_route_match_to_json () =
  let route = Kirin_remix.Route.create "/users/:id" in
  match Kirin_remix.Route.find_match [route] ["users"; "1"] with
  | Some m ->
    let json = Kirin_remix.Route.match_to_json m in
    let str = Yojson.Safe.to_string json in
    check bool "has pathname" true (contains str "pathname")
  | None -> fail "expected match"

(** {1 SSR Tests} *)

let test_ssr_default_config () =
  let config = Kirin_remix.Ssr.default_config in
  check string "entry client" "/build/entry.client.js" config.entry_client;
  check bool "streaming" true config.enable_streaming

let test_ssr_create_context () =
  let ctx = Kirin_remix.Ssr.create_context
    ~url:"/users"
    ~matches:[]
    ~loader_data:[("root", `Null)]
    () in
  check string "url" "/users" ctx.url;
  check int "loader data" 1 (List.length ctx.loader_data)

let test_ssr_hydration_script () =
  let script = Kirin_remix.Ssr.hydration_script
    ~loader_data:[("user", `Assoc [("name", `String "John")])]
    ~action_data:None in
  check bool "has window" true (contains script "window.__remixContext");
  check bool "has loader data" true (contains script "loaderData")

let test_ssr_entry_script () =
  let config = Kirin_remix.Ssr.default_config in
  let script = Kirin_remix.Ssr.entry_script config in
  check bool "has script tag" true (contains script "<script");
  check bool "has entry client" true (contains script "entry.client.js")

let test_ssr_meta_tags () =
  let loader_data = [
    ("root", `Assoc [
      ("meta", `List [
        `Assoc [("name", `String "description"); ("content", `String "My app")];
      ])
    ])
  ] in
  let meta = Kirin_remix.Ssr.meta_tags ~loader_data in
  check bool "has meta" true (contains meta "description");
  check bool "has content" true (contains meta "My app")

let test_ssr_link_tags () =
  let loader_data = [
    ("root", `Assoc [
      ("links", `List [
        `Assoc [("rel", `String "stylesheet"); ("href", `String "/styles.css")];
      ])
    ])
  ] in
  let links = Kirin_remix.Ssr.link_tags ~loader_data in
  check bool "has link" true (contains links "<link");
  check bool "has href" true (contains links "/styles.css")

let test_ssr_render_document () =
  let config = Kirin_remix.Ssr.default_config in
  let context = Kirin_remix.Ssr.create_context
    ~url:"/" ~matches:[] ~loader_data:[] () in
  let html = Kirin_remix.Ssr.render_document ~config ~context ~body:"<h1>Hello</h1>" in
  check bool "doctype" true (contains html "<!DOCTYPE html>");
  check bool "root div" true (contains html "<div id=\"root\">");
  check bool "body content" true (contains html "<h1>Hello</h1>");
  check bool "hydration" true (contains html "__remixContext")

let test_ssr_error_boundary () =
  let html = Kirin_remix.Ssr.render_error_boundary ~error:"Something failed" ~route:"/users" in
  check bool "has error class" true (contains html "remix-error-boundary");
  check bool "has error message" true (contains html "Something failed");
  check bool "has route" true (contains html "/users")

let test_ssr_catch_boundary () =
  let html = Kirin_remix.Ssr.render_catch_boundary ~status:404 ~message:"Page not found" in
  check bool "has catch class" true (contains html "remix-catch-boundary");
  check bool "has status" true (contains html "404");
  check bool "has message" true (contains html "Page not found")

let test_ssr_config_json () =
  let config = Kirin_remix.Ssr.default_config in
  let json = Kirin_remix.Ssr.config_to_json config in
  let str = Yojson.Safe.to_string json in
  check bool "has entry client" true (contains str "entryClient");
  check bool "has streaming" true (contains str "enableStreaming")

let test_ssr_context_json () =
  let context = Kirin_remix.Ssr.create_context
    ~url:"/test" ~matches:[] ~loader_data:[] () in
  let json = Kirin_remix.Ssr.context_to_json context in
  let str = Yojson.Safe.to_string json in
  check bool "has url" true (contains str "test");
  check bool "has matches" true (contains str "matches")

(** {1 Facade Tests} *)

let test_facade_pure () =
  let ctx : Kirin_remix.Loader.loader_context = {
    request_url = "/"; request_method = "GET";
    params = []; headers = []; cookies = [];
  } in
  let loader = Kirin_remix.pure (`String "facade") in
  match loader ctx with
  | Kirin_remix.Loader.Data json ->
    check json_string "data" (`String "facade") json
  | _ -> fail "expected Data"

let test_facade_redirect () =
  let ctx : Kirin_remix.Loader.loader_context = {
    request_url = "/"; request_method = "GET";
    params = []; headers = []; cookies = [];
  } in
  match Kirin_remix.redirect "/home" ctx with
  | Kirin_remix.Loader.Redirect (url, _) ->
    check string "url" "/home" url
  | _ -> fail "expected Redirect"

let test_facade_route () =
  let route = Kirin_remix.route "/api/users" in
  check string "path" "/api/users" route.path

let test_facade_validators () =
  check bool "not_empty" true (Kirin_remix.not_empty "test");
  check bool "is_email" true (Kirin_remix.is_email "a@b.c");
  check bool "is_numeric" true (Kirin_remix.is_numeric "123")

let test_facade_default_config () =
  let config = Kirin_remix.default_config in
  check bool "streaming enabled" true config.enable_streaming

(** {1 Test Runner} *)

let loader_tests = [
  "context", `Quick, test_loader_context;
  "with_params", `Quick, test_loader_with_params;
  "pure", `Quick, test_loader_pure;
  "redirect", `Quick, test_loader_redirect;
  "not_found", `Quick, test_loader_not_found;
  "error", `Quick, test_loader_error;
  "map", `Quick, test_loader_map;
  "parallel", `Quick, test_loader_parallel;
  "optional", `Quick, test_loader_optional;
  "to_json", `Quick, test_loader_to_json;
  "context_json", `Quick, test_loader_context_json;
]

let action_tests = [
  "context", `Quick, test_action_context;
  "field", `Quick, test_action_field;
  "required_field", `Quick, test_action_required_field;
  "field_values", `Quick, test_action_field_values;
  "succeed", `Quick, test_action_succeed;
  "redirect", `Quick, test_action_redirect;
  "validate", `Quick, test_action_validate;
  "validators", `Quick, test_action_validators;
  "map", `Quick, test_action_map;
  "result_json", `Quick, test_action_result_json;
  "validation_json", `Quick, test_action_validation_json;
  "context_json", `Quick, test_action_context_json;
]

let route_tests = [
  "create", `Quick, test_route_create;
  "segments", `Quick, test_route_segments;
  "optional_segment", `Quick, test_route_optional_segment;
  "index", `Quick, test_route_index;
  "layout", `Quick, test_route_layout;
  "with_loader", `Quick, test_route_with_loader;
  "with_action", `Quick, test_route_with_action;
  "with_error_boundary", `Quick, test_route_with_error_boundary;
  "match_static", `Quick, test_route_match_static;
  "match_dynamic", `Quick, test_route_match_dynamic;
  "match_splat", `Quick, test_route_match_splat;
  "no_match", `Quick, test_route_no_match;
  "find_matches", `Quick, test_route_find_matches;
  "generate_url", `Quick, test_route_generate_url;
  "to_json", `Quick, test_route_to_json;
  "match_to_json", `Quick, test_route_match_to_json;
]

let ssr_tests = [
  "default_config", `Quick, test_ssr_default_config;
  "create_context", `Quick, test_ssr_create_context;
  "hydration_script", `Quick, test_ssr_hydration_script;
  "entry_script", `Quick, test_ssr_entry_script;
  "meta_tags", `Quick, test_ssr_meta_tags;
  "link_tags", `Quick, test_ssr_link_tags;
  "render_document", `Quick, test_ssr_render_document;
  "error_boundary", `Quick, test_ssr_error_boundary;
  "catch_boundary", `Quick, test_ssr_catch_boundary;
  "config_json", `Quick, test_ssr_config_json;
  "context_json", `Quick, test_ssr_context_json;
]

let facade_tests = [
  "pure", `Quick, test_facade_pure;
  "redirect", `Quick, test_facade_redirect;
  "route", `Quick, test_facade_route;
  "validators", `Quick, test_facade_validators;
  "default_config", `Quick, test_facade_default_config;
]

let () =
  run "Remix" [
    "Loader", loader_tests;
    "Action", action_tests;
    "Route", route_tests;
    "SSR", ssr_tests;
    "Facade", facade_tests;
  ]
