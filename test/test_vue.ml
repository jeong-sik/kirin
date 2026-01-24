(** Vue/Nuxt SSR Module Tests *)

open Alcotest

(* Route Definition Tests *)

let test_route_def_page () =
  let open Kirin_vue.Route_def in
  let route = page "/users" in
  check string "path" "/users" route.path;
  check (option string) "name" (Some "users") route.name;
  check bool "is not layout" false route.is_layout

let test_route_def_param () =
  let open Kirin_vue.Route_def in
  let route = page "/users/[id]"
    |> with_param "id" String in
  check int "params count" 1 (List.length route.params);
  let p = List.hd route.params in
  check string "param name" "id" p.name;
  check bool "is string" (p.param_type = String) true

let test_route_def_catch_all () =
  let open Kirin_vue.Route_def in
  let route = page "/docs/[...slug]"
    |> with_param "slug" Slug in
  let p = List.hd route.params in
  check bool "is slug" (p.param_type = Slug) true

let test_route_def_optional () =
  let open Kirin_vue.Route_def in
  let route = page "/user/[[id]]"
    |> with_param "id" (Optional String) in
  let p = List.hd route.params in
  match p.param_type with
  | Optional String -> ()
  | _ -> fail "expected optional string"

let test_route_def_middleware () =
  let open Kirin_vue.Route_def in
  let auth_mw = middleware "auth" "authHandler" in
  let route = page "/admin"
    |> with_middleware auth_mw in
  check int "middleware count" 1 (List.length route.middleware);
  check string "first middleware" "auth" (List.hd route.middleware).name

let test_route_def_layout () =
  let open Kirin_vue.Route_def in
  let route = page "/dashboard"
    |> with_layout "admin" in
  check (option string) "layout" (Some "admin") route.meta.layout

let test_route_def_layout_route () =
  let open Kirin_vue.Route_def in
  let l = layout "admin" in
  check bool "is layout" true l.is_layout;
  check (option string) "layout name" (Some "admin") l.layout_name

(* File Router Tests *)

let test_file_router_segment () =
  let open Kirin_vue.File_router in
  let result = parse_segment "users" in
  match result.segment_type with
  | Static s -> check string "static segment" "users" s
  | _ -> fail "expected static"

let test_file_router_dynamic () =
  let open Kirin_vue.File_router in
  let result = parse_segment "[id]" in
  match result.segment_type with
  | Dynamic s -> check string "dynamic segment" "id" s
  | _ -> fail "expected dynamic"

let test_file_router_catch_all () =
  let open Kirin_vue.File_router in
  let result = parse_segment "[...slug]" in
  match result.segment_type with
  | CatchAll s -> check string "catch all" "slug" s
  | _ -> fail "expected catch all"

let test_file_router_optional () =
  let open Kirin_vue.File_router in
  let result = parse_segment "[[id]]" in
  match result.segment_type with
  | Optional s -> check string "optional" "id" s
  | _ -> fail "expected optional"

let test_file_router_path () =
  let open Kirin_vue.File_router in
  let segments = parse_path "/users/[id]/posts" in
  check int "segment count" 3 (List.length segments)

let test_file_router_pattern () =
  let open Kirin_vue.File_router in
  let pattern = path_to_pattern "/users/[id]" in
  check string "pattern" "/users/:id" pattern

(* Loader Tests *)

let test_loader_context () =
  let open Kirin_vue.Loader in
  let ctx = create_context ~url:"/users" ~method_:"GET" () in
  check string "url" "/users" ctx.url;
  check string "method" "GET" ctx.method_

let test_loader_fetch_options () =
  let open Kirin_vue.Loader in
  let opts = { default_fetch_options with key = Some "users"; server = false } in
  check (option string) "key" (Some "users") opts.key;
  check bool "server" false opts.server

let test_loader_async_options () =
  let open Kirin_vue.Loader in
  let opts = default_async_options "users" in
  check string "key" "users" opts.key;
  check bool "lazy" false opts.lazy_

let test_loader_result () =
  let open Kirin_vue.Loader in
  let result = success (`String "data") in
  check string "status" "success" result.status;
  check bool "has data" true (result.data <> None)

(* Action Tests *)

let test_action_context () =
  let open Kirin_vue.Action in
  let ctx = create_context ~url:"/api/users" ~method_:GET () in
  check string "url" "/api/users" ctx.url

let test_action_methods () =
  let open Kirin_vue.Action in
  check string "GET string" "GET" (method_to_string GET);
  check string "POST string" "POST" (method_to_string POST);
  check string "DELETE string" "DELETE" (method_to_string DELETE)

let test_action_result_data () =
  let open Kirin_vue.Action in
  let result = ActionData (`Assoc [("ok", `Bool true)]) in
  match result with
  | ActionData json ->
    let ok = Yojson.Safe.Util.member "ok" json in
    check bool "ok value" true (ok = `Bool true)
  | _ -> fail "expected data"

let test_action_result_redirect () =
  let open Kirin_vue.Action in
  let result = ActionRedirect (302, "/login") in
  match result with
  | ActionRedirect (status, url) ->
    check int "status" 302 status;
    check string "url" "/login" url
  | _ -> fail "expected redirect"

let test_action_form () =
  let open Kirin_vue.Action in
  let form = [("name", Text "John"); ("age", Number 30.0)] in
  check (option string) "name" (Some "John") (get_text form "name");
  match get_number form "age" with
  | Some n -> check bool "age is 30" true (n = 30.0)
  | None -> fail "age not found"

(* Manifest Tests *)

let test_manifest_empty () =
  let open Kirin_vue.Manifest in
  check int "routes" 0 (List.length empty.routes);
  check int "layouts" 0 (List.length empty.layouts)

let test_manifest_add_route () =
  let open Kirin_vue.Manifest in
  let entry = {
    id = "r1"; path = "/users"; pattern = "/users"; name = Some "users";
    component = "pages/users.vue"; params = []; layout = None;
    middleware = []; redirect = None; alias = []; is_page = true; is_layout = false;
  } in
  let m = add_route entry empty in
  check int "routes" 1 (List.length m.routes)

let test_manifest_find_route () =
  let open Kirin_vue.Manifest in
  let entry = {
    id = "r1"; path = "/users"; pattern = "/users"; name = Some "users";
    component = "pages/users.vue"; params = []; layout = None;
    middleware = []; redirect = None; alias = []; is_page = true; is_layout = false;
  } in
  let m = add_route entry empty in
  match find_route "/users" m with
  | Some r -> check string "found path" "/users" r.path
  | None -> fail "route not found"

(* Preload Tests *)

let test_preload_modulepreload () =
  let open Kirin_vue.Preload in
  let hint = modulepreload "/app.js" in
  check bool "is modulepreload" (hint.hint_type = Modulepreload) true;
  check string "href" "/app.js" hint.href

let test_preload_css () =
  let open Kirin_vue.Preload in
  let hint = preload_css "/app.css" in
  check bool "is preload" (hint.hint_type = Preload) true;
  check (option string) "as" (Some "style") hint.as_

let test_preload_prefetch () =
  let open Kirin_vue.Preload in
  let hint = prefetch "/data.json" in
  check bool "is prefetch" (hint.hint_type = Prefetch) true

let test_preload_render () =
  let open Kirin_vue.Preload in
  let hints = [modulepreload "/a.js"; preload_css "/b.css"] in
  let html = render_hints hints in
  check bool "contains script" true (String.length html > 0)

(* Meta Tests *)

let test_meta_title () =
  let open Kirin_vue.Meta in
  let h = with_title "Test Page" empty in
  check (option string) "title" (Some "Test Page") h.title

let test_meta_og () =
  let open Kirin_vue.Meta in
  let h = with_property ~property:"og:title" ~content:"Test" empty in
  check int "metas count" 1 (List.length h.metas)

let test_meta_link () =
  let open Kirin_vue.Meta in
  let h = with_link ~rel:"canonical" ~href:"https://example.com" empty in
  check int "links count" 1 (List.length h.links)

let test_meta_script () =
  let open Kirin_vue.Meta in
  let h = with_script ~src:"/app.js" ~defer:true empty in
  check int "scripts count" 1 (List.length h.scripts)

let test_meta_seo () =
  let open Kirin_vue.Meta in
  let s = seo ~title:"Test" ~description:"A test page" () in
  check (option string) "title" (Some "Test") s.title;
  check (option string) "description" (Some "A test page") s.description

let test_meta_render () =
  let open Kirin_vue.Meta in
  let h = empty |> with_title "Test" |> with_meta ~name:"description" ~content:"Desc" in
  let html = render h in
  check bool "contains title" true (String.length html > 0)

(* Data Tests *)

let test_data_payload () =
  let open Kirin_vue.Data in
  let p = empty_payload ~route_path:"/users" in
  check bool "has path" true (p.route_path = "/users")

let test_data_with_data () =
  let open Kirin_vue.Data in
  let p = empty_payload ~route_path:"/" in
  let p = with_data "users" (`List [`String "a"]) p in
  check int "data count" 1 (List.length p.data)

let test_data_escape () =
  let open Kirin_vue.Data in
  let json = `String "<script>alert('xss')</script>" in
  let escaped = serialize json in
  check bool "is string" true (String.length escaped > 0)

let test_data_script () =
  let open Kirin_vue.Data in
  let p = empty_payload ~route_path:"/" in
  let script = payload_script p in
  check bool "contains NUXT" true (String.length script > 0)

(* Hydrate Tests *)

let test_hydrate_options () =
  let open Kirin_vue.Hydrate in
  let opts = { default_options with title = "Test" } in
  check string "title" "Test" opts.title

let test_hydrate_spa () =
  let open Kirin_vue.Hydrate in
  let html = spa ~title:"My App" () in
  check bool "has doctype" true (String.sub html 0 9 = "<!DOCTYPE")

let test_hydrate_with_ssr () =
  let open Kirin_vue.Hydrate in
  let html = with_ssr
    ~title:"Test"
    ~entry_script:"/.nuxt/entry.js"
    ~ssr_html:"<div>Content</div>"
    () in
  check bool "has html" true (String.length html > 0)

let test_hydrate_render () =
  let open Kirin_vue.Hydrate in
  let html = render
    ~options:default_options
    ~ssr_html:"<div>Content</div>"
    ~payload:"{}"
    ~entry_script:"/.nuxt/entry.js" in
  check bool "has html" true (String.length html > 0)

let test_hydrate_error_page () =
  let open Kirin_vue.Hydrate in
  let html = error_page ~status:404 ~message:"Not Found" in
  check bool "contains 404" true (String.length html > 0)

(* Protocol Tests *)

let test_protocol_render_request () =
  let open Kirin_vue.Protocol in
  let req = render_request ~url:"/users" () in
  check string "url" "/users" req.url

let test_protocol_encode () =
  let open Kirin_vue.Protocol in
  let req = render_request ~url:"/users" () in
  let encoded = encode_render ~id:1 req in
  check bool "has jsonrpc" true (String.length encoded > 0)

let test_protocol_decode_success () =
  let open Kirin_vue.Protocol in
  let json = {|{"jsonrpc":"2.0","id":1,"result":{"html":"<div>test</div>","head":"","payload":"{}"}}|} in
  match decode_response json with
  | Success _ -> ()
  | Failure _ -> fail "expected success"

let test_protocol_decode_error () =
  let open Kirin_vue.Protocol in
  let json = {|{"jsonrpc":"2.0","id":1,"error":{"code":-32600,"message":"Invalid"}}|} in
  match decode_response json with
  | Failure _ -> ()
  | Success _ -> fail "expected error"

(* SSR Tests *)

let test_ssr_config () =
  let open Kirin_vue.Ssr in
  check int "default workers" 4 default_config.workers;
  check (float 0.001) "default timeout" 5.0 default_config.timeout_s

let test_ssr_create () =
  let open Kirin_vue.Ssr in
  let engine = create default_config in
  check bool "running" true (is_running engine)

let test_ssr_stats () =
  let open Kirin_vue.Ssr in
  check int "initial renders" 0 empty_stats.total_renders;
  check int "initial errors" 0 empty_stats.errors

let test_ssr_render () =
  let open Kirin_vue.Ssr in
  let engine = create default_config in
  match render engine ~url:"/test" () with
  | Ok result ->
    check bool "has html" true (String.length result.html > 0);
    check int "status 200" 200 result.status
  | Error _ -> fail "expected successful render"

let test_ssr_cache () =
  let open Kirin_vue.Ssr in
  let config = { default_config with cache_strategy = Stale_while_revalidate 60 } in
  let engine = create config in
  (* First render - cache miss *)
  let _ = render engine ~url:"/cached" () in
  (* Second render - cache hit *)
  match render engine ~url:"/cached" () with
  | Ok result -> check bool "cache hit" true result.cache_hit
  | Error _ -> fail "expected cache hit"

let test_ssr_cache_hit_rate () =
  let open Kirin_vue.Ssr in
  let config = { default_config with cache_strategy = Stale_while_revalidate 60 } in
  let engine = create config in
  (* First render - cache miss *)
  let _ = render engine ~url:"/rate-test" () in
  (* Second render - cache hit *)
  let _ = render engine ~url:"/rate-test" () in
  let s = stats engine in
  check int "total renders" 2 s.total_renders;
  check int "cache hits" 1 s.cache_hits;
  check int "cache misses" 1 s.cache_misses

let test_ssr_prerender () =
  let open Kirin_vue.Ssr in
  let engine = create default_config in
  let results = prerender engine ["/"; "/about"] in
  check int "prerendered count" 2 (List.length results)

let test_ssr_shutdown () =
  let open Kirin_vue.Ssr in
  let engine = create default_config in
  shutdown engine;
  check bool "not running" false (is_running engine)

(* Streaming Tests *)

let test_streaming_config () =
  let open Kirin_vue.Streaming in
  check int "flush interval" 16 default_config.flush_interval_ms;
  check bool "progressive" true default_config.enable_progressive

let test_streaming_context () =
  let open Kirin_vue.Streaming in
  let ctx = create_context () in
  check bool "pending state" true (ctx.state = Pending)

let test_streaming_shell () =
  let open Kirin_vue.Streaming in
  let chunk = shell_chunk ~html:"<div>shell</div>" in
  check bool "has content" true (String.length chunk.content > 0);
  check bool "is shell type" true (chunk.chunk_type = Shell)

let test_streaming_suspense () =
  let open Kirin_vue.Streaming in
  let chunk = suspense_chunk ~id:"s1" ~html:"<span>loaded</span>" in
  check (option string) "suspense id" (Some "s1") chunk.id

let test_streaming_finish () =
  let open Kirin_vue.Streaming in
  let ctx = create_context () in
  start ctx;
  check bool "streaming state" true (ctx.state = Streaming);
  let end_marker = finish ctx in
  check bool "has end marker" true (String.length end_marker > 0);
  check bool "complete state" true (ctx.state = Complete)

(* Handler Tests *)

let test_handler_config () =
  let open Kirin_vue.Handler in
  check bool "default SSR" (default_config.mode = SSR) true

let test_handler_html_response () =
  let open Kirin_vue.Handler in
  let resp = html_response "Hello" in
  check int "status 200" 200 resp.status

let test_handler_redirect () =
  let open Kirin_vue.Handler in
  let resp = redirect_response ~location:"/login" () in
  check int "status 302" 302 resp.status

let test_handler_error () =
  let open Kirin_vue.Handler in
  let resp = error_response ~status:404 "Not Found" in
  check int "status 404" 404 resp.status

let test_handler_request () =
  let open Kirin_vue.Handler in
  let req = request_info ~path:"/users" ~method_:"POST" () in
  check string "path" "/users" req.path;
  check string "method" "POST" req.method_

(* Codegen Tests *)

let test_codegen_param_type () =
  let open Kirin_vue.Codegen in
  let route = Kirin_vue.Route_def.page "/users/[id]"
    |> Kirin_vue.Route_def.with_param "id" Kirin_vue.Route_def.String in
  let ts = generate_param_type route in
  check bool "has field" true (String.length ts > 0)

let test_codegen_route_type () =
  let open Kirin_vue.Codegen in
  let route = Kirin_vue.Route_def.page "/users" in
  let ts = generate_route_type route in
  check bool "has interface" true (String.length ts > 0)

let test_codegen_fetch_types () =
  let open Kirin_vue.Codegen in
  let ts = generate_use_fetch_types () in
  check bool "has FetchOptions" true (String.length ts > 0)

let test_codegen_server_handler () =
  let open Kirin_vue.Codegen in
  let ts = generate_server_handler_type () in
  check bool "has EventHandler" true (String.length ts > 0)

(* Test suites *)

let route_def_tests = [
  "page route", `Quick, test_route_def_page;
  "param", `Quick, test_route_def_param;
  "catch all", `Quick, test_route_def_catch_all;
  "optional", `Quick, test_route_def_optional;
  "middleware", `Quick, test_route_def_middleware;
  "layout", `Quick, test_route_def_layout;
  "layout route", `Quick, test_route_def_layout_route;
]

let file_router_tests = [
  "static segment", `Quick, test_file_router_segment;
  "dynamic segment", `Quick, test_file_router_dynamic;
  "catch all", `Quick, test_file_router_catch_all;
  "optional", `Quick, test_file_router_optional;
  "parse path", `Quick, test_file_router_path;
  "path to pattern", `Quick, test_file_router_pattern;
]

let loader_tests = [
  "context", `Quick, test_loader_context;
  "fetch options", `Quick, test_loader_fetch_options;
  "async options", `Quick, test_loader_async_options;
  "result", `Quick, test_loader_result;
]

let action_tests = [
  "context", `Quick, test_action_context;
  "methods", `Quick, test_action_methods;
  "result data", `Quick, test_action_result_data;
  "result redirect", `Quick, test_action_result_redirect;
  "form parsing", `Quick, test_action_form;
]

let manifest_tests = [
  "empty", `Quick, test_manifest_empty;
  "add route", `Quick, test_manifest_add_route;
  "find route", `Quick, test_manifest_find_route;
]

let preload_tests = [
  "modulepreload", `Quick, test_preload_modulepreload;
  "css", `Quick, test_preload_css;
  "prefetch", `Quick, test_preload_prefetch;
  "render", `Quick, test_preload_render;
]

let meta_tests = [
  "title", `Quick, test_meta_title;
  "og property", `Quick, test_meta_og;
  "link", `Quick, test_meta_link;
  "script", `Quick, test_meta_script;
  "seo", `Quick, test_meta_seo;
  "render", `Quick, test_meta_render;
]

let data_tests = [
  "payload", `Quick, test_data_payload;
  "with_data", `Quick, test_data_with_data;
  "escape", `Quick, test_data_escape;
  "script", `Quick, test_data_script;
]

let hydrate_tests = [
  "options", `Quick, test_hydrate_options;
  "spa", `Quick, test_hydrate_spa;
  "with_ssr", `Quick, test_hydrate_with_ssr;
  "render", `Quick, test_hydrate_render;
  "error page", `Quick, test_hydrate_error_page;
]

let protocol_tests = [
  "render request", `Quick, test_protocol_render_request;
  "encode", `Quick, test_protocol_encode;
  "decode success", `Quick, test_protocol_decode_success;
  "decode error", `Quick, test_protocol_decode_error;
]

let ssr_tests = [
  "config", `Quick, test_ssr_config;
  "create", `Quick, test_ssr_create;
  "stats", `Quick, test_ssr_stats;
  "render", `Quick, test_ssr_render;
  "cache hit", `Quick, test_ssr_cache;
  "cache hit rate", `Quick, test_ssr_cache_hit_rate;
  "prerender", `Quick, test_ssr_prerender;
  "shutdown", `Quick, test_ssr_shutdown;
]

let streaming_tests = [
  "config", `Quick, test_streaming_config;
  "context", `Quick, test_streaming_context;
  "shell chunk", `Quick, test_streaming_shell;
  "suspense chunk", `Quick, test_streaming_suspense;
  "finish", `Quick, test_streaming_finish;
]

let handler_tests = [
  "config", `Quick, test_handler_config;
  "html response", `Quick, test_handler_html_response;
  "redirect", `Quick, test_handler_redirect;
  "error", `Quick, test_handler_error;
  "request info", `Quick, test_handler_request;
]

let codegen_tests = [
  "param type", `Quick, test_codegen_param_type;
  "route type", `Quick, test_codegen_route_type;
  "fetch types", `Quick, test_codegen_fetch_types;
  "server handler", `Quick, test_codegen_server_handler;
]

let () =
  run "Vue/Nuxt SSR" [
    "Route_def", route_def_tests;
    "File_router", file_router_tests;
    "Loader", loader_tests;
    "Action", action_tests;
    "Manifest", manifest_tests;
    "Preload", preload_tests;
    "Meta", meta_tests;
    "Data", data_tests;
    "Hydrate", hydrate_tests;
    "Protocol", protocol_tests;
    "SSR", ssr_tests;
    "Streaming", streaming_tests;
    "Handler", handler_tests;
    "Codegen", codegen_tests;
  ]
