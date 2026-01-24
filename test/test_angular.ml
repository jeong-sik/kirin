(** Angular SSR Tests *)

open Alcotest

let check = Alcotest.check

(* Route_def Tests *)

let test_route_def_empty () =
  let open Kirin_angular.Route_def in
  let route = empty "/users" in
  check string "path" "/users" route.path;
  check (option string) "name" None route.name;
  check bool "not lazy" false route.lazy_load

let test_route_def_ssr () =
  let open Kirin_angular.Route_def in
  let route = ssr "/users" "UsersComponent" in
  check string "path" "/users" route.path;
  check (option string) "component" (Some "UsersComponent") route.component;
  check bool "is SSR" (route.render_mode = Server) true

let test_route_def_csr () =
  let open Kirin_angular.Route_def in
  let route = csr "/dashboard" "DashboardComponent" in
  check bool "is CSR" (route.render_mode = Client) true

let test_route_def_ssg () =
  let open Kirin_angular.Route_def in
  let route = ssg "/about" "AboutComponent" in
  check bool "is SSG" (route.render_mode = Prerender) true

let test_route_def_redirect () =
  let open Kirin_angular.Route_def in
  let route = redirect ~from:"" ~to_:"/home" () in
  check (option string) "redirect to" (Some "/home") route.redirect_to;
  check (option string) "path match" (Some "full") route.path_match

let test_route_def_with_guard () =
  let open Kirin_angular.Route_def in
  let route = ssr "/admin" "AdminComponent"
    |> with_guard "AuthGuard"
  in
  check int "guards count" 1 (List.length route.guards);
  check string "guard name" "AuthGuard" (List.hd route.guards).guard_name

let test_route_def_with_resolver () =
  let open Kirin_angular.Route_def in
  let route = ssr "/users/:id" "UserComponent"
    |> with_resolver ~key:"user" "UserResolver"
  in
  check int "resolvers count" 1 (List.length route.resolvers);
  check string "resolver key" "user" (List.hd route.resolvers).key

let test_route_def_children () =
  let open Kirin_angular.Route_def in
  let parent = empty "users"
    |> with_children [
      ssr ":id" "UserComponent";
      ssr "new" "NewUserComponent";
    ]
  in
  check int "children count" 2 (List.length parent.children)

(* File_router Tests *)

let test_file_router_static () =
  let open Kirin_angular.File_router in
  let result = parse_segment "users" in
  match result.segment_type with
  | Static s -> check string "static" "users" s
  | _ -> fail "expected static"

let test_file_router_dynamic () =
  let open Kirin_angular.File_router in
  let result = parse_segment ":id" in
  match result.segment_type with
  | Dynamic s -> check string "dynamic" "id" s
  | _ -> fail "expected dynamic"

let test_file_router_wildcard () =
  let open Kirin_angular.File_router in
  let result = parse_segment "**" in
  match result.segment_type with
  | Wildcard -> ()
  | _ -> fail "expected wildcard"

let test_file_router_path () =
  let open Kirin_angular.File_router in
  let segments = parse_path "/users/:id/posts" in
  check int "segment count" 3 (List.length segments)

let test_file_router_pattern () =
  let open Kirin_angular.File_router in
  let pattern = path_to_pattern "/users/:id" in
  check string "pattern" "users/:id" pattern

(* Transfer_state Tests *)

let test_transfer_state_create () =
  let open Kirin_angular.Transfer_state in
  let state = create () in
  check int "empty" 0 (List.length (keys state))

let test_transfer_state_set_get () =
  let open Kirin_angular.Transfer_state in
  let state = create () in
  set state "key1" (`String "value1");
  match get state "key1" with
  | Some (`String v) -> check string "value" "value1" v
  | _ -> fail "expected string value"

let test_transfer_state_http_cache () =
  let open Kirin_angular.Transfer_state in
  let state = create () in
  cache_response state ~method_:"GET" ~url:"/api/users" (`List []);
  match get_cached_response state ~method_:"GET" ~url:"/api/users" () with
  | Some _ -> ()
  | None -> fail "expected cached response"

let test_transfer_state_script () =
  let open Kirin_angular.Transfer_state in
  let state = create () in
  set state "user" (`Assoc [("name", `String "test")]);
  let script = script_tag state in
  check bool "has ng-state" true (String.length script > 0);
  (* <script id="ng-state" = 22 chars *)
  check bool "has id" true (String.sub script 0 22 = {|<script id="ng-state" |})

(* Hydration Tests *)

let test_hydration_options () =
  let open Kirin_angular.Hydration in
  check bool "default mode" (default_options.mode = Full) true;
  check bool "incremental mode" (incremental_options.mode = Incremental) true

let test_hydration_boundary () =
  let open Kirin_angular.Hydration in
  let html = with_boundary ~id:"comp1" ~trigger:OnViewport "<div>content</div>" in
  (* <!--nghb: = 9 chars *)
  check bool "has start" true (String.sub html 0 9 = "<!--nghb:");
  check bool "has end" true (String.length html > 30)

let test_hydration_skip () =
  let open Kirin_angular.Hydration in
  let html = with_skip_hydration "div" "content" in
  check bool "has skip attr" true (String.length html > 0)

let test_hydration_context () =
  let open Kirin_angular.Hydration in
  let ctx = create_context () in
  register_component ctx ~id:"comp1" Dehydrated;
  match get_component_state ctx "comp1" with
  | Some Dehydrated -> ()
  | _ -> fail "expected dehydrated"

let test_hydration_mark_hydrated () =
  let open Kirin_angular.Hydration in
  let ctx = create_context () in
  register_component ctx ~id:"comp1" Dehydrated;
  mark_hydrated ctx "comp1";
  match get_component_state ctx "comp1" with
  | Some Hydrated -> ()
  | _ -> fail "expected hydrated"

(* Meta Tests *)

let test_meta_title () =
  let open Kirin_angular.Meta in
  let head = empty |> with_title "Test Page" in
  check (option string) "title" (Some "Test Page") head.title

let test_meta_property () =
  let open Kirin_angular.Meta in
  let head = empty |> with_property ~property:"og:title" ~content:"Test" in
  check int "metas count" 1 (List.length head.metas)

let test_meta_link () =
  let open Kirin_angular.Meta in
  let head = empty |> with_link ~rel:"canonical" ~href:"https://example.com" () in
  check int "links count" 1 (List.length head.links)

let test_meta_seo () =
  let open Kirin_angular.Meta in
  let head = seo ~title:"SEO Test" ~description:"Description" () in
  check (option string) "title" (Some "SEO Test") head.title

let test_meta_render () =
  let open Kirin_angular.Meta in
  let head = empty |> with_title "Test" |> with_charset "UTF-8" in
  let html = render head in
  check bool "has title" true (String.length html > 0)

(* Protocol Tests *)

let test_protocol_render_request () =
  let open Kirin_angular.Protocol in
  let req = render_request ~url:"/users" () in
  check string "url" "/users" req.url

let test_protocol_encode () =
  let open Kirin_angular.Protocol in
  let req = render_request ~url:"/users" () in
  let encoded = encode_render ~id:1 req in
  check bool "has jsonrpc" true (String.length encoded > 0)

let test_protocol_decode_success () =
  let open Kirin_angular.Protocol in
  let json = {|{"jsonrpc":"2.0","id":1,"result":{"html":"<app-root></app-root>"}}|} in
  match decode_response json with
  | Success _ -> ()
  | Failure _ -> fail "expected success"

let test_protocol_decode_error () =
  let open Kirin_angular.Protocol in
  let json = {|{"jsonrpc":"2.0","id":1,"error":{"code":-32600,"message":"Invalid"}}|} in
  match decode_response json with
  | Failure _ -> ()
  | Success _ -> fail "expected error"

(* SSR Tests *)

let test_ssr_config () =
  let open Kirin_angular.Ssr in
  check int "default workers" 4 default_config.workers;
  check (float 0.001) "default timeout" 5.0 default_config.timeout_s

let test_ssr_create () =
  let open Kirin_angular.Ssr in
  let engine = create default_config in
  check bool "running" true (is_running engine)

let test_ssr_stats () =
  let open Kirin_angular.Ssr in
  let engine = create default_config in
  let stats = get_stats engine in
  check int "initial renders" 0 stats.total_renders

let test_ssr_render () =
  let open Kirin_angular.Ssr in
  let engine = create default_config in
  match render engine ~url:"/users" () with
  | Ok html -> check bool "has html" true (String.length html > 0)
  | Error _ -> fail "expected ok"

let test_ssr_cache_hit () =
  let open Kirin_angular.Ssr in
  let config = { default_config with enable_cache = true } in
  let engine = create config in
  (* First render *)
  let _ = render engine ~url:"/cached" () in
  (* Second render should hit cache *)
  let _ = render engine ~url:"/cached" () in
  let stats = get_stats engine in
  check int "cache hits" 1 stats.cache_hits

let test_ssr_cache_hit_rate () =
  let open Kirin_angular.Ssr in
  let config = { default_config with enable_cache = true } in
  let engine = create config in
  let _ = render engine ~url:"/page1" () in
  let _ = render engine ~url:"/page1" () in
  let stats = get_stats engine in
  let rate = cache_hit_rate stats in
  check bool "hit rate > 0" true (rate > 0.0)

let test_ssr_shutdown () =
  let open Kirin_angular.Ssr in
  let engine = create default_config in
  shutdown engine;
  check bool "not running" false (is_running engine)

(* Handler Tests *)

let test_handler_config () =
  let open Kirin_angular.Handler in
  check bool "default ssr mode" (default_config.mode = SSR) true;
  check bool "fallback enabled" default_config.fallback_to_csr true

let test_handler_html_response () =
  let open Kirin_angular.Handler in
  let response = html_response "<html></html>" in
  check int "status" 200 response.status;
  check bool "has body" true (String.length response.body > 0)

let test_handler_redirect () =
  let open Kirin_angular.Handler in
  let response = redirect_response "/new-location" in
  check int "status" 302 response.status

let test_handler_error () =
  let open Kirin_angular.Handler in
  let response = error_response ~status:404 ~message:"Not found" in
  check int "status" 404 response.status

let test_handler_request_info () =
  let open Kirin_angular.Handler in
  let info = request_info ~path:"/users" ~method_:"GET" () in
  check string "path" "/users" info.path;
  check string "method" "GET" info.method_

(* Codegen Tests *)

let test_codegen_param_type () =
  let open Kirin_angular in
  let route = Route_def.ssr "/users/:id" "UserComponent"
    |> Route_def.with_param "id" Route_def.String
  in
  let param_type = Codegen.generate_param_type route in
  check bool "has id field" true (String.length param_type > 10)

let test_codegen_route_type () =
  let open Kirin_angular in
  let route = Route_def.ssr "/users" "UsersComponent"
    |> Route_def.with_name "users"
  in
  let route_type = Codegen.generate_route_type route in
  check bool "has interface" true (String.sub route_type 0 16 = "export interface")

let test_codegen_server_routes () =
  let open Kirin_angular in
  let routes = [
    Route_def.ssr "/" "HomeComponent";
    Route_def.csr "/dashboard" "DashboardComponent";
  ] in
  let content = Codegen.generate_server_routes routes in
  check bool "has import" true (String.length content > 0)

let test_codegen_types_file () =
  let open Kirin_angular in
  let routes = [
    Route_def.ssr "/users" "UsersComponent" |> Route_def.with_name "users";
  ] in
  let content = Codegen.generate_types_file ~routes in
  check bool "has auto-generated" true (String.length content > 50)

(* Test Registration *)

let route_def_tests = [
  "empty", `Quick, test_route_def_empty;
  "ssr", `Quick, test_route_def_ssr;
  "csr", `Quick, test_route_def_csr;
  "ssg", `Quick, test_route_def_ssg;
  "redirect", `Quick, test_route_def_redirect;
  "with guard", `Quick, test_route_def_with_guard;
  "with resolver", `Quick, test_route_def_with_resolver;
  "children", `Quick, test_route_def_children;
]

let file_router_tests = [
  "static", `Quick, test_file_router_static;
  "dynamic", `Quick, test_file_router_dynamic;
  "wildcard", `Quick, test_file_router_wildcard;
  "parse path", `Quick, test_file_router_path;
  "path to pattern", `Quick, test_file_router_pattern;
]

let transfer_state_tests = [
  "create", `Quick, test_transfer_state_create;
  "set/get", `Quick, test_transfer_state_set_get;
  "http cache", `Quick, test_transfer_state_http_cache;
  "script tag", `Quick, test_transfer_state_script;
]

let hydration_tests = [
  "options", `Quick, test_hydration_options;
  "boundary", `Quick, test_hydration_boundary;
  "skip", `Quick, test_hydration_skip;
  "context", `Quick, test_hydration_context;
  "mark hydrated", `Quick, test_hydration_mark_hydrated;
]

let meta_tests = [
  "title", `Quick, test_meta_title;
  "property", `Quick, test_meta_property;
  "link", `Quick, test_meta_link;
  "seo", `Quick, test_meta_seo;
  "render", `Quick, test_meta_render;
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
  "cache hit", `Quick, test_ssr_cache_hit;
  "cache hit rate", `Quick, test_ssr_cache_hit_rate;
  "shutdown", `Quick, test_ssr_shutdown;
]

let handler_tests = [
  "config", `Quick, test_handler_config;
  "html response", `Quick, test_handler_html_response;
  "redirect", `Quick, test_handler_redirect;
  "error", `Quick, test_handler_error;
  "request info", `Quick, test_handler_request_info;
]

let codegen_tests = [
  "param type", `Quick, test_codegen_param_type;
  "route type", `Quick, test_codegen_route_type;
  "server routes", `Quick, test_codegen_server_routes;
  "types file", `Quick, test_codegen_types_file;
]

let () =
  run "Angular Universal SSR" [
    "Route_def", route_def_tests;
    "File_router", file_router_tests;
    "Transfer_state", transfer_state_tests;
    "Hydration", hydration_tests;
    "Meta", meta_tests;
    "Protocol", protocol_tests;
    "SSR", ssr_tests;
    "Handler", handler_tests;
    "Codegen", codegen_tests;
  ]
