(** Qwik SSR Tests *)

open Alcotest

let check = Alcotest.check

(* QRL Tests *)

let test_qrl_create () =
  let open Kirin_qwik.Qrl in
  let qrl = create ~chunk:"chunk.js" ~symbol:"handler_click" () in
  check string "chunk" "chunk.js" qrl.chunk;
  check string "symbol" "handler_click" qrl.symbol

let test_qrl_to_string () =
  let open Kirin_qwik.Qrl in
  let qrl = create ~chunk:"chunk.js" ~symbol:"onClick" () in
  let str = to_string qrl in
  check string "serialized" "chunk.js#onClick" str

let test_qrl_with_captures () =
  let open Kirin_qwik.Qrl in
  let qrl = with_captures ~chunk:"chunk.js" ~symbol:"fn" ["a"; "b"] in
  let str = to_string qrl in
  check string "with captures" "chunk.js#fn[a b]" str

let test_qrl_on_click () =
  let open Kirin_qwik.Qrl in
  let qrl = create ~chunk:"chunk.js" ~symbol:"click" () in
  let attr = on_click qrl in
  check bool "has on:click" true (String.sub attr 0 9 = {|on:click=|})

let test_qrl_parse () =
  let open Kirin_qwik.Qrl in
  match of_serialized "chunk.js#onClick" with
  | Some qrl ->
    check string "chunk" "chunk.js" qrl.chunk;
    check string "symbol" "onClick" qrl.symbol
  | None -> fail "expected Some"

let test_qrl_prefetch () =
  let open Kirin_qwik.Qrl in
  let qrl = create ~chunk:"chunk.js" ~symbol:"fn" () in
  let hint = prefetch_hint qrl in
  check bool "has modulepreload" true (String.length hint > 0)

(* Signal Tests *)

let test_signal_create () =
  let open Kirin_qwik.Signal in
  let sig_ = create 42 in
  check int "value" 42 (get sig_)

let test_signal_set () =
  let open Kirin_qwik.Signal in
  let sig_ = create 0 in
  set sig_ 100;
  check int "updated" 100 (get sig_)

let test_signal_update () =
  let open Kirin_qwik.Signal in
  let sig_ = create 10 in
  update sig_ (fun x -> x * 2);
  check int "doubled" 20 (get sig_)

let test_computed () =
  let open Kirin_qwik.Signal in
  let comp = computed (fun () -> 1 + 2) in
  check int "computed" 3 (get_computed comp)

let test_resource_pending () =
  let open Kirin_qwik.Signal in
  let res = resource (fun () -> ()) in
  check bool "is loading" true (is_loading res)

let test_resource_resolve () =
  let open Kirin_qwik.Signal in
  let res = resource (fun () -> ()) in
  resolve res "data";
  check bool "has value" true (has_value res);
  check string "value" "data" (get_value res)

let test_store () =
  let open Kirin_qwik.Signal in
  let store = create_store () in
  set_prop store "name" (`String "test");
  match get_prop store "name" with
  | Some (`String v) -> check string "prop" "test" v
  | _ -> fail "expected string"

(* Route_def Tests *)

let test_route_def_ssr () =
  let open Kirin_qwik.Route_def in
  let route = ssr "/users" in
  check string "path" "/users" route.path;
  check bool "is SSR" (route.render_mode = SSR) true

let test_route_def_static () =
  let open Kirin_qwik.Route_def in
  let route = static "/about" in
  check bool "is static" (route.render_mode = Static) true

let test_route_def_with_loader () =
  let open Kirin_qwik.Route_def in
  let route = ssr "/users"
    |> with_loader "getUsers"
  in
  check int "loaders count" 1 (List.length route.loaders)

let test_route_def_with_action () =
  let open Kirin_qwik.Route_def in
  let route = ssr "/users"
    |> with_action "createUser"
  in
  check int "actions count" 1 (List.length route.actions)

let test_route_def_children () =
  let open Kirin_qwik.Route_def in
  let parent = ssr "users"
    |> with_children [
      ssr "[id]";
      ssr "new";
    ]
  in
  check int "children count" 2 (List.length parent.children)

let test_route_def_extract_params () =
  let open Kirin_qwik.Route_def in
  let params = extract_params "/users/[id]/posts/[...slug]" in
  check int "params count" 2 (List.length params)

(* File_router Tests *)

let test_file_router_static () =
  let open Kirin_qwik.File_router in
  let seg = parse_segment "users" in
  match seg.segment_type with
  | Static s -> check string "static" "users" s
  | _ -> fail "expected static"

let test_file_router_dynamic () =
  let open Kirin_qwik.File_router in
  let seg = parse_segment "[id]" in
  match seg.segment_type with
  | Dynamic p -> check string "dynamic" "id" p
  | _ -> fail "expected dynamic"

let test_file_router_catchall () =
  let open Kirin_qwik.File_router in
  let seg = parse_segment "[...slug]" in
  match seg.segment_type with
  | CatchAll p -> check string "catchall" "slug" p
  | _ -> fail "expected catchall"

let test_file_router_optional () =
  let open Kirin_qwik.File_router in
  let seg = parse_segment "[[optional]]" in
  match seg.segment_type with
  | Optional p -> check string "optional" "optional" p
  | _ -> fail "expected optional"

let test_file_router_group () =
  let open Kirin_qwik.File_router in
  let seg = parse_segment "(marketing)" in
  match seg.segment_type with
  | Group g -> check string "group" "marketing" g
  | _ -> fail "expected group"

let test_file_router_pattern () =
  let open Kirin_qwik.File_router in
  let pattern = segments_to_pattern (parse_path "/users/[id]") in
  check string "pattern" "/users/:id" pattern

(* Container Tests *)

let test_container_create () =
  let open Kirin_qwik.Container in
  let container = create () in
  check bool "not paused" false (is_paused container)

let test_container_add_object () =
  let open Kirin_qwik.Container in
  let container = create () in
  let ref_ = add_object container (SString "test") in
  match get_object container ref_ with
  | Some (SString s) -> check string "value" "test" s
  | _ -> fail "expected string"

let test_container_pause () =
  let open Kirin_qwik.Container in
  let container = create () in
  pause container;
  check bool "paused" true (is_paused container)

let test_container_script () =
  let open Kirin_qwik.Container in
  let container = create () in
  let script = script_tag container in
  check bool "has qwik/json" true (String.length script > 20)

(* Loader Tests *)

let test_loader_create () =
  let open Kirin_qwik.Loader in
  let loader = data ~name:"getUser" (fun _ctx -> `Assoc []) in
  check string "name" "getUser" loader.name

let test_loader_execute () =
  let open Kirin_qwik.Loader in
  let loader = data ~name:"getData" (fun _ctx -> "data") in
  let ctx = context_of_request ~url:"/test" ~params:[] ~query:[] ~headers:[] ~cookies:[] () in
  match execute loader ctx with
  | LoaderOk v -> check string "result" "data" v
  | _ -> fail "expected ok"

let test_loader_redirect () =
  let open Kirin_qwik.Loader in
  let loader = create ~name:"redirect" (fun _ctx -> redirect "/login") in
  let ctx = context_of_request ~url:"/test" ~params:[] ~query:[] ~headers:[] ~cookies:[] () in
  match execute loader ctx with
  | LoaderRedirect url -> check string "url" "/login" url
  | _ -> fail "expected redirect"

let test_loader_error () =
  let open Kirin_qwik.Loader in
  let loader = create ~name:"error" (fun _ctx -> error ~status:500 ~message:"fail") in
  let ctx = context_of_request ~url:"/test" ~params:[] ~query:[] ~headers:[] ~cookies:[] () in
  match execute loader ctx with
  | LoaderError { status; _ } -> check int "status" 500 status
  | _ -> fail "expected error"

(* Action Tests *)

let test_action_route () =
  let open Kirin_qwik.Action in
  let action = route_action ~name:"submit" (fun _ctx -> ok "done") in
  check string "name" "submit" action.name

let test_action_execute () =
  let open Kirin_qwik.Action in
  let action = route_action ~name:"create" (fun _ctx -> ok 42) in
  let ctx = context_of_request ~url:"/test" ~params:[] ~headers:[] ~cookies:[] ~form:empty_form () in
  match execute action ctx with
  | ActionOk v -> check int "result" 42 v
  | _ -> Alcotest.fail "expected ok"

let test_action_fail () =
  let open Kirin_qwik.Action in
  let action = route_action ~name:"validate" (fun _ctx -> fail [("email", "required")]) in
  let ctx = context_of_request ~url:"/test" ~params:[] ~headers:[] ~cookies:[] ~form:empty_form () in
  match execute action ctx with
  | ActionFail { errors; _ } -> check int "errors count" 1 (List.length errors)
  | _ -> Alcotest.fail "expected fail"

let test_action_validation () =
  let open Kirin_qwik.Action in
  let form = { fields = [("email", Text "test@example.com")] } in
  match email "email" form with
  | Valid -> ()
  | Invalid _ -> Alcotest.fail "expected valid"

(* Meta Tests *)

let test_meta_title () =
  let open Kirin_qwik.Meta in
  let head = empty |> with_title "Test" in
  check (option string) "title" (Some "Test") head.title

let test_meta_seo () =
  let open Kirin_qwik.Meta in
  let head = seo ~title:"SEO Test" ~description:"Desc" () in
  check (option string) "title" (Some "SEO Test") head.title;
  check bool "has metas" true (List.length head.metas > 0)

let test_meta_render () =
  let open Kirin_qwik.Meta in
  let head = empty |> with_title "Test" |> with_meta ~name:"description" ~content:"Desc" in
  let html = render head in
  check bool "has title" true (String.length html > 0)

(* SSR Tests *)

let test_ssr_config () =
  let open Kirin_qwik.Ssr in
  check int "default workers" 4 default_config.workers;
  check (float 0.001) "default timeout" 5.0 default_config.timeout_s

let test_ssr_create () =
  let open Kirin_qwik.Ssr in
  let engine = create default_config in
  check bool "running" true (is_running engine)

let test_ssr_stats () =
  let open Kirin_qwik.Ssr in
  let engine = create default_config in
  let stats = get_stats engine in
  check int "initial renders" 0 stats.total_renders

let test_ssr_render () =
  let open Kirin_qwik.Ssr in
  let engine = create default_config in
  match render engine ~url:"/users" () with
  | Ok html -> check bool "has html" true (String.length html > 0)
  | Error _ -> fail "expected ok"

let test_ssr_cache () =
  let open Kirin_qwik.Ssr in
  let config = { default_config with enable_cache = true } in
  let engine = create config in
  let _ = render engine ~url:"/cached" () in
  let _ = render engine ~url:"/cached" () in
  let stats = get_stats engine in
  check int "cache hits" 1 stats.cache_hits

let test_ssr_shutdown () =
  let open Kirin_qwik.Ssr in
  let engine = create default_config in
  shutdown engine;
  check bool "not running" false (is_running engine)

(* Handler Tests *)

let test_handler_config () =
  let open Kirin_qwik.Handler in
  check bool "default SSR mode" (default_config.mode = SSR) true

let test_handler_html_response () =
  let open Kirin_qwik.Handler in
  let response = html_response "<html></html>" in
  check int "status" 200 response.status

let test_handler_redirect () =
  let open Kirin_qwik.Handler in
  let response = redirect_response "/new" in
  check int "status" 302 response.status

let test_handler_error () =
  let open Kirin_qwik.Handler in
  let response = error_response ~status:404 ~message:"Not found" in
  check int "status" 404 response.status

(* Codegen Tests *)

let test_codegen_param_type () =
  let open Kirin_qwik in
  let route = Route_def.ssr "/users/[id]" in
  let param_type = Codegen.generate_param_type route in
  check bool "has field" true (String.length param_type > 5)

let test_codegen_route_type () =
  let open Kirin_qwik in
  let route = Route_def.ssr "/users" |> Route_def.with_name "users" in
  let route_type = Codegen.generate_route_type route in
  check bool "has interface" true (String.sub route_type 0 16 = "export interface")

let test_codegen_routes_file () =
  let open Kirin_qwik in
  let routes = [
    Route_def.ssr "/" |> Route_def.with_name "home";
  ] in
  let content = Codegen.generate_routes_file routes in
  check bool "has routes" true (String.length content > 0)

let test_codegen_types_file () =
  let open Kirin_qwik in
  let routes = [
    Route_def.ssr "/users" |> Route_def.with_name "users";
  ] in
  let content = Codegen.generate_types_file ~routes in
  check bool "has auto-generated" true (String.length content > 50)

(* Test Registration *)

let qrl_tests = [
  "create", `Quick, test_qrl_create;
  "to_string", `Quick, test_qrl_to_string;
  "with captures", `Quick, test_qrl_with_captures;
  "on_click", `Quick, test_qrl_on_click;
  "parse", `Quick, test_qrl_parse;
  "prefetch", `Quick, test_qrl_prefetch;
]

let signal_tests = [
  "create", `Quick, test_signal_create;
  "set", `Quick, test_signal_set;
  "update", `Quick, test_signal_update;
  "computed", `Quick, test_computed;
  "resource pending", `Quick, test_resource_pending;
  "resource resolve", `Quick, test_resource_resolve;
  "store", `Quick, test_store;
]

let route_def_tests = [
  "ssr", `Quick, test_route_def_ssr;
  "static", `Quick, test_route_def_static;
  "with loader", `Quick, test_route_def_with_loader;
  "with action", `Quick, test_route_def_with_action;
  "children", `Quick, test_route_def_children;
  "extract params", `Quick, test_route_def_extract_params;
]

let file_router_tests = [
  "static", `Quick, test_file_router_static;
  "dynamic", `Quick, test_file_router_dynamic;
  "catchall", `Quick, test_file_router_catchall;
  "optional", `Quick, test_file_router_optional;
  "group", `Quick, test_file_router_group;
  "pattern", `Quick, test_file_router_pattern;
]

let container_tests = [
  "create", `Quick, test_container_create;
  "add object", `Quick, test_container_add_object;
  "pause", `Quick, test_container_pause;
  "script", `Quick, test_container_script;
]

let loader_tests = [
  "create", `Quick, test_loader_create;
  "execute", `Quick, test_loader_execute;
  "redirect", `Quick, test_loader_redirect;
  "error", `Quick, test_loader_error;
]

let action_tests = [
  "route action", `Quick, test_action_route;
  "execute", `Quick, test_action_execute;
  "fail", `Quick, test_action_fail;
  "validation", `Quick, test_action_validation;
]

let meta_tests = [
  "title", `Quick, test_meta_title;
  "seo", `Quick, test_meta_seo;
  "render", `Quick, test_meta_render;
]

let ssr_tests = [
  "config", `Quick, test_ssr_config;
  "create", `Quick, test_ssr_create;
  "stats", `Quick, test_ssr_stats;
  "render", `Quick, test_ssr_render;
  "cache", `Quick, test_ssr_cache;
  "shutdown", `Quick, test_ssr_shutdown;
]

let handler_tests = [
  "config", `Quick, test_handler_config;
  "html response", `Quick, test_handler_html_response;
  "redirect", `Quick, test_handler_redirect;
  "error", `Quick, test_handler_error;
]

let codegen_tests = [
  "param type", `Quick, test_codegen_param_type;
  "route type", `Quick, test_codegen_route_type;
  "routes file", `Quick, test_codegen_routes_file;
  "types file", `Quick, test_codegen_types_file;
]

let () =
  run "Qwik Resumable SSR" [
    "QRL", qrl_tests;
    "Signal", signal_tests;
    "Route_def", route_def_tests;
    "File_router", file_router_tests;
    "Container", container_tests;
    "Loader", loader_tests;
    "Action", action_tests;
    "Meta", meta_tests;
    "SSR", ssr_tests;
    "Handler", handler_tests;
    "Codegen", codegen_tests;
  ]
