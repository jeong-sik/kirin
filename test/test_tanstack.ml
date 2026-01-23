(** TanStack Router Tests *)

open Kirin_tanstack

(** {1 String Helper} *)

(* Helper to check if string contains substring *)
module String = struct
  include String
  let contains_s haystack needle =
    try
      let _ = Str.search_forward (Str.regexp_string needle) haystack 0 in
      true
    with Not_found -> false
end

(** {1 Test Helpers} *)

let assert_eq msg expected actual =
  if expected <> actual then
    failwith (Printf.sprintf "%s: expected %s, got %s" msg expected actual)

let assert_true msg cond =
  if not cond then failwith (Printf.sprintf "%s: expected true" msg)

let assert_false msg cond =
  if cond then failwith (Printf.sprintf "%s: expected false" msg)

let passed = ref 0
let failed = ref 0

let test name f =
  try
    f ();
    incr passed;
    Printf.printf "  ✓ %s\n%!" name
  with e ->
    incr failed;
    Printf.printf "  ✗ %s: %s\n%!" name (Printexc.to_string e)

(** {1 Route_def Tests} *)

let test_route_def () =
  Printf.printf "\nRoute_def Tests:\n";

  test "create basic route" (fun () ->
    let r = Route_def.create ~id:"home" ~path:"/" () in
    assert_eq "id" "home" r.id;
    assert_eq "path" "/" r.path
  );

  test "create route with params" (fun () ->
    let params = [Route_def.string_param "id"; Route_def.int_param "page"] in
    let r = Route_def.create ~id:"user" ~path:"/users/:id" ~params () in
    assert_eq "params count" "2" (string_of_int (List.length r.params))
  );

  test "create route with loader" (fun () ->
    let loader _ctx = Ok (`String "data") in
    let r = Route_def.with_loader ~id:"test" ~path:"/test" loader in
    assert_true "has loader" (Option.is_some r.loader)
  );

  test "create route with action" (fun () ->
    let action _ctx = Ok (`String "result") in
    let r = Route_def.with_action ~id:"test" ~path:"/test" action in
    assert_true "has action" (Option.is_some r.action)
  );

  test "string_param" (fun () ->
    let p = Route_def.string_param "name" in
    assert_eq "name" "name" p.name;
    assert_false "not optional" p.optional
  );

  test "optional param" (fun () ->
    let p = Route_def.optional (Route_def.string_param "q") in
    assert_true "optional" p.optional
  );

  test "param_type_to_string" (fun () ->
    assert_eq "string" "string" (Route_def.param_type_to_string Route_def.PString);
    assert_eq "int" "number" (Route_def.param_type_to_string Route_def.PInt)
  );

  test "method_to_string" (fun () ->
    assert_eq "GET" "GET" (Route_def.method_to_string Route_def.GET);
    assert_eq "POST" "POST" (Route_def.method_to_string Route_def.POST)
  )

(** {1 File_router Tests} *)

let test_file_router () =
  Printf.printf "\nFile_router Tests:\n";

  test "parse_segment static" (fun () ->
    let seg = File_router.parse_segment "about" in
    match seg with
    | File_router.Static s -> assert_eq "static" "about" s
    | _ -> failwith "expected Static"
  );

  test "parse_segment dynamic" (fun () ->
    let seg = File_router.parse_segment "[id]" in
    match seg with
    | File_router.Dynamic s -> assert_eq "dynamic" "id" s
    | _ -> failwith "expected Dynamic"
  );

  test "parse_segment catch-all" (fun () ->
    let seg = File_router.parse_segment "[...slug]" in
    match seg with
    | File_router.CatchAll s -> assert_eq "catch-all" "slug" s
    | _ -> failwith "expected CatchAll"
  );

  test "parse_segment optional" (fun () ->
    let seg = File_router.parse_segment "[[id]]" in
    match seg with
    | File_router.Optional s -> assert_eq "optional" "id" s
    | _ -> failwith "expected Optional"
  );

  test "parse_segment group" (fun () ->
    let seg = File_router.parse_segment "(auth)" in
    match seg with
    | File_router.Group s -> assert_eq "group" "auth" s
    | _ -> failwith "expected Group"
  );

  test "segment_to_pattern static" (fun () ->
    let p = File_router.segment_to_pattern (File_router.Static "about") in
    assert_eq "path" "about" p
  );

  test "segment_to_pattern dynamic" (fun () ->
    let p = File_router.segment_to_pattern (File_router.Dynamic "id") in
    assert_eq "path" ":id" p
  );

  test "segment_to_pattern catch-all" (fun () ->
    let p = File_router.segment_to_pattern (File_router.CatchAll "slug") in
    assert_eq "path" "*slug" p
  );

  test "segment_to_pattern optional" (fun () ->
    let p = File_router.segment_to_pattern (File_router.Optional "id") in
    assert_eq "path" ":id?" p
  );

  test "segment_to_pattern group" (fun () ->
    let p = File_router.segment_to_pattern (File_router.Group "auth") in
    assert_eq "path" "" p
  )

(** {1 Loader Tests} *)

let test_loader () =
  Printf.printf "\nLoader Tests:\n";

  test "ok creates loader_result" (fun () ->
    let r = Loader.ok "test" in
    assert_eq "data" "test" r.data;
    assert_eq "status" "200" (string_of_int r.status)
  );

  test "ok with custom status" (fun () ->
    let r = Loader.ok ~status:201 "created" in
    assert_eq "status" "201" (string_of_int r.status)
  );

  test "ok with headers" (fun () ->
    let r = Loader.ok ~headers:[("X-Custom", "value")] "data" in
    assert_eq "headers count" "1" (string_of_int (List.length r.headers))
  );

  test "redirect creates error" (fun () ->
    match Loader.redirect "/login" with
    | Error msg -> assert_true "has REDIRECT" (String.sub msg 0 9 = "REDIRECT:")
    | Ok _ -> failwith "expected Error"
  );

  test "parse_redirect valid" (fun () ->
    match Loader.parse_redirect "REDIRECT:302:/login" with
    | Some (status, url) ->
      assert_eq "status" "302" (string_of_int status);
      assert_eq "url" "/login" url
    | None -> failwith "expected Some"
  );

  test "parse_redirect invalid" (fun () ->
    match Loader.parse_redirect "NOT_FOUND" with
    | Some _ -> failwith "expected None"
    | None -> ()
  );

  test "not_found creates error" (fun () ->
    match Loader.not_found () with
    | Error msg -> assert_eq "msg" "NOT_FOUND" msg
    | Ok _ -> failwith "expected Error"
  );

  test "is_not_found true" (fun () ->
    assert_true "is_not_found" (Loader.is_not_found "NOT_FOUND")
  );

  test "unauthorized creates error" (fun () ->
    match Loader.unauthorized () with
    | Error msg -> assert_eq "msg" "UNAUTHORIZED" msg
    | Ok _ -> failwith "expected Error"
  );

  test "is_unauthorized true" (fun () ->
    assert_true "is_unauthorized" (Loader.is_unauthorized "UNAUTHORIZED")
  );

  test "param from context" (fun () ->
    let ctx : _ Route_def.loader_context = {
      ctx = ();
      params = [("id", "123")];
      search_params = [];
    } in
    match Loader.param "id" ctx with
    | Some v -> assert_eq "value" "123" v
    | None -> failwith "expected Some"
  );

  test "param missing" (fun () ->
    let ctx : _ Route_def.loader_context = {
      ctx = ();
      params = [];
      search_params = [];
    } in
    match Loader.param "id" ctx with
    | Some _ -> failwith "expected None"
    | None -> ()
  );

  test "param_int valid" (fun () ->
    let ctx : _ Route_def.loader_context = {
      ctx = ();
      params = [("page", "42")];
      search_params = [];
    } in
    match Loader.param_int "page" ctx with
    | Some n -> assert_eq "value" "42" (string_of_int n)
    | None -> failwith "expected Some"
  );

  test "search_param" (fun () ->
    let ctx : _ Route_def.loader_context = {
      ctx = ();
      params = [];
      search_params = [("q", "test")];
    } in
    match Loader.search_param "q" ctx with
    | Some v -> assert_eq "value" "test" v
    | None -> failwith "expected Some"
  )

(** {1 Action Tests} *)

let test_action () =
  Printf.printf "\nAction Tests:\n";

  test "success creates Success" (fun () ->
    match Action.success "data" with
    | Action.Success s -> assert_eq "data" "data" s
    | _ -> failwith "expected Success"
  );

  test "redirect creates Redirect" (fun () ->
    match Action.redirect ~status:303 "/home" with
    | Action.Redirect (url, status) ->
      assert_eq "url" "/home" url;
      assert_eq "status" "303" (string_of_int status)
    | _ -> failwith "expected Redirect"
  );

  test "validation_error creates ValidationError" (fun () ->
    match Action.validation_error [("email", "invalid")] with
    | Action.ValidationError errors ->
      assert_eq "count" "1" (string_of_int (List.length errors))
    | _ -> failwith "expected ValidationError"
  );

  test "server_error creates ServerError" (fun () ->
    match Action.server_error "failed" with
    | Action.ServerError msg -> assert_eq "msg" "failed" msg
    | _ -> failwith "expected ServerError"
  );

  test "param from context" (fun () ->
    let ctx : _ Route_def.action_context = {
      ctx = ();
      params = [("id", "456")];
      method_ = Route_def.POST;
      body = None;
    } in
    match Action.param "id" ctx with
    | Some v -> assert_eq "value" "456" v
    | None -> failwith "expected Some"
  );

  test "json_body" (fun () ->
    let ctx : _ Route_def.action_context = {
      ctx = ();
      params = [];
      method_ = Route_def.POST;
      body = Some (`String "test");
    } in
    match Action.json_body ctx with
    | Some (`String s) -> assert_eq "body" "test" s
    | _ -> failwith "expected Some String"
  );

  test "to_json success" (fun () ->
    let json = Action.to_json (fun s -> `String s) (Action.Success "ok") in
    match json with
    | `Assoc fields ->
      (match List.assoc_opt "success" fields with
       | Some (`Bool b) -> assert_true "success" b
       | _ -> failwith "expected success field")
    | _ -> failwith "expected Assoc"
  );

  test "status_of_result success" (fun () ->
    let status = Action.status_of_result (Action.Success "ok") in
    assert_eq "status" "200" (string_of_int status)
  );

  test "status_of_result redirect" (fun () ->
    let status = Action.status_of_result (Action.Redirect ("/", 303)) in
    assert_eq "status" "303" (string_of_int status)
  );

  test "status_of_result validation_error" (fun () ->
    let status = Action.status_of_result (Action.ValidationError []) in
    assert_eq "status" "400" (string_of_int status)
  );

  test "status_of_result server_error" (fun () ->
    let status = Action.status_of_result (Action.ServerError "fail") in
    assert_eq "status" "500" (string_of_int status)
  );

  test "for_method matches" (fun () ->
    let action ctx = Action.Success (Action.param "id" ctx |> Option.get) in
    let post_action = Action.post_only action in
    let ctx : _ Route_def.action_context = {
      ctx = ();
      params = [("id", "123")];
      method_ = Route_def.POST;
      body = None;
    } in
    match post_action ctx with
    | Action.Success v -> assert_eq "value" "123" v
    | _ -> failwith "expected Success"
  );

  test "for_method rejects" (fun () ->
    let action _ctx = Action.Success "ok" in
    let post_action = Action.post_only action in
    let ctx : _ Route_def.action_context = {
      ctx = ();
      params = [];
      method_ = Route_def.GET;
      body = None;
    } in
    match post_action ctx with
    | Action.ServerError msg -> assert_true "method error" (String.length msg > 0)
    | _ -> failwith "expected ServerError"
  )

(** {1 Manifest Tests} *)

let test_manifest () =
  Printf.printf "\nManifest Tests:\n";

  test "empty manifest" (fun () ->
    let m = Manifest.empty in
    assert_eq "routes" "0" (string_of_int (List.length m.routes))
  );

  test "add_route" (fun () ->
    let entry : Manifest.route_entry = {
      id = "home";
      path = "/";
      parent_id = None;
      index = true;
      has_loader = true;
      has_action = false;
      has_error_boundary = false;
      has_loading = false;
    } in
    let m = Manifest.add_route entry Manifest.empty in
    assert_eq "routes" "1" (string_of_int (List.length m.routes))
  );

  test "find_route" (fun () ->
    let entry : Manifest.route_entry = {
      id = "about";
      path = "/about";
      parent_id = None;
      index = false;
      has_loader = false;
      has_action = false;
      has_error_boundary = false;
      has_loading = false;
    } in
    let m = Manifest.add_route entry Manifest.empty in
    match Manifest.find_route "about" m with
    | Some r -> assert_eq "id" "about" r.id
    | None -> failwith "expected Some"
  );

  test "find_route_by_path" (fun () ->
    let entry : Manifest.route_entry = {
      id = "contact";
      path = "/contact";
      parent_id = None;
      index = false;
      has_loader = false;
      has_action = false;
      has_error_boundary = false;
      has_loading = false;
    } in
    let m = Manifest.add_route entry Manifest.empty in
    match Manifest.find_route_by_path "/contact" m with
    | Some r -> assert_eq "id" "contact" r.id
    | None -> failwith "expected Some"
  );

  test "path_matches static" (fun () ->
    match Manifest.path_matches "/about" "/about" with
    | Some params -> assert_eq "params" "0" (string_of_int (List.length params))
    | None -> failwith "expected Some"
  );

  test "path_matches dynamic" (fun () ->
    match Manifest.path_matches "/users/:id" "/users/123" with
    | Some params ->
      (match List.assoc_opt "id" params with
       | Some v -> assert_eq "id" "123" v
       | None -> failwith "expected id param")
    | None -> failwith "expected Some"
  );

  test "path_matches catch-all" (fun () ->
    match Manifest.path_matches "/docs/*" "/docs/intro/getting-started" with
    | Some params ->
      (match List.assoc_opt "*" params with
       | Some v -> assert_eq "slug" "intro/getting-started" v
       | None -> failwith "expected * param")
    | None -> failwith "expected Some"
  );

  test "path_matches no match" (fun () ->
    match Manifest.path_matches "/about" "/contact" with
    | Some _ -> failwith "expected None"
    | None -> ()
  );

  test "match_path with query" (fun () ->
    let entry : Manifest.route_entry = {
      id = "search";
      path = "/search";
      parent_id = None;
      index = false;
      has_loader = true;
      has_action = false;
      has_error_boundary = false;
      has_loading = false;
    } in
    let m = Manifest.add_route entry Manifest.empty in
    match Manifest.match_path "/search?q=test&page=1" m with
    | Some result ->
      assert_eq "route id" "search" result.route.id;
      assert_eq "search params" "2" (string_of_int (List.length result.search_params))
    | None -> failwith "expected Some"
  );

  test "to_json and of_json" (fun () ->
    let entry : Manifest.route_entry = {
      id = "test";
      path = "/test";
      parent_id = None;
      index = true;
      has_loader = true;
      has_action = true;
      has_error_boundary = false;
      has_loading = false;
    } in
    let m = Manifest.add_route entry Manifest.empty in
    let json = Manifest.to_json m in
    let m' = Manifest.of_json json in
    assert_eq "routes count" "1" (string_of_int (List.length m'.routes))
  );

  test "children_of" (fun () ->
    let parent : Manifest.route_entry = {
      id = "users";
      path = "/users";
      parent_id = None;
      index = false;
      has_loader = true;
      has_action = false;
      has_error_boundary = false;
      has_loading = false;
    } in
    let child : Manifest.route_entry = {
      id = "user";
      path = "/users/:id";
      parent_id = Some "users";
      index = false;
      has_loader = true;
      has_action = false;
      has_error_boundary = false;
      has_loading = false;
    } in
    let m = Manifest.(empty |> add_route parent |> add_route child) in
    let children = Manifest.children_of "users" m in
    assert_eq "children count" "1" (string_of_int (List.length children))
  );

  test "root_routes" (fun () ->
    let root : Manifest.route_entry = {
      id = "root";
      path = "/";
      parent_id = None;
      index = true;
      has_loader = false;
      has_action = false;
      has_error_boundary = false;
      has_loading = false;
    } in
    let child : Manifest.route_entry = {
      id = "about";
      path = "/about";
      parent_id = Some "root";
      index = false;
      has_loader = false;
      has_action = false;
      has_error_boundary = false;
      has_loading = false;
    } in
    let m = Manifest.(empty |> add_route root |> add_route child) in
    let roots = Manifest.root_routes m in
    assert_eq "root routes" "1" (string_of_int (List.length roots))
  )

(** {1 Preload Tests} *)

let test_preload () =
  Printf.printf "\nPreload Tests:\n";

  test "strategy_to_string" (fun () ->
    assert_eq "intent" "intent" (Preload.strategy_to_string Preload.Intent);
    assert_eq "viewport" "viewport" (Preload.strategy_to_string Preload.Viewport);
    assert_eq "render" "render" (Preload.strategy_to_string Preload.Render);
    assert_eq "none" "none" (Preload.strategy_to_string Preload.None)
  );

  test "link_hint" (fun () ->
    let hint = Preload.link_hint ~href:"/users" ~strategy:Preload.Intent in
    assert_true "has prefetch" (String.contains_s hint "prefetch");
    assert_true "has href" (String.contains_s hint "/users")
  );

  test "module_preload" (fun () ->
    let hint = Preload.module_preload ~src:"/app.js" in
    assert_true "has modulepreload" (String.contains_s hint "modulepreload")
  );

  test "preconnect" (fun () ->
    let hint = Preload.preconnect ~origin:"https://api.example.com" in
    assert_true "has preconnect" (String.contains_s hint "preconnect")
  );

  test "dns_prefetch" (fun () ->
    let hint = Preload.dns_prefetch ~hostname:"api.example.com" in
    assert_true "has dns-prefetch" (String.contains_s hint "dns-prefetch")
  );

  test "preload_script" (fun () ->
    let script = Preload.preload_script () in
    assert_true "has script tag" (String.contains_s script "<script>");
    assert_true "has preloadCache" (String.contains_s script "preloadCache")
  );

  test "link_attrs" (fun () ->
    let attrs = Preload.link_attrs ~strategy:Preload.Viewport ~prefetch_intent:true in
    assert_true "has data-preload" (String.contains_s attrs "data-preload");
    assert_true "has prefetch-intent" (String.contains_s attrs "data-prefetch-intent")
  );

  test "priority_to_string" (fun () ->
    assert_eq "high" "high" (Preload.priority_to_string Preload.High);
    assert_eq "low" "low" (Preload.priority_to_string Preload.Low)
  );

  test "route_hints" (fun () ->
    let hints = Preload.route_hints
      ~loader_url:"/api/users"
      ~js_modules:["/app.js"]
      ~css_files:["/styles.css"]
      ~priority:Preload.High in
    assert_true "has loader" (String.contains_s hints "/api/users");
    assert_true "has module" (String.contains_s hints "modulepreload");
    assert_true "has css" (String.contains_s hints "style")
  );

  test "is_preload_request true" (fun () ->
    let headers = [("X-Preload", "1")] in
    assert_true "is preload" (Preload.is_preload_request headers)
  );

  test "is_preload_request false" (fun () ->
    let headers = [("Accept", "application/json")] in
    assert_false "not preload" (Preload.is_preload_request headers)
  )

(** {1 Codegen Tests} *)

let test_codegen () =
  Printf.printf "\nCodegen Tests:\n";

  test "generate_params_type empty" (fun () ->
    let route : Manifest.route_entry = {
      id = "home";
      path = "/";
      parent_id = None;
      index = true;
      has_loader = false;
      has_action = false;
      has_error_boundary = false;
      has_loading = false;
    } in
    let t = Codegen.generate_params_type route in
    assert_eq "empty params" "Record<string, never>" t
  );

  test "generate_params_type with params" (fun () ->
    let route : Manifest.route_entry = {
      id = "user";
      path = "/users/:id";
      parent_id = None;
      index = false;
      has_loader = true;
      has_action = false;
      has_error_boundary = false;
      has_loading = false;
    } in
    let t = Codegen.generate_params_type route in
    assert_true "has id field" (String.contains_s t "id: string")
  );

  test "generate_route_type" (fun () ->
    let route : Manifest.route_entry = {
      id = "users";
      path = "/users";
      parent_id = None;
      index = false;
      has_loader = true;
      has_action = false;
      has_error_boundary = false;
      has_loading = false;
    } in
    let t = Codegen.generate_route_type route in
    assert_true "has interface" (String.contains_s t "interface Route_users");
    assert_true "has id" (String.contains_s t "id: 'users'")
  );

  test "generate_route_id_type" (fun () ->
    let entry1 : Manifest.route_entry = {
      id = "home"; path = "/"; parent_id = None; index = true;
      has_loader = false; has_action = false; has_error_boundary = false; has_loading = false;
    } in
    let entry2 : Manifest.route_entry = {
      id = "about"; path = "/about"; parent_id = None; index = false;
      has_loader = false; has_action = false; has_error_boundary = false; has_loading = false;
    } in
    let m = Manifest.(empty |> add_route entry1 |> add_route entry2) in
    let t = Codegen.generate_route_id_type m in
    assert_true "has RouteId" (String.contains_s t "export type RouteId");
    assert_true "has home" (String.contains_s t "'home'");
    assert_true "has about" (String.contains_s t "'about'")
  );

  test "generate_root_route" (fun () ->
    let code = Codegen.generate_root_route () in
    assert_true "has createRootRoute" (String.contains_s code "createRootRoute");
    assert_true "has rootRoute" (String.contains_s code "rootRoute")
  );

  test "generate_route_definition" (fun () ->
    let route : Manifest.route_entry = {
      id = "users";
      path = "/users";
      parent_id = None;
      index = false;
      has_loader = true;
      has_action = false;
      has_error_boundary = false;
      has_loading = false;
    } in
    let code = Codegen.generate_route_definition route in
    assert_true "has createRoute" (String.contains_s code "createRoute");
    assert_true "has path" (String.contains_s code "path: '/users'")
  );

  test "generate_types_file" (fun () ->
    let entry : Manifest.route_entry = {
      id = "home"; path = "/"; parent_id = None; index = true;
      has_loader = false; has_action = false; has_error_boundary = false; has_loading = false;
    } in
    let m = Manifest.add_route entry Manifest.empty in
    let code = Codegen.generate_types_file m in
    assert_true "has header" (String.contains_s code "Auto-generated");
    assert_true "has import" (String.contains_s code "@tanstack/react-router")
  );

  test "generate_router_file" (fun () ->
    let entry : Manifest.route_entry = {
      id = "home"; path = "/"; parent_id = None; index = true;
      has_loader = true; has_action = false; has_error_boundary = false; has_loading = false;
    } in
    let m = Manifest.add_route entry Manifest.empty in
    let code = Codegen.generate_router_file m in
    assert_true "has lazy import" (String.contains_s code "lazy");
    assert_true "has createRouter" (String.contains_s code "createRouter")
  );

  test "generate_use_params_hook" (fun () ->
    let code = Codegen.generate_use_params_hook () in
    assert_true "has useParams" (String.contains_s code "useParams")
  );

  test "generate_link_props_type" (fun () ->
    let entry : Manifest.route_entry = {
      id = "user"; path = "/users/:id"; parent_id = None; index = false;
      has_loader = true; has_action = false; has_error_boundary = false; has_loading = false;
    } in
    let m = Manifest.add_route entry Manifest.empty in
    let code = Codegen.generate_link_props_type m in
    assert_true "has LinkProps" (String.contains_s code "LinkProps");
    assert_true "has to" (String.contains_s code "to:")
  )

(** {1 Handler Tests} *)

let test_handler () =
  Printf.printf "\nHandler Tests:\n";

  test "create_registry" (fun () ->
    let registry = Handler.create_registry () in
    (* Just check it doesn't crash *)
    ignore registry
  );

  test "register_handler" (fun () ->
    let registry = Handler.create_registry () in
    let loader _ctx = Ok (`String "data") in
    Handler.register_handler ~route_id:"test" ~loader registry;
    match Handler.find_handler "test" registry with
    | Some h -> assert_eq "route_id" "test" h.route_id
    | None -> failwith "expected Some"
  );

  test "find_handler missing" (fun () ->
    let registry = Handler.create_registry () in
    match Handler.find_handler "nonexistent" registry with
    | Some _ -> failwith "expected None"
    | None -> ()
  );

  test "default_config" (fun () ->
    let m = Manifest.empty in
    let config = Handler.default_config m in
    (* Check manifest is stored *)
    assert_eq "manifest routes" "0" (string_of_int (List.length config.manifest.routes))
  );

  test "build_loader_context" (fun () ->
    let ctx = Handler.build_loader_context
      ~params:[("id", "123")]
      ~search_params:[("q", "test")]
      () in
    assert_eq "params count" "1" (string_of_int (List.length ctx.params));
    assert_eq "search count" "1" (string_of_int (List.length ctx.search_params))
  );

  test "build_action_context" (fun () ->
    let ctx = Handler.build_action_context
      ~params:[("id", "456")]
      ~method_:Route_def.POST
      ~body:(Some (`String "body"))
      () in
    assert_eq "params count" "1" (string_of_int (List.length ctx.params));
    assert_true "has body" (Option.is_some ctx.body)
  )

(** {1 Integration Tests} *)

let test_integration () =
  Printf.printf "\nIntegration Tests:\n";

  test "kirin_tanstack facade route" (fun () ->
    let r = Kirin_tanstack.route ~id:"test" ~path:"/test" () in
    assert_eq "id" "test" r.Route_def.id
  );

  test "kirin_tanstack facade loader helpers" (fun () ->
    let r = Kirin_tanstack.loader_ok "data" in
    assert_eq "data" "data" r.Loader.data
  );

  test "kirin_tanstack facade action helpers" (fun () ->
    match Kirin_tanstack.action_success "ok" with
    | Action.Success s -> assert_eq "data" "ok" s
    | _ -> failwith "expected Success"
  );

  test "kirin_tanstack facade create_registry" (fun () ->
    let registry = Kirin_tanstack.create_registry () in
    ignore registry
  );

  test "full workflow" (fun () ->
    (* Create manifest *)
    let entry : Manifest.route_entry = {
      id = "users";
      path = "/users";
      parent_id = None;
      index = false;
      has_loader = true;
      has_action = false;
      has_error_boundary = false;
      has_loading = false;
    } in
    let manifest = Manifest.add_route entry Manifest.empty in

    (* Create registry and register handler *)
    let registry = Handler.create_registry () in
    let loader _ctx = Ok (`Assoc [("users", `List [])]) in
    Handler.register_handler ~route_id:"users" ~loader registry;

    (* Match path *)
    (match Manifest.match_path "/users" manifest with
     | Some result ->
       assert_eq "matched route" "users" result.route.id;
       (* Find handler *)
       (match Handler.find_handler "users" registry with
        | Some h -> assert_eq "handler route" "users" h.route_id
        | None -> failwith "handler not found")
     | None -> failwith "path not matched")
  );

  test "generate code workflow" (fun () ->
    let entry : Manifest.route_entry = {
      id = "home";
      path = "/";
      parent_id = None;
      index = true;
      has_loader = true;
      has_action = false;
      has_error_boundary = false;
      has_loading = false;
    } in
    let manifest = Manifest.add_route entry Manifest.empty in

    let types = Kirin_tanstack.generate_types manifest in
    let router = Kirin_tanstack.generate_router manifest in
    let hooks = Kirin_tanstack.generate_hooks () in

    assert_true "types has RouteId" (String.contains_s types "RouteId");
    assert_true "router has createRouter" (String.contains_s router "createRouter");
    assert_true "hooks has useParams" (String.contains_s hooks "useParams")
  )

(** {1 Main} *)

let () =
  Printf.printf "TanStack Router Test Suite\n";
  Printf.printf "==========================\n";

  test_route_def ();
  test_file_router ();
  test_loader ();
  test_action ();
  test_manifest ();
  test_preload ();
  test_codegen ();
  test_handler ();
  test_integration ();

  Printf.printf "\n==========================\n";
  Printf.printf "Results: %d passed, %d failed\n" !passed !failed;

  if !failed > 0 then exit 1
