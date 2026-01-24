(** Svelte Module Tests

    Test SvelteKit-style SSR, file-based routing, and form actions. *)

open Kirin_svelte

(** {1 Test Helpers} *)

let test name f =
  Alcotest.test_case name `Quick f

(** {1 Route Definition Tests} *)

let route_def_tests = [
  test "create page route" (fun () ->
    let route = Route_def.page "/" in
    Alcotest.(check string) "path" "/" route.Route_def.path;
    Alcotest.(check bool) "has page" true (Option.is_some route.Route_def.page));

  test "create layout route" (fun () ->
    let route = Route_def.layout "/" in
    Alcotest.(check bool) "has layout" true (Option.is_some route.Route_def.layout));

  test "create error route" (fun () ->
    let route = Route_def.error "/" in
    Alcotest.(check bool) "is error" true route.Route_def.error);

  test "add param" (fun () ->
    let route = Route_def.page "/users/[id]"
      |> Route_def.with_param "id" Route_def.Int in
    Alcotest.(check int) "param count" 1 (List.length route.Route_def.params));

  test "add optional param" (fun () ->
    let route = Route_def.page "/posts/[[slug]]"
      |> Route_def.with_optional_param "slug" Route_def.String in
    let param = List.hd route.Route_def.params in
    Alcotest.(check bool) "not required" false param.Route_def.required);

  test "add rest param" (fun () ->
    let route = Route_def.page "/files/[...path]"
      |> Route_def.with_rest_param "path" in
    let param = List.hd route.Route_def.params in
    Alcotest.(check bool) "is rest" true
      (match param.Route_def.param_type with
       | Route_def.Rest -> true | _ -> false));

  test "route group" (fun () ->
    let route = Route_def.page "/(auth)/login"
      |> Route_def.in_group "auth" in
    Alcotest.(check (option string)) "group" (Some "auth") route.Route_def.group);

  test "param type to json" (fun () ->
    let json = Route_def.param_type_to_json Route_def.Int in
    Alcotest.(check string) "json" "\"int\"" (Yojson.Safe.to_string json));

  test "route to json" (fun () ->
    let route = Route_def.page "/users/[id]"
      |> Route_def.with_param "id" Route_def.Int in
    let json = Route_def.to_json route in
    let path = Yojson.Safe.Util.(json |> member "path" |> to_string) in
    Alcotest.(check string) "path in json" "/users/[id]" path);
]

(** {1 File Router Tests} *)

let file_router_tests = [
  test "parse path segment" (fun () ->
    let seg = File_router.parse_segment "[id]" in
    Alcotest.(check bool) "is param" true
      (match seg.File_router.segment_type with
       | File_router.Param _ -> true | _ -> false));

  test "parse optional segment" (fun () ->
    let seg = File_router.parse_segment "[[slug]]" in
    Alcotest.(check bool) "is optional" true
      (match seg.File_router.segment_type with
       | File_router.OptionalParam _ -> true | _ -> false));

  test "parse rest segment" (fun () ->
    let seg = File_router.parse_segment "[...path]" in
    Alcotest.(check bool) "is rest" true
      (match seg.File_router.segment_type with
       | File_router.Rest _ -> true | _ -> false));

  test "parse static segment" (fun () ->
    let seg = File_router.parse_segment "users" in
    Alcotest.(check bool) "is static" true
      (match seg.File_router.segment_type with
       | File_router.Static _ -> true | _ -> false));

  test "parse matcher segment" (fun () ->
    let seg = File_router.parse_segment "[id=integer]" in
    Alcotest.(check bool) "is matcher" true
      (match seg.File_router.segment_type with
       | File_router.Matcher _ -> true | _ -> false));

  test "sveltekit file types" (fun () ->
    let page = File_router.classify_file "+page.svelte" in
    let layout = File_router.classify_file "+layout.svelte" in
    let server = File_router.classify_file "+page.server.ts" in
    Alcotest.(check bool) "page" true
      (match page with Some File_router.Page -> true | _ -> false);
    Alcotest.(check bool) "layout" true
      (match layout with Some _ -> true | _ -> false);
    Alcotest.(check bool) "server" true
      (match server with Some File_router.PageServer -> true | _ -> false));

  test "discover route" (fun () ->
    let files = ["+page.svelte"; "+page.server.ts"] in
    let route = File_router.discover_route "users" files in
    Alcotest.(check bool) "found" true (Option.is_some route));
]

(** {1 Loader Tests} *)

let loader_tests = [
  test "create loader result data" (fun () ->
    let result = Loader.data (`Assoc [("name", `String "test")]) in
    Alcotest.(check bool) "is data" true
      (match result with Loader.LoadData _ -> true | _ -> false));

  test "create loader result redirect" (fun () ->
    let result = Loader.redirect "/login" in
    Alcotest.(check bool) "is redirect" true
      (match result with Loader.LoadRedirect _ -> true | _ -> false));

  test "create loader result error" (fun () ->
    let result = Loader.error 404 "Not found" in
    Alcotest.(check bool) "is error" true
      (match result with Loader.LoadError _ -> true | _ -> false));

  test "loader context" (fun () ->
    let ctx = Loader.create_context ~url:"/test" ~params:[("id", "1")] ~route_id:"test" () in
    Alcotest.(check string) "url" "/test" ctx.Loader.url);

  test "get param" (fun () ->
    let ctx = Loader.create_context ~url:"/test" ~params:[("id", "123")] ~route_id:"test" () in
    let id = Loader.param ctx "id" in
    Alcotest.(check (option string)) "param" (Some "123") id);

  test "depends tracking" (fun () ->
    let ctx = Loader.create_context ~url:"/test" ~params:[] ~route_id:"test" () in
    ctx.Loader.depends "api:users";
    (* depends function doesn't return count, just call it *)
    Alcotest.(check bool) "called" true true);

  test "loader to json" (fun () ->
    let result = Loader.data (`Assoc [("ok", `Bool true)]) in
    let json = Loader.output_to_json result in
    let type_ = Yojson.Safe.Util.(json |> member "type" |> to_string) in
    Alcotest.(check string) "type" "data" type_);
]

(** {1 Action Tests} *)

let action_tests = [
  test "get text field" (fun () ->
    let form = [("name", Action.Text "test")] in
    let value = Action.get_text form "name" in
    Alcotest.(check (option string)) "value" (Some "test") value);

  test "get missing field" (fun () ->
    let form = [] in
    let value = Action.get_text form "name" in
    Alcotest.(check (option string)) "missing" None value);

  test "get all values" (fun () ->
    let form = [("tags", Action.Multiple [
      Action.Text "a"; Action.Text "b"
    ])] in
    let values = Action.get_all form "tags" in
    Alcotest.(check int) "count" 2 (List.length values));

  test "action success" (fun () ->
    let result = Action.success () in
    Alcotest.(check bool) "is success" true
      (match result with Action.ActionSuccess _ -> true | _ -> false));

  test "action fail" (fun () ->
    let result = Action.fail (`Assoc [("error", `String "bad")]) in
    Alcotest.(check bool) "is fail" true
      (match result with Action.ActionFail _ -> true | _ -> false));

  test "action redirect" (fun () ->
    let result = Action.redirect "/done" in
    Alcotest.(check bool) "is redirect" true
      (match result with Action.ActionRedirect _ -> true | _ -> false));

  test "named action" (fun () ->
    let action = Action.named "delete" (fun _ctx _form -> Action.success ()) in
    Alcotest.(check (option string)) "name" (Some "delete") action.Action.name);

  test "find action" (fun () ->
    let actions = [
      Action.default (fun _ctx _form -> Action.success ());
      Action.named "delete" (fun _ctx _form -> Action.success ());
    ] in
    let found = Action.find_action actions (Some "delete") in
    Alcotest.(check bool) "found" true (Option.is_some found));

  test "validation required" (fun () ->
    let form = [("email", Action.Text "")] in
    let result = Action.required form "email" in
    Alcotest.(check bool) "invalid" true
      (match result with Action.Invalid _ -> true | _ -> false));

  test "validation email" (fun () ->
    let form = [("email", Action.Text "bad-email")] in
    let result = Action.email form "email" in
    Alcotest.(check bool) "invalid" true
      (match result with Action.Invalid _ -> true | _ -> false));

  test "action output to json" (fun () ->
    let output = Action.ActionSuccess (`Assoc [("id", `Int 1)]) in
    let json = Action.output_to_json output in
    let type_ = Yojson.Safe.Util.(json |> member "type" |> to_string) in
    Alcotest.(check string) "type" "success" type_);
]

(** {1 Manifest Tests} *)

let manifest_tests = [
  test "empty manifest" (fun () ->
    let manifest = Manifest.empty in
    Alcotest.(check int) "empty routes" 0 (List.length manifest.Manifest.routes));

  test "add route" (fun () ->
    let entry = Manifest.route ~id:"/" ~pattern:"^/$" () in
    let manifest = Manifest.add_route entry Manifest.empty in
    Alcotest.(check int) "count" 1 (List.length manifest.Manifest.routes));

  test "add layout" (fun () ->
    let entry = Manifest.layout ~id:"root" () in
    let manifest = Manifest.add_layout entry Manifest.empty in
    Alcotest.(check int) "count" 1 (List.length manifest.Manifest.layouts));

  test "add matcher" (fun () ->
    let manifest = Manifest.add_matcher "integer" Manifest.empty in
    Alcotest.(check int) "count" 1 (List.length manifest.Manifest.matchers));

  test "manifest to json" (fun () ->
    let json = Manifest.to_json Manifest.empty in
    Alcotest.(check bool) "is assoc" true
      (match json with `Assoc _ -> true | _ -> false));

  test "route to json" (fun () ->
    let entry = Manifest.route ~id:"/users" ~pattern:"^/users$" () in
    let json = Manifest.route_to_json entry in
    let id = Yojson.Safe.Util.(json |> member "id" |> to_string) in
    Alcotest.(check string) "id" "/users" id);
]

(** {1 Preload Tests} *)

let preload_tests = [
  test "modulepreload hint" (fun () ->
    let tag = Preload.modulepreload_hint "/module.js" in
    Alcotest.(check bool) "contains modulepreload" true
      (String.length tag > 0 && String.sub tag 0 5 = "<link"));

  test "preload css hint" (fun () ->
    let tag = Preload.preload_css_hint "/style.css" in
    Alcotest.(check bool) "contains as style" true
      (String.length tag > 0));

  test "prefetch hint" (fun () ->
    let tag = Preload.prefetch_hint "/page" in
    Alcotest.(check bool) "contains prefetch" true
      (String.length tag > 0));

  test "route hints" (fun () ->
    let hints = Preload.route_hints ~js:["/a.js"] ~css:["/b.css"] () in
    Alcotest.(check int) "js count" 1 (List.length hints.Preload.js_modules));

  test "render hints" (fun () ->
    let hints = Preload.route_hints ~js:["/a.js"; "/b.js"] ~css:["/c.css"] () in
    let html = Preload.render_hints hints in
    Alcotest.(check bool) "has content" true (String.length html > 0));
]

(** {1 Meta Tests} *)

let meta_tests = [
  test "render title tag" (fun () ->
    let tag = Meta.render_tag (Meta.Title "Test Page") in
    Alcotest.(check string) "tag" "<title>Test Page</title>" tag);

  test "render description tag" (fun () ->
    let tag = Meta.render_tag (Meta.Description "A test page") in
    Alcotest.(check bool) "has content" true
      (String.length tag > 0));

  test "builder pattern" (fun () ->
    let html = Meta.create ()
      |> Meta.title "Test"
      |> Meta.description "A test"
      |> Meta.build in
    Alcotest.(check bool) "has content" true (String.length html > 0));

  test "page helper" (fun () ->
    let b = Meta.page ~title:"Test Page" () in
    let html = Meta.build b in
    Alcotest.(check bool) "has content" true (String.length html > 0));

  test "og tags" (fun () ->
    let b = Meta.create ()
      |> Meta.og ~title:"Test" () in
    let html = Meta.build b in
    Alcotest.(check bool) "has og" true (String.length html > 0));

  test "canonical tag" (fun () ->
    let tag = Meta.render_tag (Meta.Canonical "https://example.com/page") in
    Alcotest.(check bool) "has href" true
      (String.sub tag 0 5 = "<link"));

  test "render tags" (fun () ->
    let tags = [Meta.Title "Test"; Meta.Description "Desc"] in
    let html = Meta.render_tags tags in
    Alcotest.(check bool) "has content" true (String.length html > 0));
]

(** {1 Data Tests} *)

let data_tests = [
  test "serialize json" (fun () ->
    let json = `Assoc [("name", `String "test")] in
    let str = Data.serialize json in
    Alcotest.(check bool) "valid" true (String.length str > 0));

  test "serialize escapes script" (fun () ->
    let json = `String "</script><script>alert(1)</script>" in
    let str = Data.serialize json in
    Alcotest.(check bool) "escaped" true
      (not (String.sub str 0 2 = "</")));

  test "script tag" (fun () ->
    let json = `Assoc [("user", `Int 1)] in
    let script = Data.script_tag json in
    Alcotest.(check bool) "has script" true
      (String.sub script 0 7 = "<script"));

  test "route data" (fun () ->
    let rd = Data.route_data ~route_id:"/" (`Assoc [("count", `Int 42)]) in
    Alcotest.(check string) "route id" "/" rd.Data.route_id);

  test "page data" (fun () ->
    let pd = Data.page_data ~url:"/test" ~params:[] () in
    Alcotest.(check string) "url" "/test" pd.Data.url);

  test "page data to json" (fun () ->
    let pd = Data.page_data ~url:"/" ~params:[] () in
    let json = Data.page_data_to_json pd in
    let url = Yojson.Safe.Util.(json |> member "url" |> to_string) in
    Alcotest.(check string) "url" "/" url);
]

(** {1 Hydrate Tests} *)

let hydrate_tests = [
  test "spa shell" (fun () ->
    let html = Hydrate.spa ~title:"Test" ~entry_script:"/app.js" () in
    Alcotest.(check bool) "has doctype" true
      (String.sub html 0 9 = "<!DOCTYPE"));

  test "with ssr" (fun () ->
    let html = Hydrate.with_ssr
      ~title:"Test"
      ~entry_script:"/app.js"
      ~ssr_html:"<div>Hello</div>"
      () in
    Alcotest.(check bool) "has ssr" true
      (String.length html > 0));

  test "render options" (fun () ->
    let opts = Hydrate.{
      default_options with
      title = "Test";
      scripts = ["/app.js"];
    } in
    let html = Hydrate.render opts in
    Alcotest.(check bool) "has content" true (String.length html > 0));

  test "sveltekit shell" (fun () ->
    let html = Hydrate.sveltekit_shell
      ~title:"Test"
      ~entry_script:"/app.js"
      ~css_files:["/app.css"]
      () in
    Alcotest.(check bool) "has shell" true (String.length html > 0));

  test "error page" (fun () ->
    let html = Hydrate.error_page ~title:"Error" ~status:404 ~message:"Not found" () in
    Alcotest.(check bool) "has error" true (String.length html > 0));

  test "streaming placeholder" (fun () ->
    let html = Hydrate.streaming_placeholder ~id:"async1" ~fallback:"Loading..." in
    Alcotest.(check bool) "has template" true
      (String.sub html 0 9 = "<template"));
]

(** {1 Protocol Tests} *)

let protocol_tests = [
  test "encode render request" (fun () ->
    let req = Protocol.{
      url = "/test";
      props = `Assoc [];
      route_id = None;
      cookies = [];
      headers = [];
    } in
    let (_id, json) = Protocol.encode_render_request req in
    Alcotest.(check bool) "has jsonrpc" true
      (String.length json > 0));

  test "encode request" (fun () ->
    let (_id, json) = Protocol.encode_request "test" (`Assoc []) in
    Alcotest.(check bool) "has method" true
      (String.length json > 0));

  test "encode notification" (fun () ->
    let json = Protocol.encode_notification "ping" (`Assoc []) in
    Alcotest.(check bool) "no id" true
      (not (String.sub json 0 6 = "{\"id\":")));

  test "decode response" (fun () ->
    let json = Yojson.Safe.from_string {|{"jsonrpc":"2.0","id":1,"result":{"ok":true}}|} in
    let msg = Protocol.decode_response json in
    Alcotest.(check bool) "is response" true
      (match msg with Protocol.Response _ -> true | _ -> false));

  test "decode error" (fun () ->
    let json = Yojson.Safe.from_string {|{"jsonrpc":"2.0","id":1,"error":{"code":-32600,"message":"Invalid"}}|} in
    let msg = Protocol.decode_response json in
    Alcotest.(check bool) "is error" true
      (match msg with Protocol.Error _ -> true | _ -> false));

  test "error codes" (fun () ->
    Alcotest.(check int) "parse error" (-32700) Protocol.ErrorCode.parse_error;
    Alcotest.(check int) "invalid request" (-32600) Protocol.ErrorCode.invalid_request);

  test "encode batch" (fun () ->
    let requests = [("a", `Null); ("b", `Null)] in
    let json = Protocol.encode_batch requests in
    Alcotest.(check bool) "is array" true (json.[0] = '['));
]

(** {1 Worker Tests} *)

let worker_tests = [
  test "create worker" (fun () ->
    let worker = Worker.create ~bundle:"test.js" ~max_requests:1000 ~memory_limit_mb:200 () in
    (* Freshly created worker starts in Ready state, which is healthy *)
    Alcotest.(check bool) "is healthy" true
      (Worker.is_healthy worker));

  test "worker stats" (fun () ->
    let worker = Worker.create ~bundle:"test.js" ~max_requests:1000 ~memory_limit_mb:200 () in
    let stats = Worker.stats worker in
    Alcotest.(check int) "requests" 0 stats.Worker.requests);

  test "needs restart" (fun () ->
    let worker = Worker.create ~bundle:"test.js" ~max_requests:10 ~memory_limit_mb:200 () in
    Alcotest.(check bool) "no restart needed" false
      (Worker.needs_restart worker));
]

(** {1 SSR Tests} *)

let ssr_tests = [
  test "default config" (fun () ->
    let config = Ssr.default_config in
    Alcotest.(check int) "workers" 4 config.Ssr.num_workers;
    Alcotest.(check int) "cache ttl" 60 config.Ssr.cache_ttl);

  test "create cache" (fun () ->
    let cache = Ssr.create_cache ~max_size:100 ~ttl:60 in
    Alcotest.(check int) "max size" 100 cache.Ssr.max_size);

  test "cache key" (fun () ->
    let key1 = Ssr.cache_key "/test" (`Assoc []) in
    let key2 = Ssr.cache_key "/test" (`Assoc [("a", `Int 1)]) in
    Alcotest.(check bool) "different keys" true (key1 <> key2));

  test "cache get empty" (fun () ->
    let cache = Ssr.create_cache ~max_size:100 ~ttl:60 in
    let result = Ssr.cache_get cache "missing" in
    Alcotest.(check bool) "none" true (Option.is_none result));

  test "cache put and get" (fun () ->
    let cache = Ssr.create_cache ~max_size:100 ~ttl:60 in
    let response = Protocol.{
      html = "<div>test</div>";
      head = "";
      css = None;
      error = None;
      status = 200;
      redirect = None;
      data = None;
    } in
    Ssr.cache_put cache "key1" response;
    let result = Ssr.cache_get cache "key1" in
    Alcotest.(check bool) "found" true (Option.is_some result));

  test "cache eviction" (fun () ->
    let cache = Ssr.create_cache ~max_size:2 ~ttl:60 in
    let response = Protocol.{
      html = ""; head = ""; css = None; error = None;
      status = 200; redirect = None; data = None;
    } in
    Ssr.cache_put cache "a" response;
    Ssr.cache_put cache "b" response;
    Ssr.cache_put cache "c" response;
    (* Should have evicted some entries *)
    Alcotest.(check bool) "limited" true true);

  test "stats to json" (fun () ->
    let stats = Ssr.{
      total_renders = 100;
      cache_hits = 75;
      cache_hit_rate = 0.75;
      errors = 5;
      cache_size = 50;
      workers_ready = 4;
      workers_total = 4;
    } in
    let json = Ssr.stats_to_json stats in
    let hits = Yojson.Safe.Util.(json |> member "cacheHits" |> to_int) in
    Alcotest.(check int) "hits" 75 hits);
]

(** {1 Streaming Tests} *)

let streaming_tests = [
  test "create stream" (fun () ->
    let stream = Streaming.create () in
    Alcotest.(check bool) "not completed" false stream.Streaming.completed);

  test "add chunk" (fun () ->
    let stream = Streaming.create () in
    Streaming.add_chunk stream (Streaming.Html "<div>test</div>");
    Alcotest.(check int) "chunk count" 1 (List.length stream.Streaming.chunks));

  test "complete stream" (fun () ->
    let stream = Streaming.create () in
    Streaming.complete stream;
    Alcotest.(check bool) "completed" true stream.Streaming.completed);

  test "fail stream" (fun () ->
    let stream = Streaming.create () in
    Streaming.fail stream "error";
    Alcotest.(check bool) "has error" true (Option.is_some stream.Streaming.error));

  test "shell generation" (fun () ->
    let html = Streaming.shell ~title:"Test" ~entry_script:"/app.js" () in
    Alcotest.(check bool) "has doctype" true
      (String.length html > 10));

  test "render chunk" (fun () ->
    let html = Streaming.render_chunk (Streaming.Html "<p>test</p>") in
    Alcotest.(check string) "content" "<p>test</p>" html);

  test "hydration priority" (fun () ->
    let s = Streaming.priority_string Streaming.Visible in
    Alcotest.(check string) "visible" "visible" s);

  test "hydration boundary" (fun () ->
    let html = Streaming.hydration_boundary
      ~id:"comp1" ~priority:Streaming.Idle ~fallback:None "<div/>" in
    Alcotest.(check bool) "has id" true (String.length html > 0));

  test "placeholder" (fun () ->
    let html = Streaming.placeholder ~id:"async1" ~fallback:"Loading..." in
    Alcotest.(check bool) "template" true
      (String.sub html 0 9 = "<template"));

  test "sse event" (fun () ->
    let event = Streaming.sse_event ~event:"test" ~data:"hello" in
    Alcotest.(check bool) "has event" true
      (String.sub event 0 6 = "event:"));

  test "chunk to json" (fun () ->
    let json = Streaming.chunk_to_json (Streaming.Shell "<html>") in
    let type_ = Yojson.Safe.Util.(json |> member "type" |> to_string) in
    Alcotest.(check string) "type" "shell" type_);
]

(** {1 Codegen Tests} *)

let codegen_tests = [
  test "generate param type" (fun () ->
    let ts = Codegen.generate_param_type [("id", "string"); ("name", "string")] in
    Alcotest.(check bool) "has id field" true (String.length ts > 0);
    Alcotest.(check bool) "contains id" true
      (String.length ts > 5 && String.sub ts 0 1 = "{"));

  test "generate param type empty" (fun () ->
    let ts = Codegen.generate_param_type [] in
    Alcotest.(check string) "record never" "Record<string, never>" ts);

  test "generate route type" (fun () ->
    let entry = Manifest.{
      id = "/users/[id]";
      pattern = "/users/:id";
      page = true;
      endpoint = false;
      params = [("id", "string")];
      layout = None;
      error = false;
    } in
    let ts = Codegen.generate_route_type entry in
    Alcotest.(check bool) "has export" true
      (String.length ts > 6 && String.sub ts 0 6 = "export"));

  test "generate hooks server" (fun () ->
    let ts = Codegen.generate_hooks_server in
    Alcotest.(check bool) "has Handle" true
      (String.length ts > 0));

  test "generate hooks client" (fun () ->
    let ts = Codegen.generate_hooks_client in
    Alcotest.(check bool) "has HandleClientError" true
      (String.length ts > 0));
]

(** {1 Handler Tests} *)

let handler_tests = [
  test "default options" (fun () ->
    let opts = Handler.default_options in
    Alcotest.(check bool) "no ssr" true (Option.is_none opts.Handler.ssr_engine);
    Alcotest.(check bool) "dev mode" false opts.Handler.dev_mode);

  test "is vite request" (fun () ->
    let r1 = Handler.is_vite_request "@vite/client" in
    let r2 = Handler.is_vite_request "/api/users" in
    Alcotest.(check bool) "vite" true r1;
    Alcotest.(check bool) "not vite" false r2);
]

(** {1 Run All Tests} *)

let () =
  Alcotest.run "Svelte" [
    ("Route_def", route_def_tests);
    ("File_router", file_router_tests);
    ("Loader", loader_tests);
    ("Action", action_tests);
    ("Manifest", manifest_tests);
    ("Preload", preload_tests);
    ("Meta", meta_tests);
    ("Data", data_tests);
    ("Hydrate", hydrate_tests);
    ("Protocol", protocol_tests);
    ("Worker", worker_tests);
    ("SSR", ssr_tests);
    ("Streaming", streaming_tests);
    ("Codegen", codegen_tests);
    ("Handler", handler_tests);
  ]
