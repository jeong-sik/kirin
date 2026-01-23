(** Solid.js SSR Tests *)

open Kirin_solid

(* ===========================================
   Test Helpers
   =========================================== *)

module String = struct
  include String

  let contains_s haystack needle =
    let nl = length needle and hl = length haystack in
    if nl > hl then false
    else
      let rec check i =
        if i > hl - nl then false
        else if sub haystack i nl = needle then true
        else check (i + 1)
      in
      check 0
end

let test_count = ref 0
let pass_count = ref 0
let fail_count = ref 0

let test name f =
  incr test_count;
  try
    f ();
    incr pass_count;
    Printf.printf "  ✓ %s\n" name
  with e ->
    incr fail_count;
    Printf.printf "  ✗ %s: %s\n" name (Printexc.to_string e)

let assert_eq expected actual msg =
  if expected <> actual then
    failwith (Printf.sprintf "%s: expected %s, got %s" msg expected actual)

let assert_eq_int expected actual msg =
  if expected <> actual then
    failwith (Printf.sprintf "%s: expected %d, got %d" msg expected actual)

let assert_eq_opt expected actual msg =
  if expected <> actual then
    let str_of_opt = function Some s -> "Some " ^ s | None -> "None" in
    failwith (Printf.sprintf "%s: expected %s, got %s" msg (str_of_opt expected) (str_of_opt actual))

let assert_eq_bool expected actual msg =
  if expected <> actual then
    failwith (Printf.sprintf "%s: expected %b, got %b" msg expected actual)

let assert_true cond msg =
  if not cond then failwith msg

let assert_contains haystack needle msg =
  if not (String.contains_s haystack needle) then
    failwith (Printf.sprintf "%s: '%s' not found in '%s'" msg needle
      (if String.length haystack > 100 then String.sub haystack 0 100 ^ "..." else haystack))

(* ===========================================
   Meta Tag Tests
   =========================================== *)

let test_meta () =
  Printf.printf "\n📝 Meta Tests:\n";

  test "create empty builder" (fun () ->
    let b = Meta.create () in
    let result = Meta.build b in
    assert_eq "" result "empty builder should produce empty string"
  );

  test "add title" (fun () ->
    let b = Meta.create () in
    let b = Meta.title "Test Page" b in
    let result = Meta.build b in
    assert_contains result "<title>Test Page</title>" "should contain title tag"
  );

  test "add description" (fun () ->
    let b = Meta.create () in
    let b = Meta.description "A test description" b in
    let result = Meta.build b in
    assert_contains result "description" "should contain description meta"
  );

  test "add charset" (fun () ->
    let b = Meta.create () in
    let b = Meta.charset "utf-8" b in
    let result = Meta.build b in
    assert_contains result "charset" "should contain charset meta"
  );

  test "add viewport" (fun () ->
    let b = Meta.create () in
    let b = Meta.viewport "width=device-width" b in
    let result = Meta.build b in
    assert_contains result "viewport" "should contain viewport meta"
  );

  test "add og tags" (fun () ->
    let b = Meta.create () in
    let b = Meta.og ~title:"OG Title" ~description:"OG Desc" () b in
    let result = Meta.build b in
    assert_contains result "og:title" "should contain og:title";
    assert_contains result "og:description" "should contain og:description"
  );

  test "add twitter card" (fun () ->
    let b = Meta.create () in
    let b = Meta.twitter ~card:"summary" ~title:"Tweet" () b in
    let result = Meta.build b in
    assert_contains result "twitter:card" "should contain twitter:card"
  );

  test "add canonical URL" (fun () ->
    let b = Meta.create () in
    let b = Meta.canonical "https://example.com" b in
    let result = Meta.build b in
    assert_contains result "canonical" "should contain canonical link"
  );

  test "add robots meta" (fun () ->
    let b = Meta.create () in
    let b = Meta.robots "noindex,nofollow" b in
    let result = Meta.build b in
    assert_contains result "robots" "should contain robots meta"
  );

  test "add custom meta" (fun () ->
    let b = Meta.create () in
    let b = Meta.custom ~name:"author" ~content:"Test Author" b in
    let result = Meta.build b in
    assert_contains result "author" "should contain custom meta"
  );

  test "chain multiple tags" (fun () ->
    let result =
      Meta.create ()
      |> Meta.title "Test"
      |> Meta.description "Desc"
      |> Meta.charset "utf-8"
      |> Meta.build
    in
    assert_contains result "<title>" "should contain title";
    assert_contains result "description" "should contain description";
    assert_contains result "charset" "should contain charset"
  );

  test "defaults function" (fun () ->
    let b = Meta.defaults () in
    let result = Meta.build b in
    assert_contains result "charset" "defaults should include charset";
    assert_contains result "viewport" "defaults should include viewport"
  );

  test "page convenience function" (fun () ->
    let b = Meta.page ~title:"Page Title" ~description:"Page desc" () in
    let result = Meta.build b in
    assert_contains result "Page Title" "should contain page title"
  );

  test "to_json serialization" (fun () ->
    let b = Meta.create () |> Meta.title "JSON Test" in
    let json = Meta.to_json b in
    match json with
    | `List _ -> ()
    | _ -> failwith "to_json should return list"
  )

(* ===========================================
   Data Serialization Tests
   =========================================== *)

let test_data () =
  Printf.printf "\n💾 Data Tests:\n";

  test "serialize simple json" (fun () ->
    let json = `Assoc [("key", `String "value")] in
    let result = Data.serialize json in
    assert_contains result "key" "should contain key"
  );

  test "XSS escape in serialization" (fun () ->
    let json = `String "</script><script>alert(1)</script>" in
    let result = Data.serialize json in
    assert_true (not (String.contains_s result "</script>")) "should escape script tags"
  );

  test "create script tag" (fun () ->
    let json = `Assoc [("user", `String "test")] in
    let result = Data.script_tag json in
    assert_contains result "__SOLID_DATA__" "should contain solid data var";
    assert_contains result "<script>" "should be wrapped in script tag"
  );

  test "create route data tag" (fun () ->
    let json = `Assoc [("items", `List [])] in
    let result = Data.route_data_tag ~route_id:"users" json in
    assert_contains result "__ROUTE_DATA__" "should contain route data var";
    assert_contains result "users" "should contain route id"
  );

  test "create store data tag" (fun () ->
    let json = `Assoc [("count", `Int 0)] in
    let result = Data.store_data_tag ~store_id:"counter" json in
    assert_contains result "__SOLID_STORES__" "should contain solid stores var"
  );

  test "create signal data tag" (fun () ->
    let result = Data.signal_data_tag ~signal_id:"theme" (`String "dark") in
    assert_contains result "__SOLID_SIGNALS__" "should contain solid signals var"
  );

  test "hydration markers" (fun () ->
    let start_marker = Data.hydration_start ~component_id:"c1" in
    let end_marker = Data.hydration_end ~component_id:"c1" in
    assert_contains start_marker "c1" "should have component id in start marker";
    assert_contains end_marker "c1" "should have component id in end marker"
  );

  test "empty initial data" (fun () ->
    let result = Data.all_scripts Data.empty_initial_data in
    assert_eq "" result "empty data should produce empty scripts"
  );

  test "combined initial data scripts" (fun () ->
    let data = { Data.empty_initial_data with
      route_id = Some "test-route";
      route_data = Some (`Assoc [("key", `String "val")]);
    } in
    let result = Data.all_scripts data in
    assert_contains result "__ROUTE_DATA__" "should contain route data"
  )

(* ===========================================
   Hydration Tests
   =========================================== *)

let test_hydrate () =
  Printf.printf "\n💧 Hydrate Tests:\n";

  test "default options" (fun () ->
    let opts = Hydrate.default_options in
    assert_eq "en" opts.lang "default lang should be en";
    assert_eq "app" opts.root_id "default root_id should be app"
  );

  test "render basic shell" (fun () ->
    let html = Hydrate.render Hydrate.default_options in
    assert_contains html "<!DOCTYPE html>" "should have doctype";
    assert_contains html "<html" "should have html tag";
    assert_contains html "<head>" "should have head tag";
    assert_contains html "<body>" "should have body tag"
  );

  test "render shell with title" (fun () ->
    let opts = { Hydrate.default_options with title = "Test Page" } in
    let html = Hydrate.render opts in
    assert_contains html "<title>Test Page</title>" "should contain title"
  );

  test "render shell with scripts" (fun () ->
    let opts = { Hydrate.default_options with scripts = ["/app.js"; "/vendor.js"] } in
    let html = Hydrate.render opts in
    assert_contains html "/app.js" "should contain first script";
    assert_contains html "/vendor.js" "should contain second script"
  );

  test "render shell with styles" (fun () ->
    let opts = { Hydrate.default_options with styles = ["/style.css"] } in
    let html = Hydrate.render opts in
    assert_contains html "/style.css" "should contain stylesheet"
  );

  test "render with SSR content" (fun () ->
    let html = Hydrate.render ~ssr_html:"<div>SSR Content</div>" Hydrate.default_options in
    assert_contains html "SSR Content" "should contain SSR HTML"
  );

  test "spa shell helper" (fun () ->
    let html = Hydrate.spa ~title:"SPA App" ~entry_script:"/app.js" () in
    assert_contains html "SPA App" "should contain title";
    assert_contains html "/app.js" "should contain entry script"
  );

  test "ssr shell helper" (fun () ->
    let html = Hydrate.with_ssr
      ~title:"SSR App"
      ~entry_script:"/app.js"
      ~ssr_html:"<div>Server rendered</div>"
      ()
    in
    assert_contains html "Server rendered" "should contain SSR content"
  );

  test "island wrapper" (fun () ->
    let html = Hydrate.island
      ~component_name:"Counter"
      ~props_json:(`Assoc [("initial", `Int 0)])
      "<button>+</button>"
    in
    assert_contains html "solid-island" "should have island wrapper";
    assert_contains html "Counter" "should have component name"
  );

  test "lazy island" (fun () ->
    let html = Hydrate.lazy_island
      ~component_path:"./Counter.tsx"
      ~props_json:(`Assoc [])
    in
    assert_contains html "data-import" "should have import path"
  );

  test "streaming placeholder" (fun () ->
    let html = Hydrate.streaming_placeholder ~id:"s1" ~fallback:"Loading..." in
    assert_contains html "template" "should use template tag";
    assert_contains html "s1" "should contain id"
  );

  test "streaming replacement" (fun () ->
    let html = Hydrate.streaming_replacement ~id:"s1" "<div>Content</div>" in
    assert_contains html "script" "should be a script tag";
    assert_contains html "s1" "should reference placeholder id"
  )

(* ===========================================
   Protocol Tests
   =========================================== *)

let test_protocol () =
  Printf.printf "\n📡 Protocol Tests:\n";

  test "encode render request" (fun () ->
    let req : Protocol.render_request = {
      url = "/test";
      props = `Assoc [("key", `String "val")];
      meta = None;
    } in
    let (_id, json) = Protocol.encode_render_request req in
    assert_contains json "\"jsonrpc\"" "should have jsonrpc field";
    assert_contains json "\"method\":\"render\"" "should have method"
  );

  test "encode health request" (fun () ->
    let (_id, json) = Protocol.encode_health_request () in
    assert_contains json "\"method\":\"health\"" "should have health method"
  );

  test "decode success response" (fun () ->
    let json = {|{"jsonrpc":"2.0","id":1,"result":{"html":"<div>test</div>","head":"","status":200}}|} in
    match Protocol.decode_render_response json with
    | Ok resp ->
      assert_eq "<div>test</div>" resp.html "should decode html";
      assert_eq_int 200 resp.status "should decode status"
    | Error e -> failwith ("decode failed: " ^ e)
  );

  test "decode error response" (fun () ->
    let json = {|{"jsonrpc":"2.0","id":1,"error":{"code":-32600,"message":"Invalid"}}|} in
    match Protocol.decode_render_response json with
    | Error _ -> ()
    | Ok _ -> failwith "should return error for error response"
  );

  test "decode redirect response" (fun () ->
    let json = {|{"jsonrpc":"2.0","id":1,"result":{"html":"","head":"","status":302,"redirect":"/login"}}|} in
    match Protocol.decode_render_response json with
    | Ok resp ->
      assert_eq_int 302 resp.status "should have redirect status";
      assert_eq_opt (Some "/login") resp.redirect "should have redirect url"
    | Error e -> failwith ("decode failed: " ^ e)
  );

  test "decode stream chunk - html" (fun () ->
    let chunk = {|{"type":"html","content":"<div>chunk</div>"}|} in
    match Protocol.decode_stream_chunk chunk with
    | Protocol.Html html -> assert_contains html "chunk" "should decode html chunk"
    | _ -> failwith "expected Html chunk"
  );

  test "decode stream chunk - script" (fun () ->
    let chunk = {|{"type":"script","content":"console.log('test')"}|} in
    match Protocol.decode_stream_chunk chunk with
    | Protocol.Script script -> assert_contains script "console" "should decode script chunk"
    | _ -> failwith "expected Script chunk"
  );

  test "decode stream chunk - complete" (fun () ->
    let chunk = {|{"type":"complete"}|} in
    match Protocol.decode_stream_chunk chunk with
    | Protocol.Complete -> ()
    | _ -> failwith "expected Complete chunk"
  );

  test "decode stream chunk - error" (fun () ->
    let chunk = {|{"type":"error","message":"render failed"}|} in
    match Protocol.decode_stream_chunk chunk with
    | Protocol.Error msg -> assert_contains msg "failed" "should have error message"
    | _ -> failwith "expected Error chunk"
  )

(* ===========================================
   Worker Tests
   =========================================== *)

let test_worker () =
  Printf.printf "\n👷 Worker Tests:\n";

  test "create worker" (fun () ->
    let worker = Worker.create
      ~bundle:"test.js"
      ~max_requests:1000
      ~timeout:5.0
      ()
    in
    assert_true (not (Worker.is_ready worker)) "new worker should not be ready"
  );

  test "worker initial state" (fun () ->
    let worker = Worker.create ~bundle:"test.js" ~max_requests:1000 ~timeout:5.0 () in
    match Worker.get_state worker with
    | Worker.Stopped -> ()
    | _ -> failwith "new worker should be stopped"
  );

  test "worker needs restart when unhealthy" (fun () ->
    let worker = Worker.create ~bundle:"test.js" ~max_requests:1000 ~timeout:5.0 () in
    (* New worker is stopped, so doesn't need restart *)
    assert_true (not (Worker.needs_restart worker)) "stopped worker doesn't need restart"
  );

  test "worker stop is safe" (fun () ->
    let worker = Worker.create
      ~bundle:"test.js"
      ~max_requests:1000
      ~timeout:5.0
      ()
    in
    (* Stopping a stopped worker should be safe *)
    Worker.stop worker
  );

  test "worker health check when stopped" (fun () ->
    let worker = Worker.create ~bundle:"test.js" ~max_requests:1000 ~timeout:5.0 () in
    match Worker.health_check worker with
    | Error _ -> ()
    | Ok _ -> failwith "stopped worker health check should fail"
  )

(* ===========================================
   SSR Engine Tests
   =========================================== *)

let test_ssr () =
  Printf.printf "\n🚀 SSR Engine Tests:\n";

  test "default config" (fun () ->
    let config = Ssr.default_config ~bundle:"entry.js" in
    assert_eq "entry.js" config.bundle "should have bundle path";
    assert_eq_int 4 config.workers "default workers should be 4";
    assert_true (config.timeout > 0.0) "timeout should be positive"
  );

  test "create engine" (fun () ->
    let config = Ssr.default_config ~bundle:"test.js" in
    let engine = Ssr.create config in
    let stats = Ssr.stats engine in
    assert_eq_int 0 stats.total_renders "new engine should have 0 renders"
  );

  test "engine stats" (fun () ->
    let config = Ssr.default_config ~bundle:"test.js" in
    let engine = Ssr.create config in
    let stats = Ssr.stats engine in
    assert_eq_int 4 stats.workers_total "should have 4 workers";
    assert_eq_int 0 stats.errors "should have 0 errors initially"
  );

  test "render without workers ready" (fun () ->
    let config = Ssr.default_config ~bundle:"test.js" in
    let engine = Ssr.create config in
    match Ssr.render engine ~url:"/" ~props:(`Assoc []) () with
    | Error msg -> assert_contains msg "No workers" "should fail with no workers"
    | Ok _ -> failwith "should fail when no workers ready"
  );

  test "render with fallback" (fun () ->
    let config = Ssr.default_config ~bundle:"test.js" in
    let engine = Ssr.create config in
    let result = Ssr.render_with_fallback
      engine
      ~url:"/"
      ~props:(`Assoc [])
      ~fallback:"<div>Fallback</div>"
      ()
    in
    assert_eq "<div>Fallback</div>" result "should return fallback on error"
  );

  test "cache operations" (fun () ->
    let config = Ssr.default_config ~bundle:"test.js" in
    let engine = Ssr.create config in
    assert_eq_int 0 (Ssr.cache_size engine) "new engine cache should be empty";
    Ssr.clear_cache engine;
    assert_eq_int 0 (Ssr.cache_size engine) "cleared cache should be empty"
  );

  test "health check" (fun () ->
    let config = Ssr.default_config ~bundle:"test.js" in
    let engine = Ssr.create config in
    let health = Ssr.health_check engine in
    assert_eq_int 4 (Array.length health) "should check all workers"
  );

  test "recover unhealthy" (fun () ->
    let config = Ssr.default_config ~bundle:"test.js" in
    let engine = Ssr.create config in
    (* Should not fail even if workers are stopped *)
    Ssr.recover_unhealthy engine
  );

  test "shutdown engine" (fun () ->
    let config = Ssr.default_config ~bundle:"test.js" in
    let engine = Ssr.create config in
    Ssr.shutdown engine;
    (* After shutdown, stats should still work *)
    let stats = Ssr.stats engine in
    assert_eq_int 0 stats.workers_ready "no workers should be ready after shutdown"
  )

(* ===========================================
   Router Tests
   =========================================== *)

let test_router () =
  Printf.printf "\n🛤️ Router Tests:\n";

  test "create simple route" (fun () ->
    let route = Router.route ~path:"/users" ~component:"./Users.tsx" () in
    assert_eq "/users" route.path "should have correct path";
    assert_eq "./Users.tsx" route.component "should have correct component"
  );

  test "create route with children" (fun () ->
    let child = Router.route ~path:":id" ~component:"./User.tsx" () in
    let parent = Router.route ~path:"/users" ~component:"./Users.tsx" ~children:[child] () in
    assert_eq_int 1 (List.length parent.children) "should have one child"
  );

  test "create index route" (fun () ->
    let route = Router.index ~component:"./Index.tsx" () in
    assert_eq "" route.path "index route should have empty path"
  );

  test "create catch-all route" (fun () ->
    let route = Router.catch_all ~component:"./NotFound.tsx" () in
    assert_eq "*" route.path "catch-all should have * path"
  );

  test "create layout route" (fun () ->
    let route = Router.layout ~component:"./Layout.tsx" () in
    assert_eq "" route.path "layout should have empty path"
  );

  test "match simple path" (fun () ->
    let routes = [Router.route ~path:"/users" ~component:"./Users.tsx" ()] in
    match Router.find_route "/users" routes with
    | Some _ -> ()
    | None -> failwith "should match /users"
  );

  test "match dynamic path" (fun () ->
    let routes = [Router.route ~path:"/users/:id" ~component:"./User.tsx" ()] in
    match Router.find_route "/users/123" routes with
    | Some result ->
      (match List.assoc_opt "id" result.params with
       | Some "123" -> ()
       | _ -> failwith "should capture id param")
    | None -> failwith "should match /users/123"
  );

  test "match catch-all" (fun () ->
    let routes = [Router.route ~path:"/docs/*" ~component:"./Docs.tsx" ()] in
    match Router.find_route "/docs/api/users" routes with
    | Some result ->
      (match List.assoc_opt "*" result.params with
       | Some "api/users" -> ()
       | _ -> failwith "should capture rest of path")
    | None -> failwith "should match catch-all"
  );

  test "no match returns None" (fun () ->
    let routes = [Router.route ~path:"/users" ~component:"./Users.tsx" ()] in
    match Router.find_route "/posts" routes with
    | None -> ()
    | Some _ -> failwith "should not match /posts"
  );

  test "route to config" (fun () ->
    let route = Router.route ~path:"/users" ~component:"./Users.tsx" () in
    let config = Router.route_to_config route in
    assert_contains config "path" "config should have path";
    assert_contains config "component" "config should have component"
  );

  test "routes to config array" (fun () ->
    let routes = [
      Router.route ~path:"/users" ~component:"./Users.tsx" ();
      Router.route ~path:"/posts" ~component:"./Posts.tsx" ();
    ] in
    let config = Router.routes_to_config routes in
    assert_contains config "[" "should be array";
    assert_contains config "/users" "should contain first route"
  );

  test "preload link" (fun () ->
    let route = Router.route ~path:"/users" ~component:"./Users.tsx" ~preload:true () in
    let link = Router.preload_link route in
    assert_contains link "modulepreload" "should be modulepreload link"
  );

  test "loader result serialization" (fun () ->
    let data = Router.Data (`Assoc [("id", `Int 1)]) in
    let json = Router.serialize_loader_result (fun x -> x) data in
    match json with
    | `Assoc fields ->
      (match List.assoc_opt "type" fields with
       | Some (`String "data") -> ()
       | _ -> failwith "should have type data")
    | _ -> failwith "should be object"
  )

(* ===========================================
   Streaming Tests
   =========================================== *)

let test_streaming () =
  Printf.printf "\n📡 Streaming Tests:\n";

  test "default streaming options" (fun () ->
    let opts = Streaming.default_options in
    assert_true (opts.buffer_size > 0) "buffer size should be positive";
    assert_true opts.progressive "progressive should be enabled by default"
  );

  test "shell with placeholder" (fun () ->
    let shell = Streaming.shell_with_placeholder
      ~head:"<title>Test</title>"
      ~body_start:"<div id='app'>"
      ~suspense_fallbacks:["<div>Loading...</div>"]
    in
    assert_contains shell "<!DOCTYPE html>" "should have doctype";
    assert_contains shell "Loading..." "should contain fallback"
  );

  test "replacement script" (fun () ->
    let script = Streaming.replacement_script ~suspense_id:"s1" ~content:"<div>Done</div>" in
    assert_contains script "$SR" "should call $SR function";
    assert_contains script "s1" "should reference suspense id"
  );

  test "runtime script" (fun () ->
    let script = Streaming.runtime_script in
    assert_contains script "$SR" "should define $SR function";
    assert_contains script "function" "should be a function"
  );

  test "format chunk" (fun () ->
    let chunk = Streaming.format_chunk "test data" in
    assert_contains chunk "9" "should have length in hex";
    assert_contains chunk "test data" "should contain data"
  );

  test "end chunk" (fun () ->
    let chunk = Streaming.end_chunk in
    assert_eq "0\r\n\r\n" chunk "end chunk should be zero length"
  );

  test "SSE event format" (fun () ->
    let event = Streaming.sse_event ~event_type:"shell" ~data:"<html>" in
    assert_contains event "event: shell" "should have event type";
    assert_contains event "data:" "should have data field"
  );

  test "progressive marker" (fun () ->
    let marker = Streaming.progressive_marker ~component_id:"c1" ~priority:Streaming.Visible in
    assert_contains marker "solid-hydrate" "should have hydrate tag";
    assert_contains marker "visible" "should have priority"
  );

  test "progressive script" (fun () ->
    let script = Streaming.progressive_script in
    assert_contains script "__solidHydrate" "should define hydrate function";
    assert_contains script "IntersectionObserver" "should use intersection observer"
  );

  test "priority to string - immediate" (fun () ->
    let str = Streaming.priority_to_string Streaming.Immediate in
    assert_eq "immediate" str "immediate priority"
  );

  test "priority to string - idle" (fun () ->
    let str = Streaming.priority_to_string Streaming.Idle in
    assert_eq "idle" str "idle priority"
  );

  test "out of order placeholder" (fun () ->
    let placeholder = Streaming.ooo_placeholder ~id:"ooo1" ~fallback:"Loading..." in
    assert_contains placeholder "template" "should use template tag"
  );

  test "out of order replacement" (fun () ->
    let replacement = Streaming.ooo_replacement ~id:"ooo1" ~content:"<div>Done</div>" in
    assert_contains replacement "script" "should be a script"
  );

  test "error boundary fallback" (fun () ->
    let fallback = Streaming.error_boundary_fallback ~boundary_id:"eb1" ~error_message:"Test error" in
    assert_contains fallback "error-boundary" "should have error boundary class";
    assert_contains fallback "Test error" "should contain error message"
  );

  test "error recovery script" (fun () ->
    let script = Streaming.error_recovery_script ~boundary_id:"eb1" in
    assert_contains script "__solidErrorBoundary" "should set error boundary flag"
  )

(* ===========================================
   Handler Tests
   =========================================== *)

let test_handler () =
  Printf.printf "\n🎛️ Handler Tests:\n";

  test "default handler options" (fun () ->
    let opts = Handler.default_options in
    assert_true (Option.is_none opts.ssr_engine) "default should have no SSR engine";
    assert_eq_bool false opts.dev_mode "dev mode should be false by default"
  );

  test "is_vite_request - tsx file" (fun () ->
    assert_true (Handler.is_vite_request "/src/App.tsx") "should detect .tsx"
  );

  test "is_vite_request - css file" (fun () ->
    assert_true (Handler.is_vite_request "/styles/main.css") "should detect .css"
  );

  test "is_vite_request - @ prefix" (fun () ->
    assert_true (Handler.is_vite_request "@vite/client") "should detect @ prefix"
  );

  test "is_vite_request - regular path" (fun () ->
    assert_true (not (Handler.is_vite_request "/api/users")) "should not detect regular path"
  );

  test "routes creates list" (fun () ->
    let routes = Handler.routes ~options:Handler.default_options in
    assert_true (List.length routes > 0) "should create routes"
  );

  test "data_routes creates list" (fun () ->
    let loaders = [
      ("/users", fun ~url:_ ~params:_ -> Router.Data (`List []));
    ] in
    let routes = Handler.data_routes ~loaders in
    assert_eq_int 1 (List.length routes) "should create one data route"
  )

(* ===========================================
   API Facade Tests
   =========================================== *)

let test_facade () =
  Printf.printf "\n🎭 Facade Tests:\n";

  test "config is available" (fun () ->
    let config = Kirin_solid.config ~bundle:"test.js" in
    assert_eq "test.js" config.bundle "config should work"
  );

  test "create_engine builds engine" (fun () ->
    let config = Kirin_solid.config ~bundle:"test.js" in
    let engine = Kirin_solid.create_engine config in
    let stats = Kirin_solid.stats engine in
    assert_eq_int 4 stats.workers_total "engine should have workers"
  );

  test "meta helper" (fun () ->
    let builder = Kirin_solid.meta () in
    let _ = builder in
    ()
  );

  test "route helper" (fun () ->
    let route = Kirin_solid.route ~path:"/" ~component:"./Index.tsx" () in
    assert_eq "/" route.path "route should work"
  );

  test "spa_shell helper" (fun () ->
    let html = Kirin_solid.spa_shell ~title:"Test" ~entry_script:"/app.js" () in
    assert_contains html "Test" "spa_shell should work"
  );

  test "ssr_shell helper" (fun () ->
    let html = Kirin_solid.ssr_shell ~title:"Test" ~entry_script:"/app.js" ~ssr_html:"<div/>" () in
    assert_contains html "Test" "ssr_shell should work"
  )

(* ===========================================
   Main
   =========================================== *)

let () =
  Printf.printf "🧪 Solid.js SSR Tests\n";
  Printf.printf "======================\n";

  test_meta ();
  test_data ();
  test_hydrate ();
  test_protocol ();
  test_worker ();
  test_ssr ();
  test_router ();
  test_streaming ();
  test_handler ();
  test_facade ();

  Printf.printf "\n======================\n";
  Printf.printf "Total: %d tests, %d passed, %d failed\n"
    !test_count !pass_count !fail_count;

  if !fail_count > 0 then exit 1
