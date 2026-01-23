(** React Module Tests (Phase 18) *)

open Kirin_react

(* ========== Manifest Tests ========== *)

let manifest_tests = [
  "parse empty manifest", `Quick, (fun () ->
    let manifest = Manifest.parse (`Assoc []) in
    Alcotest.(check int) "empty" 0 (List.length manifest)
  );

  "parse single entry", `Quick, (fun () ->
    let json = `Assoc [
      ("index.html", `Assoc [
        ("file", `String "assets/index-abc123.js");
        ("isEntry", `Bool true);
        ("css", `List [`String "assets/index.css"]);
      ])
    ] in
    let manifest = Manifest.parse json in
    Alcotest.(check int) "one entry" 1 (List.length manifest);
    match Manifest.find manifest "index.html" with
    | Some entry ->
      Alcotest.(check string) "file" "assets/index-abc123.js" entry.file;
      Alcotest.(check bool) "is_entry" true entry.is_entry
    | None -> Alcotest.fail "Entry not found"
  );

  "resolve file path", `Quick, (fun () ->
    let json = `Assoc [
      ("src/main.tsx", `Assoc [
        ("file", `String "assets/main-xyz789.js");
      ])
    ] in
    let manifest = Manifest.parse json in
    match Manifest.resolve manifest "src/main.tsx" with
    | Some file -> Alcotest.(check string) "resolved" "assets/main-xyz789.js" file
    | None -> Alcotest.fail "Should resolve"
  );

  "css_for entry", `Quick, (fun () ->
    let json = `Assoc [
      ("app.tsx", `Assoc [
        ("file", `String "app.js");
        ("css", `List [`String "app.css"; `String "vendor.css"]);
      ])
    ] in
    let manifest = Manifest.parse json in
    let css = Manifest.css_for manifest "app.tsx" in
    Alcotest.(check int) "css count" 2 (List.length css)
  );

  "all_css from manifest", `Quick, (fun () ->
    let json = `Assoc [
      ("a.tsx", `Assoc [("file", `String "a.js"); ("css", `List [`String "a.css"])]);
      ("b.tsx", `Assoc [("file", `String "b.js"); ("css", `List [`String "b.css"])]);
    ] in
    let manifest = Manifest.parse json in
    let all = Manifest.all_css manifest in
    Alcotest.(check int) "all css" 2 (List.length all)
  );

  "entries filter", `Quick, (fun () ->
    let json = `Assoc [
      ("main.tsx", `Assoc [("file", `String "main.js"); ("isEntry", `Bool true)]);
      ("util.ts", `Assoc [("file", `String "util.js"); ("isEntry", `Bool false)]);
    ] in
    let manifest = Manifest.parse json in
    let entries = Manifest.entries manifest in
    Alcotest.(check int) "entry count" 1 (List.length entries)
  );

  "parse_string error", `Quick, (fun () ->
    match Manifest.parse_string "invalid json" with
    | Error _ -> ()
    | Ok _ -> Alcotest.fail "Should fail on invalid JSON"
  );

  "mem check", `Quick, (fun () ->
    let json = `Assoc [("test.tsx", `Assoc [("file", `String "test.js")])] in
    let manifest = Manifest.parse json in
    Alcotest.(check bool) "exists" true (Manifest.mem manifest "test.tsx");
    Alcotest.(check bool) "not exists" false (Manifest.mem manifest "other.tsx")
  );
]

(* ========== Meta Tests ========== *)

let meta_tests = [
  "title tag", `Quick, (fun () ->
    let tag = Meta.title "My App" in
    Alcotest.(check bool) "contains title" true
      (String.length tag > 0 && tag = "<title>My App</title>")
  );

  "description tag", `Quick, (fun () ->
    let tag = Meta.description "A great app" in
    Alcotest.(check bool) "contains meta" true
      (try let _ = Str.search_forward (Str.regexp_string "description") tag 0 in true
       with Not_found -> false)
  );

  "og tag", `Quick, (fun () ->
    let tag = Meta.og ~property:"title" ~content:"Test" in
    Alcotest.(check bool) "contains og:title" true
      (try let _ = Str.search_forward (Str.regexp_string "og:title") tag 0 in true
       with Not_found -> false)
  );

  "twitter tag", `Quick, (fun () ->
    let tag = Meta.twitter ~name:"card" ~content:"summary" in
    Alcotest.(check bool) "contains twitter:card" true
      (try let _ = Str.search_forward (Str.regexp_string "twitter:card") tag 0 in true
       with Not_found -> false)
  );

  "canonical link", `Quick, (fun () ->
    let tag = Meta.canonical "https://example.com" in
    Alcotest.(check bool) "contains canonical" true
      (try let _ = Str.search_forward (Str.regexp_string "canonical") tag 0 in true
       with Not_found -> false)
  );

  "escape html", `Quick, (fun () ->
    let tag = Meta.title "<script>alert('xss')</script>" in
    Alcotest.(check bool) "escaped" true
      (not (try let _ = Str.search_forward (Str.regexp_string "<script>") tag 0 in true
            with Not_found -> false))
  );
]

(* ========== Assets Tests ========== *)

let assets_tests = [
  "url resolution", `Quick, (fun () ->
    let json = `Assoc [("main.tsx", `Assoc [("file", `String "assets/main-abc.js")])] in
    let manifest = Manifest.parse json in
    let url = Assets.url ~manifest "main.tsx" in
    Alcotest.(check string) "url" "/assets/main-abc.js" url
  );

  "url with base_url", `Quick, (fun () ->
    let json = `Assoc [("main.tsx", `Assoc [("file", `String "main.js")])] in
    let manifest = Manifest.parse json in
    let url = Assets.url ~base_url:"/static" ~manifest "main.tsx" in
    Alcotest.(check string) "url" "/static/main.js" url
  );

  "script_tag generation", `Quick, (fun () ->
    let json = `Assoc [("app.tsx", `Assoc [("file", `String "app.js")])] in
    let manifest = Manifest.parse json in
    let tag = Assets.script_tag ~manifest "app.tsx" in
    Alcotest.(check bool) "has script" true
      (try let _ = Str.search_forward (Str.regexp_string "<script") tag 0 in true
       with Not_found -> false);
    Alcotest.(check bool) "has module" true
      (try let _ = Str.search_forward (Str.regexp_string "module") tag 0 in true
       with Not_found -> false)
  );

  "link_tag generation", `Quick, (fun () ->
    let json = `Assoc [("style.css", `Assoc [("file", `String "style-abc.css")])] in
    let manifest = Manifest.parse json in
    let tag = Assets.link_tag ~manifest "style.css" in
    Alcotest.(check bool) "has link" true
      (try let _ = Str.search_forward (Str.regexp_string "stylesheet") tag 0 in true
       with Not_found -> false)
  );

  "css_tags multiple", `Quick, (fun () ->
    let json = `Assoc [
      ("app.tsx", `Assoc [
        ("file", `String "app.js");
        ("css", `List [`String "a.css"; `String "b.css"])
      ])
    ] in
    let manifest = Manifest.parse json in
    let tags = Assets.css_tags ~manifest "app.tsx" in
    let count = List.length (String.split_on_char '\n' tags) in
    Alcotest.(check int) "two link tags" 2 count
  );

  "fallback for missing", `Quick, (fun () ->
    let manifest = [] in
    let url = Assets.url ~manifest "missing.tsx" in
    Alcotest.(check string) "fallback" "/missing.tsx" url
  );
]

(* ========== Vite Tests ========== *)

let vite_tests = [
  "is_dev with NODE_ENV", `Quick, (fun () ->
    (* This test depends on environment *)
    let _ = Vite.is_dev () in
    ()  (* Just verify it doesn't crash *)
  );

  "dev_server_url", `Quick, (fun () ->
    let url = Vite.dev_server_url "/test" in
    Alcotest.(check bool) "has localhost" true
      (try let _ = Str.search_forward (Str.regexp_string "localhost") url 0 in true
       with Not_found -> false);
    Alcotest.(check bool) "has port" true
      (try let _ = Str.search_forward (Str.regexp_string "5173") url 0 in true
       with Not_found -> false)
  );

  "dev_script_tag", `Quick, (fun () ->
    let tag = Vite.dev_script_tag ~entry:"main.tsx" () in
    Alcotest.(check bool) "has script" true
      (try let _ = Str.search_forward (Str.regexp_string "<script") tag 0 in true
       with Not_found -> false)
  );

  "hmr_script", `Quick, (fun () ->
    let tag = Vite.hmr_script () in
    Alcotest.(check bool) "has vite client" true
      (try let _ = Str.search_forward (Str.regexp_string "@vite/client") tag 0 in true
       with Not_found -> false)
  );
]

(* ========== Data Tests ========== *)

let data_tests = [
  "serialize json", `Quick, (fun () ->
    let json = `Assoc [("name", `String "test")] in
    let s = Data.serialize json in
    Alcotest.(check bool) "has content" true (String.length s > 0)
  );

  "xss escape", `Quick, (fun () ->
    let json = `Assoc [("html", `String "</script>")] in
    let s = Data.serialize json in
    Alcotest.(check bool) "escaped script" true
      (not (try let _ = Str.search_forward (Str.regexp_string "</script>") s 0 in true
            with Not_found -> false))
  );

  "script_tag generation", `Quick, (fun () ->
    let json = `Assoc [("user", `String "alice")] in
    let tag = Data.script_tag json in
    Alcotest.(check bool) "has window" true
      (try let _ = Str.search_forward (Str.regexp_string "window.__INITIAL_DATA__") tag 0 in true
       with Not_found -> false)
  );

  "named_script_tag", `Quick, (fun () ->
    let json = `Assoc [] in
    let tag = Data.named_script_tag ~name:"__MY_DATA__" json in
    Alcotest.(check bool) "has custom name" true
      (try let _ = Str.search_forward (Str.regexp_string "__MY_DATA__") tag 0 in true
       with Not_found -> false)
  );
]

(* ========== Hydrate Tests ========== *)

let hydrate_tests = [
  "render default options", `Quick, (fun () ->
    let html = Hydrate.render Hydrate.default_options in
    Alcotest.(check bool) "has doctype" true
      (try let _ = Str.search_forward (Str.regexp_string "<!DOCTYPE html>") html 0 in true
       with Not_found -> false);
    Alcotest.(check bool) "has root div" true
      (try let _ = Str.search_forward (Str.regexp_string {|id="root"|}) html 0 in true
       with Not_found -> false)
  );

  "render with title", `Quick, (fun () ->
    let opts = { Hydrate.default_options with title = "My App" } in
    let html = Hydrate.render opts in
    Alcotest.(check bool) "has title" true
      (try let _ = Str.search_forward (Str.regexp_string "<title>My App</title>") html 0 in true
       with Not_found -> false)
  );

  "render with initial_data", `Quick, (fun () ->
    let opts = {
      Hydrate.default_options with
      initial_data = Some (`Assoc [("user", `String "alice")])
    } in
    let html = Hydrate.render opts in
    Alcotest.(check bool) "has initial data" true
      (try let _ = Str.search_forward (Str.regexp_string "__INITIAL_DATA__") html 0 in true
       with Not_found -> false)
  );

  "render with scripts", `Quick, (fun () ->
    let opts = { Hydrate.default_options with scripts = ["/app.js"] } in
    let html = Hydrate.render opts in
    Alcotest.(check bool) "has script tag" true
      (try let _ = Str.search_forward (Str.regexp_string "/app.js") html 0 in true
       with Not_found -> false)
  );

  "render with styles", `Quick, (fun () ->
    let opts = { Hydrate.default_options with styles = ["/style.css"] } in
    let html = Hydrate.render opts in
    Alcotest.(check bool) "has link tag" true
      (try let _ = Str.search_forward (Str.regexp_string "/style.css") html 0 in true
       with Not_found -> false)
  );

  "custom root_id", `Quick, (fun () ->
    let opts = { Hydrate.default_options with root_id = "app" } in
    let html = Hydrate.render opts in
    Alcotest.(check bool) "has app div" true
      (try let _ = Str.search_forward (Str.regexp_string {|id="app"|}) html 0 in true
       with Not_found -> false)
  );

  "builder pattern", `Quick, (fun () ->
    let html =
      Hydrate.Builder.create ()
      |> Hydrate.Builder.title "Builder Test"
      |> Hydrate.Builder.render
    in
    Alcotest.(check bool) "has title" true
      (try let _ = Str.search_forward (Str.regexp_string "Builder Test") html 0 in true
       with Not_found -> false)
  );

  "shell convenience", `Quick, (fun () ->
    let manifest = [] in
    let html = Hydrate.shell ~title:"Shell" ~manifest ~entry:"main.js" () in
    Alcotest.(check bool) "has doctype" true
      (try let _ = Str.search_forward (Str.regexp_string "<!DOCTYPE") html 0 in true
       with Not_found -> false)
  );
]

(* ========== Protocol Tests ========== *)

let protocol_tests = [
  "encode request", `Quick, (fun () ->
    let req = Protocol.render_request ~id:1 ~url:"/test" () in
    let s = Protocol.encode_request req in
    Alcotest.(check bool) "has jsonrpc" true
      (try let _ = Str.search_forward (Str.regexp_string "jsonrpc") s 0 in true
       with Not_found -> false);
    Alcotest.(check bool) "has render method" true
      (try let _ = Str.search_forward (Str.regexp_string "render") s 0 in true
       with Not_found -> false)
  );

  "decode success response", `Quick, (fun () ->
    let json = {|{"jsonrpc":"2.0","id":1,"result":{"html":"<div>test</div>"}}|} in
    match Protocol.decode_response json with
    | Ok (Protocol.Success { id; _ }) ->
      Alcotest.(check int) "id" 1 id
    | _ -> Alcotest.fail "Should decode success"
  );

  "decode error response", `Quick, (fun () ->
    let json = {|{"jsonrpc":"2.0","id":1,"error":{"code":-32600,"message":"Invalid"}}|} in
    match Protocol.decode_response json with
    | Ok (Protocol.Error { code; message; _ }) ->
      Alcotest.(check int) "code" (-32600) code;
      Alcotest.(check string) "message" "Invalid" message
    | _ -> Alcotest.fail "Should decode error"
  );

  "extract html", `Quick, (fun () ->
    let response = Protocol.Success {
      id = 1;
      result = `Assoc [("html", `String "<div>Hello</div>")]
    } in
    match Protocol.extract_html response with
    | Ok html -> Alcotest.(check string) "html" "<div>Hello</div>" html
    | Error _ -> Alcotest.fail "Should extract html"
  );

  "next_id increments", `Quick, (fun () ->
    let id1 = Protocol.next_id () in
    let id2 = Protocol.next_id () in
    Alcotest.(check bool) "increments" true (id2 > id1)
  );

  "health request", `Quick, (fun () ->
    let req = Protocol.health_request ~id:42 in
    Alcotest.(check string) "method" "health" req.method_
  );
]

(* ========== Worker Tests ========== *)

let worker_tests = [
  "status_to_string", `Quick, (fun () ->
    Alcotest.(check string) "idle" "idle" (Worker.status_to_string Worker.Idle);
    Alcotest.(check string) "busy" "busy" (Worker.status_to_string Worker.Busy);
    Alcotest.(check string) "dead" "dead" (Worker.status_to_string Worker.Dead)
  );

  "empty stats", `Quick, (fun () ->
    let stats = Worker.empty_stats in
    Alcotest.(check int) "requests" 0 stats.requests_handled;
    Alcotest.(check int) "errors" 0 stats.errors
  );

  "default config", `Quick, (fun () ->
    let config = Worker.default_config in
    Alcotest.(check int) "timeout" 5000 config.timeout_ms;
    Alcotest.(check int) "memory limit" 200 config.memory_limit_mb
  );

  "render cache create", `Quick, (fun () ->
    let cache = Worker.Render_cache.create () in
    Alcotest.(check int) "empty" 0 (Worker.Render_cache.size cache)
  );

  "render cache set/get", `Quick, (fun () ->
    let cache = Worker.Render_cache.create () in
    Worker.Render_cache.set cache
      ~url:"/test" ~props:(`Assoc []) ~html:"<div>cached</div>" ~ttl_seconds:60.0;
    match Worker.Render_cache.get cache ~url:"/test" ~props:(`Assoc []) with
    | Some html -> Alcotest.(check string) "cached html" "<div>cached</div>" html
    | None -> Alcotest.fail "Should find cached"
  );

  "render cache clear", `Quick, (fun () ->
    let cache = Worker.Render_cache.create () in
    Worker.Render_cache.set cache ~url:"/a" ~props:(`Assoc []) ~html:"a" ~ttl_seconds:60.0;
    Worker.Render_cache.clear cache;
    Alcotest.(check int) "cleared" 0 (Worker.Render_cache.size cache)
  );
]

(* ========== SSR Tests ========== *)

let ssr_tests = [
  "default config", `Quick, (fun () ->
    let config = Ssr.default_config in
    Alcotest.(check int) "workers" 4 config.workers;
    Alcotest.(check (float 0.1)) "timeout" 5.0 config.timeout
  );

  "render cache create", `Quick, (fun () ->
    let cache = Worker.Render_cache.create () in
    Alcotest.(check int) "empty cache" 0 (Worker.Render_cache.size cache)
  );

  "render cache set/get", `Quick, (fun () ->
    let cache = Worker.Render_cache.create () in
    Worker.Render_cache.set cache ~url:"/test" ~props:(`Assoc []) ~html:"<div>hello</div>" ~ttl_seconds:60.0;
    match Worker.Render_cache.get cache ~url:"/test" ~props:(`Assoc []) with
    | Some html -> Alcotest.(check string) "cached html" "<div>hello</div>" html
    | None -> Alcotest.fail "should be cached"
  );

  "render cache miss", `Quick, (fun () ->
    let cache = Worker.Render_cache.create () in
    match Worker.Render_cache.get cache ~url:"/missing" ~props:(`Assoc []) with
    | Some _ -> Alcotest.fail "should not be cached"
    | None -> ()
  );

  "render cache clear", `Quick, (fun () ->
    let cache = Worker.Render_cache.create () in
    Worker.Render_cache.set cache ~url:"/test" ~props:(`Assoc []) ~html:"<div>" ~ttl_seconds:60.0;
    Worker.Render_cache.clear cache;
    Alcotest.(check int) "cleared" 0 (Worker.Render_cache.size cache)
  );

  (* Note: Full SSR tests would require a running Node.js worker *)
]

(* ========== Streaming Tests ========== *)

let streaming_tests = [
  "create session", `Quick, (fun () ->
    let session = Streaming.create_session () in
    Alcotest.(check bool) "not complete" false session.is_complete
  );

  "html_start", `Quick, (fun () ->
    let html = Streaming.html_start ~title:"Test" ~head_extra:"" ~styles:[] in
    Alcotest.(check bool) "has doctype" true
      (try let _ = Str.search_forward (Str.regexp_string "<!DOCTYPE") html 0 in true
       with Not_found -> false)
  );

  "sse event format", `Quick, (fun () ->
    let event = Streaming.SSE.event ~name:"test" ~data:"hello" in
    Alcotest.(check bool) "has event" true
      (try let _ = Str.search_forward (Str.regexp_string "event: test") event 0 in true
       with Not_found -> false);
    Alcotest.(check bool) "has data" true
      (try let _ = Str.search_forward (Str.regexp_string "data: hello") event 0 in true
       with Not_found -> false)
  );

  "sse complete", `Quick, (fun () ->
    let event = Streaming.SSE.complete () in
    Alcotest.(check bool) "has complete event" true
      (try let _ = Str.search_forward (Str.regexp_string "complete") event 0 in true
       with Not_found -> false)
  );
]

(* ========== Integration Tests ========== *)

let integration_tests = [
  "kirin_react modules exported", `Quick, (fun () ->
    (* Verify all modules are accessible by using their functions *)
    let _ = Kirin_react.Manifest.parse in
    let _ = Kirin_react.Meta.title in
    let _ = Kirin_react.Assets.url in
    let _ = Kirin_react.Vite.is_dev in
    let _ = Kirin_react.Data.serialize in
    let _ = Kirin_react.Hydrate.render in
    let _ = Kirin_react.Protocol.encode_request in
    let _ = Kirin_react.Worker.Render_cache.create in
    let _ = Kirin_react.Ssr.default_config in
    let _ = Kirin_react.Streaming.create_session in
    ()
  );
]

let () =
  Alcotest.run "React" [
    ("Manifest", manifest_tests);
    ("Meta", meta_tests);
    ("Assets", assets_tests);
    ("Vite", vite_tests);
    ("Data", data_tests);
    ("Hydrate", hydrate_tests);
    ("Protocol", protocol_tests);
    ("Worker", worker_tests);
    ("SSR", ssr_tests);
    ("Streaming", streaming_tests);
    ("Integration", integration_tests);
  ]
