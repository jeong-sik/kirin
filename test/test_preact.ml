(** Preact Integration Tests *)

open Alcotest

(** {1 Test Utilities} *)

let string_contains ~sub s =
  try
    let _ = Str.search_forward (Str.regexp_string sub) s 0 in true
  with Not_found -> false

(** {1 Signals Tests} *)

let test_signal_creation () =
  let s = Kirin_preact.Signals.signal "count" (`Int 0) in
  check string "signal name" "count" (Kirin_preact.Signals.name s);
  check bool "not computed" false (Kirin_preact.Signals.is_computed s);
  check bool "not readonly" false (Kirin_preact.Signals.is_readonly s)

let test_computed_signal () =
  let c = Kirin_preact.Signals.computed
    ~name:"double"
    ~dependencies:["count"]
    "count.value * 2" in
  check bool "computed flag" true (Kirin_preact.Signals.is_computed c.signal);
  check bool "readonly flag" true (Kirin_preact.Signals.is_readonly c.signal)

let test_signal_declaration () =
  let s = Kirin_preact.Signals.signal "count" (`Int 42) in
  let decl = Kirin_preact.Signals.to_declaration s in
  check bool "has signal call" true (string_contains ~sub:"signal(42)" decl);
  check bool "has const" true (string_contains ~sub:"const count" decl)

let test_effect_creation () =
  let e = Kirin_preact.Signals.make_effect ~cleanup:true ~name:"logger" ["count"; "name"] in
  check string "effect name" "logger" e.effect_name;
  check int "deps count" 2 (List.length e.effect_dependencies);
  check bool "cleanup enabled" true e.effect_cleanup

let test_effect_code () =
  let e = Kirin_preact.Signals.make_effect ~cleanup:false ~name:"log" ["count"] in
  let code = Kirin_preact.Signals.effect_to_code e in
  check bool "has effect" true (string_contains ~sub:"effect(" code)

let test_batch_operation () =
  let b = Kirin_preact.Signals.batch [("count", `Int 10); ("name", `String "test")] in
  let code = Kirin_preact.Signals.batch_to_code b in
  check bool "has batch" true (string_contains ~sub:"batch(" code);
  check bool "has count" true (string_contains ~sub:"count.value" code)

let test_import_generation () =
  let import = Kirin_preact.Signals.generate_import [
    Kirin_preact.Signals.Signal;
    Kirin_preact.Signals.Computed;
    Kirin_preact.Signals.Effect;
  ] in
  check bool "has preact/signals" true (string_contains ~sub:"@preact/signals" import);
  check bool "has signal" true (string_contains ~sub:"signal" import);
  check bool "has computed" true (string_contains ~sub:"computed" import)

let test_signal_typescript () =
  let s = Kirin_preact.Signals.signal ~readonly:true "count" (`Int 0) in
  let ts = Kirin_preact.Signals.signal_to_ts s in
  check bool "has ReadonlySignal" true (string_contains ~sub:"ReadonlySignal<number>" ts)

let signals_tests = [
  "signal creation", `Quick, test_signal_creation;
  "computed signal", `Quick, test_computed_signal;
  "signal declaration", `Quick, test_signal_declaration;
  "effect creation", `Quick, test_effect_creation;
  "effect code", `Quick, test_effect_code;
  "batch operation", `Quick, test_batch_operation;
  "import generation", `Quick, test_import_generation;
  "signal typescript", `Quick, test_signal_typescript;
]

(** {1 Compat Tests} *)

let test_compat_config () =
  let c = Kirin_preact.Compat.default_config in
  check bool "enabled" true c.enabled;
  check bool "strict mode" true c.strict_mode;
  check bool "hooks enabled" true c.hooks_enabled

let test_vite_aliases () =
  let aliases = Kirin_preact.Compat.vite_aliases () in
  check bool "has react" true (string_contains ~sub:"'react': 'preact/compat'" aliases);
  check bool "has react-dom" true (string_contains ~sub:"'react-dom': 'preact/compat'" aliases)

let test_webpack_aliases () =
  let aliases = Kirin_preact.Compat.webpack_aliases () in
  check bool "has resolve" true (string_contains ~sub:"resolve:" aliases);
  check bool "has alias" true (string_contains ~sub:"alias:" aliases)

let test_library_compat () =
  let c = Kirin_preact.Compat.find_compat "react-query" in
  check bool "found" true (Option.is_some c);
  match c with
  | Some compat ->
    check string "level" "aliased" (Kirin_preact.Compat.level_to_string compat.level)
  | None -> fail "Expected to find react-query"

let test_hook_import () =
  let import = Kirin_preact.Compat.hook_import [
    Kirin_preact.Compat.UseState;
    Kirin_preact.Compat.UseEffect;
  ] in
  check bool "has preact/hooks" true (string_contains ~sub:"preact/hooks" import);
  check bool "has useState" true (string_contains ~sub:"useState" import)

let test_standard_aliases () =
  let aliases = Kirin_preact.Compat.standard_aliases in
  check bool "has aliases" true (List.length aliases >= 4)

let compat_tests = [
  "compat config", `Quick, test_compat_config;
  "vite aliases", `Quick, test_vite_aliases;
  "webpack aliases", `Quick, test_webpack_aliases;
  "library compat", `Quick, test_library_compat;
  "hook import", `Quick, test_hook_import;
  "standard aliases", `Quick, test_standard_aliases;
]

(** {1 Manifest Tests} *)

let test_manifest_parse () =
  let json = `Assoc [
    ("index.html", `Assoc [
      ("file", `String "assets/index.abc123.js");
      ("isEntry", `Bool true);
      ("css", `List [`String "assets/style.css"]);
    ])
  ] in
  let m = Kirin_preact.Manifest.parse json in
  check int "entry count" 1 (List.length m.entries)

let test_manifest_resolve () =
  let json = `Assoc [
    ("index.html", `Assoc [
      ("file", `String "assets/index.js");
      ("isEntry", `Bool true);
    ])
  ] in
  let m = Kirin_preact.Manifest.parse json in
  let entry = Kirin_preact.Manifest.resolve m "index.html" in
  check bool "found" true (Option.is_some entry)

let test_manifest_css () =
  let json = `Assoc [
    ("app.tsx", `Assoc [
      ("file", `String "assets/app.js");
      ("css", `List [`String "assets/app.css"; `String "assets/global.css"]);
    ])
  ] in
  let m = Kirin_preact.Manifest.parse ~base_url:"/static/" json in
  let css = Kirin_preact.Manifest.css_for m "app.tsx" in
  check int "css count" 2 (List.length css);
  check bool "has base url" true (string_contains ~sub:"/static/" (List.hd css))

let test_manifest_preload () =
  let json = `Assoc [
    ("main.tsx", `Assoc [
      ("file", `String "assets/main.js");
      ("isEntry", `Bool true);
    ])
  ] in
  let m = Kirin_preact.Manifest.parse json in
  let hint = Kirin_preact.Manifest.preload_hint m "main.tsx" in
  check bool "has modulepreload" true (string_contains ~sub:"modulepreload" hint)

let test_manifest_script_tag () =
  let json = `Assoc [
    ("entry.tsx", `Assoc [
      ("file", `String "assets/entry.js");
    ])
  ] in
  let m = Kirin_preact.Manifest.parse json in
  let tag = Kirin_preact.Manifest.script_tag m "entry.tsx" in
  check bool "has script" true (string_contains ~sub:"<script" tag);
  check bool "has type module" true (string_contains ~sub:"type=\"module\"" tag)

let manifest_tests = [
  "manifest parse", `Quick, test_manifest_parse;
  "manifest resolve", `Quick, test_manifest_resolve;
  "manifest css", `Quick, test_manifest_css;
  "manifest preload", `Quick, test_manifest_preload;
  "manifest script tag", `Quick, test_manifest_script_tag;
]

(** {1 Hydrate Tests} *)

let test_hydrate_render () =
  let options = { Kirin_preact.Hydrate.default_options with
    title = "Test App";
    root_id = "root";
  } in
  let html = Kirin_preact.Hydrate.render options in
  check bool "has doctype" true (string_contains ~sub:"<!DOCTYPE html>" html);
  check bool "has title" true (string_contains ~sub:"<title>Test App</title>" html);
  check bool "has root div" true (string_contains ~sub:"id=\"root\"" html)

let test_hydrate_initial_data () =
  let options = { Kirin_preact.Hydrate.default_options with
    title = "App";
    initial_data = Some (`Assoc [("user", `String "test")]);
  } in
  let html = Kirin_preact.Hydrate.render options in
  check bool "has initial data" true (string_contains ~sub:"__INITIAL_DATA__" html)

let test_hydrate_signals_data () =
  let options = { Kirin_preact.Hydrate.default_options with
    title = "App";
    signals_data = [("count", `Int 5)];
  } in
  let html = Kirin_preact.Hydrate.render options in
  check bool "has signals data" true (string_contains ~sub:"__PREACT_SIGNALS__" html)

let test_hydrate_meta_tags () =
  let options = { Kirin_preact.Hydrate.default_options with
    title = "App";
    meta = [("description", "Test"); ("og:title", "Open Graph")];
  } in
  let html = Kirin_preact.Hydrate.render options in
  check bool "has meta name" true (string_contains ~sub:"name=\"description\"" html);
  check bool "has meta property" true (string_contains ~sub:"property=\"og:title\"" html)

let test_hydrate_client_entry () =
  let code = Kirin_preact.Hydrate.client_entry_native ~component:"./App" ~root_id:"app" in
  check bool "has preact import" true (string_contains ~sub:"from 'preact'" code);
  check bool "has hydrate" true (string_contains ~sub:"hydrate(" code)

let test_hydrate_mode () =
  check string "native mode" "native" (Kirin_preact.Hydrate.mode_to_string Kirin_preact.Hydrate.Native);
  check string "compat mode" "compat" (Kirin_preact.Hydrate.mode_to_string Kirin_preact.Hydrate.Compat)

let hydrate_tests = [
  "hydrate render", `Quick, test_hydrate_render;
  "hydrate initial data", `Quick, test_hydrate_initial_data;
  "hydrate signals data", `Quick, test_hydrate_signals_data;
  "hydrate meta tags", `Quick, test_hydrate_meta_tags;
  "hydrate client entry", `Quick, test_hydrate_client_entry;
  "hydrate mode", `Quick, test_hydrate_mode;
]

(** {1 Protocol Tests} *)

let test_protocol_request () =
  let req = Kirin_preact.Protocol.render_request ~url:"/test" () in
  check string "method" "render" req.method_;
  let encoded = Kirin_preact.Protocol.encode_request req in
  check bool "has jsonrpc" true (string_contains ~sub:"jsonrpc" encoded);
  check bool "has 2.0" true (string_contains ~sub:"2.0" encoded)

let test_protocol_health_request () =
  let req = Kirin_preact.Protocol.health_request () in
  check string "method" "health" req.method_

let test_protocol_decode_success () =
  let json = {|{"jsonrpc":"2.0","id":1,"result":{"html":"<div>Test</div>"}}|} in
  match Kirin_preact.Protocol.decode_response json with
  | Ok (Kirin_preact.Protocol.RpcSuccess s) ->
    check int "id" 1 s.success_id
  | _ -> fail "Expected success response"

let test_protocol_decode_error () =
  let json = {|{"jsonrpc":"2.0","id":1,"error":{"code":-32600,"message":"Invalid"}}|} in
  match Kirin_preact.Protocol.decode_response json with
  | Ok (Kirin_preact.Protocol.RpcError e) ->
    check int "code" (-32600) e.error.code
  | _ -> fail "Expected error response"

let test_protocol_extract_html () =
  let result = `Assoc [("html", `String "<div>Content</div>")] in
  match Kirin_preact.Protocol.extract_html result with
  | Some html -> check string "html" "<div>Content</div>" html
  | None -> fail "Expected html"

let test_protocol_error_codes () =
  check int "parse error" (-32700) Kirin_preact.Protocol.parse_error;
  check int "render error" (-32000) Kirin_preact.Protocol.render_error

let protocol_tests = [
  "protocol request", `Quick, test_protocol_request;
  "protocol health request", `Quick, test_protocol_health_request;
  "protocol decode success", `Quick, test_protocol_decode_success;
  "protocol decode error", `Quick, test_protocol_decode_error;
  "protocol extract html", `Quick, test_protocol_extract_html;
  "protocol error codes", `Quick, test_protocol_error_codes;
]

(** {1 SSR Tests} *)

let test_ssr_config () =
  let c = Kirin_preact.Ssr.default_config in
  check int "pool size" 4 c.pool_size;
  check bool "signals ssr" true c.signals_ssr

let test_ssr_create () =
  let engine = Kirin_preact.Ssr.create Kirin_preact.Ssr.default_config in
  check bool "healthy" true (Kirin_preact.Ssr.is_healthy engine)

let test_ssr_render () =
  let engine = Kirin_preact.Ssr.create Kirin_preact.Ssr.default_config in
  match Kirin_preact.Ssr.render engine ~url:"/test" () with
  | Ok html ->
    check bool "has content" true (String.length html > 0);
    check bool "has preact" true (string_contains ~sub:"Preact SSR" html)
  | Error _ -> fail "Render should succeed"

let test_ssr_render_with_fallback () =
  let engine = Kirin_preact.Ssr.create Kirin_preact.Ssr.default_config in
  let html = Kirin_preact.Ssr.render_with_fallback engine ~url:"/test" ~fallback:"<div>Fallback</div>" in
  check bool "has content" true (String.length html > 0)

let test_ssr_stats () =
  let engine = Kirin_preact.Ssr.create Kirin_preact.Ssr.default_config in
  let _ = Kirin_preact.Ssr.render engine ~url:"/test" () in
  let stats = Kirin_preact.Ssr.stats engine in
  check int "total renders" 1 stats.stat_total_renders;
  check int "active workers" 4 stats.stat_active_workers

let test_ssr_shutdown () =
  let engine = Kirin_preact.Ssr.create Kirin_preact.Ssr.default_config in
  Kirin_preact.Ssr.shutdown engine;
  check bool "not healthy" false (Kirin_preact.Ssr.is_healthy engine)

let test_ssr_dehydrate () =
  let signals = `Assoc [("count", `Int 5)] in
  let script = Kirin_preact.Ssr.dehydrate_signals signals in
  check bool "has script" true (string_contains ~sub:"<script>" script);
  check bool "has signals" true (string_contains ~sub:"__PREACT_SIGNALS__" script)

let ssr_tests = [
  "ssr config", `Quick, test_ssr_config;
  "ssr create", `Quick, test_ssr_create;
  "ssr render", `Quick, test_ssr_render;
  "ssr render with fallback", `Quick, test_ssr_render_with_fallback;
  "ssr stats", `Quick, test_ssr_stats;
  "ssr shutdown", `Quick, test_ssr_shutdown;
  "ssr dehydrate", `Quick, test_ssr_dehydrate;
]

(** {1 Vite Tests} *)

let test_vite_config () =
  let c = Kirin_preact.Vite.default_config in
  check int "port" 5173 c.port;
  check bool "hmr" true c.hmr

let test_vite_is_dev () =
  (* Default should be false in test environment *)
  let is_dev = Kirin_preact.Vite.is_dev () in
  check bool "is dev" false is_dev

let test_vite_dev_url () =
  let c = Kirin_preact.Vite.default_config in
  let url = Kirin_preact.Vite.dev_url c in
  check bool "has localhost" true (string_contains ~sub:"localhost" url);
  check bool "has port" true (string_contains ~sub:"5173" url)

let test_vite_config_generation () =
  let c = Kirin_preact.Vite.default_config in
  let config = Kirin_preact.Vite.generate_vite_config c in
  check bool "has preact" true (string_contains ~sub:"preact()" config);
  check bool "has defineConfig" true (string_contains ~sub:"defineConfig" config)

let test_vite_hmr_script () =
  let c = { Kirin_preact.Vite.default_config with hmr = true } in
  let script = Kirin_preact.Vite.hmr_script c in
  check bool "has vite client" true (string_contains ~sub:"@vite/client" script)

let test_vite_refresh_preamble () =
  let c = Kirin_preact.Vite.default_config in
  (* Not in dev mode, so empty *)
  let preamble = Kirin_preact.Vite.refresh_preamble c in
  check string "empty in prod" "" preamble

let vite_tests = [
  "vite config", `Quick, test_vite_config;
  "vite is dev", `Quick, test_vite_is_dev;
  "vite dev url", `Quick, test_vite_dev_url;
  "vite config generation", `Quick, test_vite_config_generation;
  "vite hmr script", `Quick, test_vite_hmr_script;
  "vite refresh preamble", `Quick, test_vite_refresh_preamble;
]

(** {1 Facade Tests} *)

let test_facade_signal () =
  let s = Kirin_preact.signal "test" (`Int 0) in
  check string "name" "test" (Kirin_preact.Signals.name s)

let test_facade_computed () =
  let c = Kirin_preact.computed ~name:"double" ~dependencies:["count"] "count * 2" in
  check bool "is computed" true (Kirin_preact.Signals.is_computed c.signal)

let test_facade_compat_aliases () =
  let aliases = Kirin_preact.compat_aliases () in
  check bool "has content" true (String.length aliases > 0)

let test_facade_library_check () =
  let compat = Kirin_preact.check_library_compat "zustand" in
  check bool "found zustand" true (Option.is_some compat)

let facade_tests = [
  "facade signal", `Quick, test_facade_signal;
  "facade computed", `Quick, test_facade_computed;
  "facade compat aliases", `Quick, test_facade_compat_aliases;
  "facade library check", `Quick, test_facade_library_check;
]

(** {1 Run Tests} *)

let () =
  run "Preact" [
    "Signals", signals_tests;
    "Compat", compat_tests;
    "Manifest", manifest_tests;
    "Hydrate", hydrate_tests;
    "Protocol", protocol_tests;
    "SSR", ssr_tests;
    "Vite", vite_tests;
    "Facade", facade_tests;
  ]
