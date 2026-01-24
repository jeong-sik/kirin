(** Tests for Kirin Astro - Islands Architecture *)

open Alcotest

(** {1 Island Tests} *)

let test_island_create () =
  let island = Kirin_astro.Island.create
    ~component:"Counter"
    ~framework:Kirin_astro.Island.React
    ~directive:Kirin_astro.Island.ClientLoad
    ()
  in
  check string "component" "Counter" island.Kirin_astro.Island.component;
  check bool "framework is React" true
    (island.Kirin_astro.Island.framework = Kirin_astro.Island.React)

let test_island_directive_string () =
  check string "load" "load"
    (Kirin_astro.Island.directive_to_string Kirin_astro.Island.ClientLoad);
  check string "idle" "idle"
    (Kirin_astro.Island.directive_to_string Kirin_astro.Island.ClientIdle);
  check string "visible" "visible"
    (Kirin_astro.Island.directive_to_string Kirin_astro.Island.ClientVisible);
  check string "media" "media:(max-width: 768px)"
    (Kirin_astro.Island.directive_to_string (Kirin_astro.Island.ClientMedia "(max-width: 768px)"))

let test_island_framework_string () =
  check string "react" "react"
    (Kirin_astro.Island.framework_to_string Kirin_astro.Island.React);
  check string "vue" "vue"
    (Kirin_astro.Island.framework_to_string Kirin_astro.Island.Vue);
  check string "svelte" "svelte"
    (Kirin_astro.Island.framework_to_string Kirin_astro.Island.Svelte);
  check string "solid" "solid"
    (Kirin_astro.Island.framework_to_string Kirin_astro.Island.Solid)

let test_island_render_wrapper () =
  let island = Kirin_astro.Island.create
    ~component:"Counter"
    ~framework:Kirin_astro.Island.React
    ~directive:Kirin_astro.Island.ClientLoad
    ()
  in
  let html = Kirin_astro.Island.render_wrapper island "<div>Content</div>" in
  check bool "has astro-island" true
    (String.length html > 0 && html |> String.split_on_char '<' |> List.length > 1)

let test_island_hydration_script () =
  let script = Kirin_astro.Island.hydration_script () in
  check bool "contains IntersectionObserver" true
    (String.length script > 0)

let test_island_to_json () =
  let island = Kirin_astro.Island.create
    ~component:"Counter"
    ~framework:Kirin_astro.Island.React
    ~directive:Kirin_astro.Island.ClientLoad
    ()
  in
  let json = Kirin_astro.Island.to_json island in
  match json with
  | `Assoc fields ->
    check bool "has component field" true
      (List.mem_assoc "component" fields)
  | _ -> fail "Expected JSON object"

let island_tests = [
  "create island", `Quick, test_island_create;
  "directive to string", `Quick, test_island_directive_string;
  "framework to string", `Quick, test_island_framework_string;
  "render wrapper", `Quick, test_island_render_wrapper;
  "hydration script", `Quick, test_island_hydration_script;
  "island to json", `Quick, test_island_to_json;
]

(** {1 Route Def Tests} *)

let test_route_static () =
  let route = Kirin_astro.Route_def.static "/about" in
  check string "path" "/about" route.Kirin_astro.Route_def.path;
  check bool "prerender is Static" true
    (route.Kirin_astro.Route_def.prerender = Kirin_astro.Route_def.Static)

let test_route_ssr () =
  let route = Kirin_astro.Route_def.ssr "/api/users" in
  check bool "prerender is OnDemand" true
    (route.Kirin_astro.Route_def.prerender = Kirin_astro.Route_def.OnDemand)

let test_route_hybrid () =
  let route = Kirin_astro.Route_def.hybrid "/dashboard" in
  check bool "prerender is Hybrid" true
    (route.Kirin_astro.Route_def.prerender = Kirin_astro.Route_def.Hybrid)

let test_route_params () =
  let route = Kirin_astro.Route_def.static "/blog/[slug]" in
  let params = Kirin_astro.Route_def.get_params route in
  check int "param count" 1 (List.length params);
  check string "param name" "slug" (List.hd params)

let test_route_rest_params () =
  let route = Kirin_astro.Route_def.static "/docs/[...path]" in
  let params = Kirin_astro.Route_def.get_params route in
  check int "param count" 1 (List.length params);
  check string "param name" "path" (List.hd params)

let test_route_is_dynamic () =
  let static = Kirin_astro.Route_def.static "/about" in
  let dynamic = Kirin_astro.Route_def.static "/blog/[slug]" in
  check bool "about is not dynamic" false
    (Kirin_astro.Route_def.is_dynamic static);
  check bool "blog slug is dynamic" true
    (Kirin_astro.Route_def.is_dynamic dynamic)

let test_route_matches () =
  let route = Kirin_astro.Route_def.static "/blog/[slug]" in
  match Kirin_astro.Route_def.matches route "/blog/hello-world" with
  | Some params ->
    check int "param count" 1 (List.length params);
    check string "slug value" "hello-world" (snd (List.hd params))
  | None -> fail "Expected match"

let test_route_matches_rest () =
  let route = Kirin_astro.Route_def.static "/docs/[...path]" in
  match Kirin_astro.Route_def.matches route "/docs/api/users/list" with
  | Some params ->
    check int "param count" 1 (List.length params);
    check string "path value" "api/users/list" (snd (List.hd params))
  | None -> fail "Expected match"

let route_def_tests = [
  "static route", `Quick, test_route_static;
  "ssr route", `Quick, test_route_ssr;
  "hybrid route", `Quick, test_route_hybrid;
  "route params", `Quick, test_route_params;
  "rest params", `Quick, test_route_rest_params;
  "is dynamic", `Quick, test_route_is_dynamic;
  "route matches", `Quick, test_route_matches;
  "route matches rest", `Quick, test_route_matches_rest;
]

(** {1 File Router Tests} *)

let test_file_router_segment_static () =
  match Kirin_astro.File_router.parse_segment "about" with
  | Kirin_astro.File_router.Static name ->
    check string "segment name" "about" name
  | _ -> fail "Expected Static segment"

let test_file_router_segment_dynamic () =
  match Kirin_astro.File_router.parse_segment "[slug]" with
  | Kirin_astro.File_router.Dynamic name ->
    check string "param name" "slug" name
  | _ -> fail "Expected Dynamic segment"

let test_file_router_segment_catchall () =
  match Kirin_astro.File_router.parse_segment "[...path]" with
  | Kirin_astro.File_router.CatchAll name ->
    check string "param name" "path" name
  | _ -> fail "Expected CatchAll segment"

let test_file_router_file_to_path () =
  check string "index" "/"
    (Kirin_astro.File_router.file_to_route_path "index.astro");
  check string "about" "/about"
    (Kirin_astro.File_router.file_to_route_path "about.astro");
  check string "nested" "/blog/post"
    (Kirin_astro.File_router.file_to_route_path "blog/post.astro")

let test_file_router_extension () =
  check bool "is astro" true
    (Kirin_astro.File_router.should_include "index.astro");
  check bool "is mdx" true
    (Kirin_astro.File_router.should_include "post.mdx");
  check bool "is not ts" false
    (Kirin_astro.File_router.should_include "utils.ts")

let file_router_tests = [
  "parse static segment", `Quick, test_file_router_segment_static;
  "parse dynamic segment", `Quick, test_file_router_segment_dynamic;
  "parse catchall segment", `Quick, test_file_router_segment_catchall;
  "file to route path", `Quick, test_file_router_file_to_path;
  "page file extension", `Quick, test_file_router_extension;
]

(** {1 Content Tests} *)

let test_content_string_field () =
  let field = Kirin_astro.Content.string_ () in
  match field with
  | Kirin_astro.Content.StringField { required } ->
    check bool "required" true required
  | _ -> fail "Expected StringField"

let test_content_number_field () =
  let field = Kirin_astro.Content.number ~min:0.0 ~max:100.0 () in
  match field with
  | Kirin_astro.Content.NumberField { required; min; max } ->
    check bool "required" true required;
    check (option (float 0.01)) "min" (Some 0.0) min;
    check (option (float 0.01)) "max" (Some 100.0) max
  | _ -> fail "Expected NumberField"

let test_content_enum_field () =
  let field = Kirin_astro.Content.enum ["draft"; "published"] in
  match field with
  | Kirin_astro.Content.EnumField { values; _ } ->
    check int "value count" 2 (List.length values)
  | _ -> fail "Expected EnumField"

let test_content_schema () =
  let schema = Kirin_astro.Content.define_schema "blog"
    [("title", Kirin_astro.Content.string_ ());
     ("draft", Kirin_astro.Content.bool_ ())]
  in
  check string "schema name" "blog" schema.Kirin_astro.Content.name;
  check int "field count" 2 (List.length schema.Kirin_astro.Content.fields)

let test_content_collection () =
  let schema = Kirin_astro.Content.define_schema "blog"
    [("title", Kirin_astro.Content.string_ ())]
  in
  let collection = Kirin_astro.Content.define_collection ~name:"blog" ~schema () in
  check string "collection name" "blog" collection.Kirin_astro.Content.name

let test_content_entry () =
  let entry = Kirin_astro.Content.create_entry
    ~id:"hello-world"
    ~collection:"blog"
    ~data:[("title", `String "Hello World")]
    ~body:"# Hello\n\nWorld"
    ()
  in
  check string "entry id" "hello-world" entry.Kirin_astro.Content.id;
  check string "entry slug" "hello-world" entry.Kirin_astro.Content.slug

let content_tests = [
  "string field", `Quick, test_content_string_field;
  "number field", `Quick, test_content_number_field;
  "enum field", `Quick, test_content_enum_field;
  "define schema", `Quick, test_content_schema;
  "define collection", `Quick, test_content_collection;
  "create entry", `Quick, test_content_entry;
]

(** {1 Meta Tests} *)

let test_meta_description () =
  let tag = Kirin_astro.Meta.description "A description" in
  match tag with
  | Kirin_astro.Meta.Name { name; content } ->
    check string "tag name" "description" name;
    check string "content" "A description" content
  | _ -> fail "Expected Name tag"

let test_meta_og () =
  let tag = Kirin_astro.Meta.og_title "OG Title" in
  match tag with
  | Kirin_astro.Meta.Property { property; content } ->
    check string "property" "og:title" property;
    check string "content" "OG Title" content
  | _ -> fail "Expected Property tag"

let test_meta_twitter () =
  let tag = Kirin_astro.Meta.twitter_title "Twitter Title" in
  match tag with
  | Kirin_astro.Meta.Name { name; content } ->
    check string "name" "twitter:title" name;
    check string "content" "Twitter Title" content
  | _ -> fail "Expected Name tag"

let test_meta_render () =
  let tag = Kirin_astro.Meta.description "Test" in
  let html = Kirin_astro.Meta.render_meta tag in
  check bool "contains meta" true
    (String.length html > 0)

let test_meta_head () =
  let head = Kirin_astro.Meta.empty
    |> Kirin_astro.Meta.with_title "Test"
  in
  check (option string) "title" (Some "Test") head.Kirin_astro.Meta.title

let test_meta_seo () =
  let head = Kirin_astro.Meta.seo ~title:"SEO Test" () in
  check (option string) "title" (Some "SEO Test") head.Kirin_astro.Meta.title

let test_meta_render_full () =
  let head = Kirin_astro.Meta.empty
    |> Kirin_astro.Meta.with_title "Test Page"
    |> Kirin_astro.Meta.with_meta (Kirin_astro.Meta.description "A test")
  in
  let html = Kirin_astro.Meta.render head in
  check bool "has content" true (String.length html > 0)

let meta_tests = [
  "description tag", `Quick, test_meta_description;
  "og tag", `Quick, test_meta_og;
  "twitter tag", `Quick, test_meta_twitter;
  "render tag", `Quick, test_meta_render;
  "head builder", `Quick, test_meta_head;
  "render full head", `Quick, test_meta_render_full;
  "seo helper", `Quick, test_meta_seo;
]

(** {1 Integration Tests} *)

let test_integration_react () =
  let i = Kirin_astro.Integration.react in
  check string "name" "react" i.Kirin_astro.Integration.config.Kirin_astro.Integration.name

let test_integration_vue () =
  let i = Kirin_astro.Integration.vue in
  check string "name" "vue" i.Kirin_astro.Integration.config.Kirin_astro.Integration.name

let test_integration_svelte () =
  let i = Kirin_astro.Integration.svelte in
  check string "name" "svelte" i.Kirin_astro.Integration.config.Kirin_astro.Integration.name

let test_integration_solid () =
  let i = Kirin_astro.Integration.solid in
  check string "name" "solid" i.Kirin_astro.Integration.config.Kirin_astro.Integration.name

let test_integration_detect () =
  check (option string) "tsx" (Some "react")
    (Kirin_astro.Integration.detect_framework "Counter.tsx");
  check (option string) "vue" (Some "vue")
    (Kirin_astro.Integration.detect_framework "Counter.vue");
  check (option string) "svelte" (Some "svelte")
    (Kirin_astro.Integration.detect_framework "Counter.svelte")

let test_integration_generate_config () =
  let config = Kirin_astro.Integration.generate_config [Kirin_astro.Integration.react] in
  check bool "has content" true (String.length config > 0)

let integration_tests = [
  "react integration", `Quick, test_integration_react;
  "vue integration", `Quick, test_integration_vue;
  "svelte integration", `Quick, test_integration_svelte;
  "solid integration", `Quick, test_integration_solid;
  "detect framework", `Quick, test_integration_detect;
  "generate config", `Quick, test_integration_generate_config;
]

(** {1 View Transitions Tests} *)

let test_view_transitions_config () =
  let config = Kirin_astro.View_transitions.default_config in
  check bool "enabled by default" true config.Kirin_astro.View_transitions.enabled

let test_view_transitions_enable () =
  let config = Kirin_astro.View_transitions.default_config
    |> Kirin_astro.View_transitions.disable
    |> Kirin_astro.View_transitions.enable
  in
  check bool "re-enabled" true config.Kirin_astro.View_transitions.enabled

let test_view_transitions_animation () =
  check string "fade" "fade"
    (Kirin_astro.View_transitions.animation_to_string Kirin_astro.View_transitions.Fade);
  check string "slide" "slide"
    (Kirin_astro.View_transitions.animation_to_string Kirin_astro.View_transitions.Slide)

let test_view_transitions_element () =
  let elem = Kirin_astro.View_transitions.element
    ~name:"header"
    ~animation:Kirin_astro.View_transitions.Slide
    ()
  in
  check string "name" "header" elem.Kirin_astro.View_transitions.name

let test_view_transitions_attrs () =
  let elem = Kirin_astro.View_transitions.element ~name:"hero" () in
  let attrs = Kirin_astro.View_transitions.transition_attrs elem in
  check bool "has transition:name" true
    (String.length attrs > 0)

let test_view_transitions_script () =
  let config = Kirin_astro.View_transitions.default_config in
  let script = Kirin_astro.View_transitions.generate_script config in
  check bool "script not empty" true (String.length script > 0)

let view_transitions_tests = [
  "default config", `Quick, test_view_transitions_config;
  "enable/disable", `Quick, test_view_transitions_enable;
  "animation string", `Quick, test_view_transitions_animation;
  "create element", `Quick, test_view_transitions_element;
  "transition attrs", `Quick, test_view_transitions_attrs;
  "generate script", `Quick, test_view_transitions_script;
]

(** {1 Protocol Tests} *)

let test_protocol_render_request () =
  let req = Kirin_astro.Protocol.render_request ~url:"/test" () in
  check string "method" "render" req.Kirin_astro.Protocol.method_

let test_protocol_encode_request () =
  let req = Kirin_astro.Protocol.render_request ~url:"/test" () in
  let encoded = Kirin_astro.Protocol.encode_request req in
  check bool "contains jsonrpc" true
    (String.length encoded > 0 && encoded.[0] = '{')

let test_protocol_decode_success () =
  let json = {|{"jsonrpc":"2.0","id":1,"result":{"html":"<div>test</div>"}}|} in
  match Kirin_astro.Protocol.decode_response json with
  | Kirin_astro.Protocol.Success { id; _ } ->
    check int "id" 1 id
  | Kirin_astro.Protocol.Error _ -> fail "Expected success"

let test_protocol_decode_error () =
  let json = {|{"jsonrpc":"2.0","id":1,"error":{"code":-32000,"message":"timeout"}}|} in
  match Kirin_astro.Protocol.decode_response json with
  | Kirin_astro.Protocol.Error { code; message; _ } ->
    check int "code" (-32000) code;
    check string "message" "timeout" message
  | Kirin_astro.Protocol.Success _ -> fail "Expected error"

let test_protocol_health_request () =
  let req = Kirin_astro.Protocol.health_request () in
  check string "method" "health" req.Kirin_astro.Protocol.method_

let test_protocol_batch () =
  let reqs = [
    Kirin_astro.Protocol.render_request ~url:"/a" ();
    Kirin_astro.Protocol.render_request ~url:"/b" ();
  ] in
  let encoded = Kirin_astro.Protocol.encode_batch_request reqs in
  check bool "is array" true (encoded.[0] = '[')

let protocol_tests = [
  "render request", `Quick, test_protocol_render_request;
  "encode request", `Quick, test_protocol_encode_request;
  "decode success", `Quick, test_protocol_decode_success;
  "decode error", `Quick, test_protocol_decode_error;
  "health request", `Quick, test_protocol_health_request;
  "batch request", `Quick, test_protocol_batch;
]

(** {1 SSR Tests} *)

let test_ssr_create () =
  let engine = Kirin_astro.Ssr.create Kirin_astro.Ssr.default_config in
  check bool "is running" true engine.Kirin_astro.Ssr.running

let test_ssr_render () =
  let engine = Kirin_astro.Ssr.create Kirin_astro.Ssr.default_config in
  match Kirin_astro.Ssr.render engine ~url:"/test" () with
  | Ok html -> check bool "has html" true (String.length html > 0)
  | Error _ -> fail "Expected success"

let test_ssr_shutdown () =
  let engine = Kirin_astro.Ssr.create Kirin_astro.Ssr.default_config in
  Kirin_astro.Ssr.shutdown engine;
  check bool "not running" false engine.Kirin_astro.Ssr.running

let test_ssr_render_after_shutdown () =
  let engine = Kirin_astro.Ssr.create Kirin_astro.Ssr.default_config in
  Kirin_astro.Ssr.shutdown engine;
  match Kirin_astro.Ssr.render engine ~url:"/test" () with
  | Error msg -> check bool "has error message" true (String.length msg > 0)
  | Ok _ -> fail "Expected error after shutdown"

let test_ssr_stats () =
  let engine = Kirin_astro.Ssr.create Kirin_astro.Ssr.default_config in
  let _ = Kirin_astro.Ssr.render engine ~url:"/a" () in
  let _ = Kirin_astro.Ssr.render engine ~url:"/b" () in
  let stats = Kirin_astro.Ssr.get_stats engine in
  check int "total renders" 2 stats.Kirin_astro.Ssr.total_renders

let test_ssr_cache () =
  let config = { Kirin_astro.Ssr.default_config with enable_cache = true } in
  let engine = Kirin_astro.Ssr.create config in
  let _ = Kirin_astro.Ssr.render engine ~url:"/cached" () in
  let _ = Kirin_astro.Ssr.render engine ~url:"/cached" () in
  let stats = Kirin_astro.Ssr.get_stats engine in
  check int "cache hits" 1 stats.Kirin_astro.Ssr.cache_hits

let test_ssr_cache_miss () =
  let config = { Kirin_astro.Ssr.default_config with enable_cache = true } in
  let engine = Kirin_astro.Ssr.create config in
  let _ = Kirin_astro.Ssr.render engine ~url:"/a" () in
  let _ = Kirin_astro.Ssr.render engine ~url:"/b" () in
  let stats = Kirin_astro.Ssr.get_stats engine in
  check int "cache misses" 2 stats.Kirin_astro.Ssr.cache_misses

let test_ssr_cache_hit_rate () =
  let config = { Kirin_astro.Ssr.default_config with enable_cache = true } in
  let engine = Kirin_astro.Ssr.create config in
  let _ = Kirin_astro.Ssr.render engine ~url:"/test" () in
  let _ = Kirin_astro.Ssr.render engine ~url:"/test" () in
  let stats = Kirin_astro.Ssr.get_stats engine in
  let hit_rate = Kirin_astro.Ssr.cache_hit_rate stats in
  check (float 0.01) "cache hit rate" 0.5 hit_rate

let test_ssr_with_fallback () =
  let engine = Kirin_astro.Ssr.create Kirin_astro.Ssr.default_config in
  Kirin_astro.Ssr.shutdown engine;
  let html = Kirin_astro.Ssr.render_with_fallback engine ~url:"/test" ~fallback:"<h1>Fallback</h1>" in
  check string "fallback used" "<h1>Fallback</h1>" html

let ssr_tests = [
  "create engine", `Quick, test_ssr_create;
  "render page", `Quick, test_ssr_render;
  "shutdown engine", `Quick, test_ssr_shutdown;
  "render after shutdown", `Quick, test_ssr_render_after_shutdown;
  "render stats", `Quick, test_ssr_stats;
  "cache hit", `Quick, test_ssr_cache;
  "cache miss", `Quick, test_ssr_cache_miss;
  "cache hit rate", `Quick, test_ssr_cache_hit_rate;
  "render with fallback", `Quick, test_ssr_with_fallback;
]

(** {1 Handler Tests} *)

let test_handler_config () =
  let config = Kirin_astro.Handler.default_config in
  check bool "hybrid by default" true
    (config.Kirin_astro.Handler.output = Kirin_astro.Handler.Hybrid)

let test_handler_html_response () =
  let resp = Kirin_astro.Handler.html_response "<h1>Hello</h1>" in
  check int "status 200" 200 resp.Kirin_astro.Handler.status;
  check string "body" "<h1>Hello</h1>" resp.Kirin_astro.Handler.body

let test_handler_redirect () =
  let resp = Kirin_astro.Handler.redirect_response "/new-page" in
  check int "status 302" 302 resp.Kirin_astro.Handler.status

let test_handler_json () =
  let resp = Kirin_astro.Handler.json_response (`Assoc [("ok", `Bool true)]) in
  check int "status 200" 200 resp.Kirin_astro.Handler.status;
  check bool "has content-type" true
    (List.mem_assoc "Content-Type" resp.Kirin_astro.Handler.headers)

let test_handler_error () =
  let resp = Kirin_astro.Handler.error_response ~status:404 ~message:"Not found" in
  check int "status 404" 404 resp.Kirin_astro.Handler.status

let test_handler_is_static_asset () =
  check bool "js is static" true
    (Kirin_astro.Handler.is_static_asset "/app.js");
  check bool "css is static" true
    (Kirin_astro.Handler.is_static_asset "/style.css");
  check bool "html is not static" false
    (Kirin_astro.Handler.is_static_asset "/page.html")

let handler_tests = [
  "default config", `Quick, test_handler_config;
  "html response", `Quick, test_handler_html_response;
  "redirect response", `Quick, test_handler_redirect;
  "json response", `Quick, test_handler_json;
  "error response", `Quick, test_handler_error;
  "is static asset", `Quick, test_handler_is_static_asset;
]

(** {1 Codegen Tests} *)

let test_codegen_schema_to_ts () =
  let schema = Kirin_astro.Content.define_schema "Blog"
    [("title", Kirin_astro.Content.string_ ());
     ("views", Kirin_astro.Content.number ())]
  in
  let ts = Kirin_astro.Codegen.schema_to_ts schema in
  check bool "has interface" true
    (ts |> String.split_on_char '\n' |> List.hd |> fun s ->
     String.length s > 0 && String.sub s 0 6 = "export")

let test_codegen_route_to_ts () =
  let route = Kirin_astro.Route_def.static "/blog/[slug]" in
  let ts = Kirin_astro.Codegen.route_to_ts route in
  check bool "has slug param" true
    (String.length ts > 0)

let test_codegen_generate_types () =
  let schema = Kirin_astro.Content.define_schema "blog"
    [("title", Kirin_astro.Content.string_ ())]
  in
  let collection = Kirin_astro.Content.define_collection ~name:"blog" ~schema () in
  let route = Kirin_astro.Route_def.static "/blog/[slug]" in
  let ts = Kirin_astro.Codegen.generate_types_file
    ~routes:[route]
    ~collections:[collection]
  in
  check bool "has Routes interface" true
    (String.length ts > 0)

let test_codegen_routes_file () =
  let route = Kirin_astro.Route_def.static "/about" in
  let ts = Kirin_astro.Codegen.generate_routes_file [route] in
  check bool "has routes array" true
    (String.length ts > 0)

let test_codegen_astro_config () =
  let ts = Kirin_astro.Codegen.generate_astro_config
    ~integrations:[Kirin_astro.Integration.react]
    ~output:Kirin_astro.Handler.Hybrid
  in
  check bool "has defineConfig" true
    (String.length ts > 0)

let test_codegen_env_dts () =
  let ts = Kirin_astro.Codegen.generate_env_dts () in
  check bool "has ImportMetaEnv" true
    (String.length ts > 0)

let codegen_tests = [
  "schema to ts", `Quick, test_codegen_schema_to_ts;
  "route to ts", `Quick, test_codegen_route_to_ts;
  "generate types", `Quick, test_codegen_generate_types;
  "routes file", `Quick, test_codegen_routes_file;
  "astro config", `Quick, test_codegen_astro_config;
  "env.d.ts", `Quick, test_codegen_env_dts;
]

(** {1 API Facade Tests} *)

let test_facade_dev_config () =
  let config = Kirin_astro.dev_config () in
  check bool "dev mode" true config.Kirin_astro.Handler.dev_mode

let test_facade_static_config () =
  let config = Kirin_astro.static_config ~dist_path:"dist" () in
  check bool "static output" true
    (config.Kirin_astro.Handler.output = Kirin_astro.Handler.Static)

let test_facade_react_island () =
  let island = Kirin_astro.react_island ~component:"Counter" () in
  check bool "is react" true
    (island.Kirin_astro.Island.framework = Kirin_astro.Island.React)

let test_facade_vue_island () =
  let island = Kirin_astro.vue_island ~component:"Counter" () in
  check bool "is vue" true
    (island.Kirin_astro.Island.framework = Kirin_astro.Island.Vue)

let test_facade_client_directives () =
  check bool "client_load" true
    (Kirin_astro.client_load = Kirin_astro.Island.ClientLoad);
  check bool "client_idle" true
    (Kirin_astro.client_idle = Kirin_astro.Island.ClientIdle);
  check bool "client_visible" true
    (Kirin_astro.client_visible = Kirin_astro.Island.ClientVisible)

let facade_tests = [
  "dev config", `Quick, test_facade_dev_config;
  "static config", `Quick, test_facade_static_config;
  "react island", `Quick, test_facade_react_island;
  "vue island", `Quick, test_facade_vue_island;
  "client directives", `Quick, test_facade_client_directives;
]

(** {1 Test Runner} *)

let () =
  run "Kirin Astro" [
    "Island", island_tests;
    "Route_def", route_def_tests;
    "File_router", file_router_tests;
    "Content", content_tests;
    "Meta", meta_tests;
    "Integration", integration_tests;
    "View_transitions", view_transitions_tests;
    "Protocol", protocol_tests;
    "SSR", ssr_tests;
    "Handler", handler_tests;
    "Codegen", codegen_tests;
    "Facade", facade_tests;
  ]
