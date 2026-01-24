(** Lit SSR Engine

    Server-side rendering for Lit components using @lit-labs/ssr.
    Manages Node.js worker pool for efficient rendering. *)

(** {1 SSR Configuration} *)

(** SSR configuration *)
type ssr_config = {
  bundle: string;
  pool_size: int;
  timeout: float;
  memory_limit_mb: int;
  restart_after: int;
  use_declarative_shadow_dom: bool;
  include_polyfill: bool;
}

(** Default configuration *)
let default_config = {
  bundle = "dist/server/lit-ssr.js";
  pool_size = 4;
  timeout = 5.0;
  memory_limit_mb = 200;
  restart_after = 5000;
  use_declarative_shadow_dom = true;
  include_polyfill = true;
}

(** {1 Worker Management} *)

(** Worker state *)
type worker_state =
  | Idle
  | Busy
  | Dead

(** Worker *)
type worker = {
  mutable state: worker_state;
  mutable request_count: int;
  mutable last_error: string option;
}

(** SSR engine *)
type t = {
  config: ssr_config;
  workers: worker array;
  mutable total_renders: int;
  mutable errors: int;
  mutable timeouts: int;
  mutable cache_hits: int;
  mutable cache_misses: int;
}

(** SSR statistics *)
type ssr_stats = {
  stat_total_renders: int;
  stat_errors: int;
  stat_timeouts: int;
  stat_avg_time_ms: float;
  stat_cache_hit_rate: float;
  stat_active_workers: int;
}

(** {1 Engine Creation} *)

(** Create SSR engine *)
let create config =
  let workers = Array.init config.pool_size (fun _ -> {
    state = Idle;
    request_count = 0;
    last_error = None;
  }) in
  {
    config;
    workers;
    total_renders = 0;
    errors = 0;
    timeouts = 0;
    cache_hits = 0;
    cache_misses = 0;
  }

(** {1 Rendering} *)

(** Render result *)
type render_result = {
  html: string;
  components: string list;
  has_shadow_dom: bool;
}

(** Simulate render (actual implementation would use @lit-labs/ssr) *)
let render_internal engine ~tag_name ?(props = `Null) ?(slots = []) () =
  (* In real implementation, this would:
     1. Get available worker from pool
     2. Send JSON-RPC request with component name and props
     3. Use @lit-labs/ssr to render to string
     4. Return Declarative Shadow DOM HTML *)

  let props_attrs = match props with
    | `Assoc pairs ->
      pairs |> List.map (fun (k, v) ->
        let value = match v with
          | `String s -> s
          | `Bool b -> string_of_bool b
          | `Int n -> string_of_int n
          | `Float f -> string_of_float f
          | _ -> Yojson.Safe.to_string v
        in
        Printf.sprintf " %s=\"%s\"" k value
      ) |> String.concat ""
    | _ -> ""
  in

  let slot_content = slots |> List.map (fun (name, content) ->
    match name with
    | None -> content
    | Some n -> Printf.sprintf "<span slot=\"%s\">%s</span>" n content
  ) |> String.concat "" in

  let shadow_dom = if engine.config.use_declarative_shadow_dom then
    Printf.sprintf {|<template shadowrootmode="open">
  <style>:host { display: block; }</style>
  <slot></slot>
</template>|}
    else ""
  in

  let html = Printf.sprintf "<%s%s>%s%s</%s>"
    tag_name props_attrs shadow_dom slot_content tag_name
  in

  Ok { html; components = [tag_name]; has_shadow_dom = engine.config.use_declarative_shadow_dom }

(** Render component *)
let render engine ~tag_name ?props ?slots () =
  engine.total_renders <- engine.total_renders + 1;
  engine.cache_misses <- engine.cache_misses + 1;
  match render_internal engine ~tag_name ?props ?slots () with
  | Ok result ->
    let html = if engine.config.include_polyfill then
      Shadow_dom.with_polyfill result.html
    else
      result.html
    in
    Ok html
  | Error msg ->
    engine.errors <- engine.errors + 1;
    Error msg

(** Render with fallback *)
let render_with_fallback engine ~tag_name ~fallback =
  match render engine ~tag_name () with
  | Ok html -> html
  | Error _ -> fallback

(** {1 Multi-Component Rendering} *)

(** Render page with multiple components *)
let render_page engine ~components =
  let results = components |> List.map (fun (tag_name, props, slots) ->
    match render_internal engine ~tag_name ~props ~slots () with
    | Ok r -> r.html
    | Error _ -> Printf.sprintf "<%s></%s>" tag_name tag_name
  ) in

  let html = String.concat "\n" results in

  (* Add polyfill once at the end *)
  if engine.config.include_polyfill then
    html ^ "\n" ^ Shadow_dom.polyfill_check
  else
    html

(** {1 Streaming SSR} *)

(** Stream render result *)
type stream_chunk =
  | Shell of string
  | Component of string
  | Complete

(** Render with streaming (simulated) *)
let render_stream engine ~tag_name ~on_chunk =
  (* Shell first *)
  on_chunk (Shell (Printf.sprintf "<%s>" tag_name));

  (* Shadow DOM template *)
  if engine.config.use_declarative_shadow_dom then
    on_chunk (Component {|<template shadowrootmode="open"><slot></slot></template>|});

  (* Close tag *)
  on_chunk (Shell (Printf.sprintf "</%s>" tag_name));

  (* Polyfill if needed *)
  if engine.config.include_polyfill then
    on_chunk (Component Shadow_dom.polyfill_check);

  on_chunk Complete

(** {1 Kirin Integration} *)

(** Create Kirin handler *)
let handler engine ~tag_name =
  fun _req ->
    match render engine ~tag_name () with
    | Ok html -> Kirin.Response.html html
    | Error msg -> Kirin.Response.html ("<h1>SSR Error</h1><p>" ^ msg ^ "</p>")

(** Create handler with props from request *)
let handler_with_props ~get_props engine ~tag_name =
  fun req ->
    let props = get_props req in
    match render engine ~tag_name ~props () with
    | Ok html -> Kirin.Response.html html
    | Error msg -> Kirin.Response.html ("<h1>SSR Error</h1><p>" ^ msg ^ "</p>")

(** {1 Statistics} *)

(** Get engine statistics *)
let stats engine =
  let active = Array.fold_left (fun acc w ->
    if w.state <> Dead then acc + 1 else acc
  ) 0 engine.workers in
  let total = engine.cache_hits + engine.cache_misses in
  let hit_rate = if total > 0 then float_of_int engine.cache_hits /. float_of_int total else 0.0 in
  {
    stat_total_renders = engine.total_renders;
    stat_errors = engine.errors;
    stat_timeouts = engine.timeouts;
    stat_avg_time_ms = 0.0;  (* Would track actual times *)
    stat_cache_hit_rate = hit_rate;
    stat_active_workers = active;
  }

(** {1 Lifecycle} *)

(** Shutdown engine *)
let shutdown engine =
  Array.iter (fun w -> w.state <- Dead) engine.workers

(** Check if engine is healthy *)
let is_healthy engine =
  Array.exists (fun w -> w.state <> Dead) engine.workers

(** {1 Serialization} *)

(** Config to JSON *)
let config_to_json c =
  `Assoc [
    ("bundle", `String c.bundle);
    ("workers", `Int c.pool_size);
    ("timeout", `Float c.timeout);
    ("memoryLimitMb", `Int c.memory_limit_mb);
    ("restartAfter", `Int c.restart_after);
    ("useDeclarativeShadowDom", `Bool c.use_declarative_shadow_dom);
    ("includePolyfill", `Bool c.include_polyfill);
  ]

(** Stats to JSON *)
let stats_to_json s =
  `Assoc [
    ("totalRenders", `Int s.stat_total_renders);
    ("errors", `Int s.stat_errors);
    ("timeouts", `Int s.stat_timeouts);
    ("avgTimeMs", `Float s.stat_avg_time_ms);
    ("cacheHitRate", `Float s.stat_cache_hit_rate);
    ("activeWorkers", `Int s.stat_active_workers);
  ]

(** Render result to JSON *)
let result_to_json r =
  `Assoc [
    ("html", `String r.html);
    ("components", `List (List.map (fun c -> `String c) r.components));
    ("hasShadowDom", `Bool r.has_shadow_dom);
  ]
