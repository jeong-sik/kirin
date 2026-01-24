(** Preact SSR Engine

    Server-side rendering engine for Preact applications.
    Manages Node.js worker pool for efficient rendering. *)

(** {1 SSR Types} *)

(** SSR configuration *)
type ssr_config = {
  bundle: string;
  pool_size: int;
  timeout: float;
  memory_limit_mb: int;
  restart_after: int;
  signals_ssr: bool;
}

(** Default configuration *)
let default_config = {
  bundle = "dist/server/entry-server.js";
  pool_size = 4;
  timeout = 5.0;
  memory_limit_mb = 200;
  restart_after = 5000;
  signals_ssr = true;
}

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
type stats = {
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
  head: string option;
  signals_state: Yojson.Safe.t option;
}

(** Simulate render (actual implementation would use Node.js worker) *)
let render_internal _engine ~url ?(props = `Null) ?(signals = `Null) () =
  (* In real implementation, this would:
     1. Get available worker from pool
     2. Send JSON-RPC request via Protocol
     3. Wait for response with timeout
     4. Parse and return result *)
  let html =
    "<div id=\"app\">" ^
    "<h1>Preact SSR</h1>" ^
    "<p>Rendered: " ^ url ^ "</p>" ^
    (match props with
     | `Null -> ""
     | _ -> "<script>window.__PROPS__=" ^ Yojson.Safe.to_string props ^ "</script>") ^
    (match signals with
     | `Null -> ""
     | _ -> "<script>window.__SIGNALS__=" ^ Yojson.Safe.to_string signals ^ "</script>") ^
    "</div>"
  in
  Ok { html; head = None; signals_state = None }

(** Render URL with SSR *)
let render engine ~url ?props ?signals () =
  engine.total_renders <- engine.total_renders + 1;
  engine.cache_misses <- engine.cache_misses + 1;
  match render_internal engine ~url ?props ?signals () with
  | Ok result -> Ok result.html
  | Error msg ->
    engine.errors <- engine.errors + 1;
    Error msg

(** Render with fallback *)
let render_with_fallback engine ~url ~fallback =
  match render engine ~url () with
  | Ok html -> html
  | Error _ -> fallback

(** {1 Signals SSR} *)

(** Render with signals state *)
let render_with_signals engine ~url ~signals () =
  if not engine.config.signals_ssr then
    render engine ~url ()
  else
    render engine ~url ~signals ()

(** Dehydrate signals for client *)
let dehydrate_signals signals =
  "<script>window.__PREACT_SIGNALS__=" ^
  Yojson.Safe.to_string signals ^
  ";</script>"

(** {1 Kirin Integration} *)

(** Create Kirin handler *)
let handler engine =
  fun _req ->
    let url = "/todo" in  (* Would extract from request *)
    match render engine ~url () with
    | Ok html -> Kirin.Response.html html
    | Error msg -> Kirin.Response.html ("<h1>SSR Error</h1><p>" ^ msg ^ "</p>")

(** Create handler with fallback *)
let handler_with_fallback ~fallback engine =
  fun _req ->
    let url = "/todo" in
    let html = render_with_fallback engine ~url ~fallback in
    Kirin.Response.html html

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
    ("signalsSsr", `Bool c.signals_ssr);
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
