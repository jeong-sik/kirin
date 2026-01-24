(** Angular SSR Engine

    SSR engine with Node.js worker pool for Angular Universal. *)

(** {1 Configuration} *)

(** SSR configuration *)
type config = {
  bundle: string;
  workers: int;
  timeout_s: float;
  memory_limit_mb: int;
  restart_after: int;
  enable_cache: bool;
  cache_size: int;
  prerender_routes: string list;
}

(** Default configuration *)
let default_config = {
  bundle = "dist/server/main.js";
  workers = 4;
  timeout_s = 5.0;
  memory_limit_mb = 256;
  restart_after = 5000;
  enable_cache = true;
  cache_size = 1000;
  prerender_routes = [];
}

(** {1 Cache} *)

(** Cache entry *)
type cache_entry = {
  html: string;
  timestamp: float;
  ttl: float;
}

(** LRU cache *)
type cache = {
  mutable entries: (string * cache_entry) list;
  max_size: int;
}

(** Create cache *)
let create_cache size = {
  entries = [];
  max_size = size;
}

(** Get from cache *)
let cache_get cache key =
  let now = Unix.gettimeofday () in
  match List.assoc_opt key cache.entries with
  | Some entry when now -. entry.timestamp < entry.ttl ->
    (* Move to front (LRU) *)
    cache.entries <- (key, entry) :: List.filter (fun (k, _) -> k <> key) cache.entries;
    Some entry.html
  | _ ->
    (* Remove expired *)
    cache.entries <- List.filter (fun (k, _) -> k <> key) cache.entries;
    None

(** Set cache entry *)
let cache_set cache key html ?(ttl=60.0) () =
  let entry = { html; timestamp = Unix.gettimeofday (); ttl } in
  cache.entries <- (key, entry) :: List.filter (fun (k, _) -> k <> key) cache.entries;
  (* Enforce max size *)
  if List.length cache.entries > cache.max_size then
    cache.entries <- List.filteri (fun i _ -> i < cache.max_size) cache.entries

(** {1 Stats} *)

(** SSR statistics *)
type stats = {
  mutable total_renders: int;
  mutable cache_hits: int;
  mutable cache_misses: int;
  mutable errors: int;
  mutable timeouts: int;
  mutable total_time_ms: float;
}

(** Create stats *)
let create_stats () = {
  total_renders = 0;
  cache_hits = 0;
  cache_misses = 0;
  errors = 0;
  timeouts = 0;
  total_time_ms = 0.0;
}

(** Get cache hit rate *)
let cache_hit_rate stats =
  let total = stats.cache_hits + stats.cache_misses in
  if total = 0 then 0.0
  else float_of_int stats.cache_hits /. float_of_int total

(** Get average render time *)
let avg_render_time stats =
  if stats.total_renders = 0 then 0.0
  else stats.total_time_ms /. float_of_int stats.total_renders

(** {1 Engine} *)

(** SSR engine state *)
type t = {
  config: config;
  mutable running: bool;
  cache: cache;
  stats: stats;
}

(** Create SSR engine *)
let create config = {
  config;
  running = true;
  cache = create_cache config.cache_size;
  stats = create_stats ();
}

(** Check if running *)
let is_running engine = engine.running

(** Shutdown engine *)
let shutdown engine =
  engine.running <- false

(** Get stats *)
let get_stats engine = engine.stats

(** {1 Rendering} *)

(** Render page (mock - actual would spawn Node.js worker) *)
let render engine ~url ?(headers=[]) () =
  if not engine.running then
    Error "Engine not running"
  else begin
    let start = Unix.gettimeofday () in
    engine.stats.total_renders <- engine.stats.total_renders + 1;

    (* Check cache first *)
    let cache_key = url in
    match cache_get engine.cache cache_key with
    | Some html when engine.config.enable_cache ->
      engine.stats.cache_hits <- engine.stats.cache_hits + 1;
      Ok html
    | _ ->
      engine.stats.cache_misses <- engine.stats.cache_misses + 1;
      (* Mock render - in real impl would call Node.js worker *)
      let html = Printf.sprintf {|<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <title>Angular SSR</title>
</head>
<body>
  <app-root ng-version="19.0.0" ng-server-context="ssr">
    <router-outlet></router-outlet>
    <!-- URL: %s -->
  </app-root>
  <script id="ng-state" type="application/json">{}</script>
</body>
</html>|} url in

      (* Cache result *)
      if engine.config.enable_cache then
        cache_set engine.cache cache_key html ();

      let elapsed = (Unix.gettimeofday () -. start) *. 1000.0 in
      engine.stats.total_time_ms <- engine.stats.total_time_ms +. elapsed;

      let _ = headers in  (* Use headers in real impl *)
      Ok html
  end

(** Render with fallback *)
let render_with_fallback engine ~url ~fallback =
  match render engine ~url () with
  | Ok html -> html
  | Error _ -> fallback

(** {1 Prerendering} *)

(** Prerender routes to static files *)
let prerender engine ~routes ~output_dir =
  List.filter_map (fun route ->
    match render engine ~url:route () with
    | Ok html ->
      let filename = if route = "/" then "index.html"
        else (String.sub route 1 (String.length route - 1)) ^ ".html"
      in
      let filepath = Filename.concat output_dir filename in
      (* In real impl, would write to file *)
      Some (route, filepath, html)
    | Error _ ->
      engine.stats.errors <- engine.stats.errors + 1;
      None
  ) routes

(** {1 Serialization} *)

(** Stats to JSON *)
let stats_to_json stats =
  `Assoc [
    ("totalRenders", `Int stats.total_renders);
    ("cacheHits", `Int stats.cache_hits);
    ("cacheMisses", `Int stats.cache_misses);
    ("cacheHitRate", `Float (cache_hit_rate stats));
    ("errors", `Int stats.errors);
    ("timeouts", `Int stats.timeouts);
    ("avgRenderTimeMs", `Float (avg_render_time stats));
  ]

(** Config to JSON *)
let config_to_json config =
  `Assoc [
    ("bundle", `String config.bundle);
    ("workers", `Int config.workers);
    ("timeoutS", `Float config.timeout_s);
    ("memoryLimitMb", `Int config.memory_limit_mb);
    ("restartAfter", `Int config.restart_after);
    ("enableCache", `Bool config.enable_cache);
    ("cacheSize", `Int config.cache_size);
  ]
