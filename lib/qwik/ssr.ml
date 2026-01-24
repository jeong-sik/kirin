(** Qwik SSR Engine

    Server-side rendering for Qwik with resumability. *)

(** {1 Configuration} *)

(** SSR configuration *)
type config = {
  server_entry: string;       (* Server entry point *)
  manifest_path: string;      (* Q-manifest.json location *)
  workers: int;               (* Worker count *)
  timeout_s: float;           (* Render timeout *)
  memory_limit_mb: int;       (* Memory limit per worker *)
  restart_after: int;         (* Restart after N requests *)
  enable_cache: bool;         (* Enable render cache *)
  cache_ttl_s: float;         (* Cache TTL *)
  cache_max_size: int;        (* Max cache entries *)
}

(** Default configuration *)
let default_config = {
  server_entry = "dist/server/entry.mjs";
  manifest_path = "dist/q-manifest.json";
  workers = 4;
  timeout_s = 5.0;
  memory_limit_mb = 200;
  restart_after = 5000;
  enable_cache = true;
  cache_ttl_s = 300.0;
  cache_max_size = 1000;
}

(** {1 Cache} *)

(** Cache entry *)
type cache_entry = {
  html: string;
  container_state: Container.t option;
  created_at: float;
  ttl: float;
}

(** LRU Cache *)
type cache = {
  mutable entries: (string * cache_entry) list;
  max_size: int;
  default_ttl: float;
}

(** Create cache *)
let create_cache ~max_size ~default_ttl = {
  entries = [];
  max_size;
  default_ttl;
}

(** Get from cache *)
let cache_get cache key =
  let now = Unix.gettimeofday () in
  match List.assoc_opt key cache.entries with
  | Some entry when now -. entry.created_at < entry.ttl -> Some entry
  | _ ->
    (* Remove expired *)
    cache.entries <- List.filter (fun (k, _) -> k <> key) cache.entries;
    None

(** Put in cache *)
let cache_put cache key html ?container_state () =
  let entry = {
    html;
    container_state;
    created_at = Unix.gettimeofday ();
    ttl = cache.default_ttl;
  } in
  (* Remove old if exists *)
  cache.entries <- List.filter (fun (k, _) -> k <> key) cache.entries;
  (* Add new *)
  cache.entries <- (key, entry) :: cache.entries;
  (* Evict if over max *)
  if List.length cache.entries > cache.max_size then
    cache.entries <- List.rev cache.entries |> List.tl |> List.rev

(** {1 Engine State} *)

(** Engine statistics *)
type stats = {
  mutable total_renders: int;
  mutable cache_hits: int;
  mutable cache_misses: int;
  mutable errors: int;
  mutable timeouts: int;
  mutable avg_render_ms: float;
}

(** Create stats *)
let create_stats () = {
  total_renders = 0;
  cache_hits = 0;
  cache_misses = 0;
  errors = 0;
  timeouts = 0;
  avg_render_ms = 0.0;
}

(** SSR Engine *)
type t = {
  config: config;
  cache: cache option;
  stats: stats;
  mutable running: bool;
  mutable next_id: int;
}

(** {1 Engine Operations} *)

(** Create SSR engine *)
let create config =
  let cache = if config.enable_cache then
    Some (create_cache ~max_size:config.cache_max_size ~default_ttl:config.cache_ttl_s)
  else
    None
  in
  {
    config;
    cache;
    stats = create_stats ();
    running = true;
    next_id = 1;
  }

(** Check if engine is running *)
let is_running engine = engine.running

(** Get next request ID *)
let next_id engine =
  let id = engine.next_id in
  engine.next_id <- id + 1;
  id

(** Get stats *)
let get_stats engine = engine.stats

(** Cache hit rate *)
let cache_hit_rate stats =
  let total = stats.cache_hits + stats.cache_misses in
  if total = 0 then 0.0
  else float_of_int stats.cache_hits /. float_of_int total

(** {1 Rendering} *)

(** Generate cache key *)
let cache_key ~url ~headers:_ =
  (* Simple key - could include more factors *)
  url

(** Render URL *)
let render engine ~url ?(headers=[]) ?(container_attrs=[]) ?server_data () =
  (* Silence unused warnings for future use *)
  let _ = container_attrs, server_data in
  if not engine.running then
    Error "Engine is not running"
  else begin
    engine.stats.total_renders <- engine.stats.total_renders + 1;

    (* Check cache first *)
    let key = cache_key ~url ~headers in
    match engine.cache with
    | Some cache when engine.config.enable_cache ->
      (match cache_get cache key with
      | Some entry ->
        engine.stats.cache_hits <- engine.stats.cache_hits + 1;
        Ok entry.html
      | None ->
        engine.stats.cache_misses <- engine.stats.cache_misses + 1;
        (* Simulate render *)
        let start = Unix.gettimeofday () in
        let html = Printf.sprintf {|<!DOCTYPE html>
<html>
<head>
  <meta charset="UTF-8">
</head>
<body>
  <div id="qwik-container" q:container="paused" q:base="/">
    <!-- Qwik SSR content for %s -->
    <div>Rendered at %s</div>
  </div>
  <script type="qwik/json">{"ctx":{}}</script>
  <script type="module" src="/build/q-loader.js"></script>
</body>
</html>|} url (string_of_float start) in
        let elapsed = (Unix.gettimeofday () -. start) *. 1000.0 in
        engine.stats.avg_render_ms <-
          (engine.stats.avg_render_ms *. float_of_int (engine.stats.total_renders - 1) +. elapsed)
          /. float_of_int engine.stats.total_renders;
        (* Cache the result *)
        cache_put cache key html ();
        Ok html)
    | _ ->
      (* No cache, just render *)
      let html = Printf.sprintf {|<!DOCTYPE html>
<html>
<head><meta charset="UTF-8"></head>
<body>
  <div id="qwik-container" q:container="paused">
    <!-- Qwik SSR: %s -->
  </div>
</body>
</html>|} url in
      Ok html
  end

(** Render with fallback on error *)
let render_with_fallback engine ~url ~fallback =
  match render engine ~url () with
  | Ok html -> html
  | Error _ -> fallback

(** {1 Prerendering} *)

(** Prerender routes to static files *)
let prerender engine ~routes ~output_dir =
  List.iter (fun route ->
    let url = route.Route_def.path in
    match render engine ~url () with
    | Ok html ->
      let path = if url = "/" then "/index" else url in
      let file_path = Filename.concat output_dir (path ^ ".html") in
      let dir = Filename.dirname file_path in
      ignore (Sys.command (Printf.sprintf "mkdir -p %s" dir));
      let oc = open_out file_path in
      output_string oc html;
      close_out oc
    | Error msg ->
      Printf.eprintf "Failed to prerender %s: %s\n" url msg
  ) routes

(** {1 Shutdown} *)

(** Shutdown engine *)
let shutdown engine =
  engine.running <- false

(** {1 Serialization} *)

(** Stats to JSON *)
let stats_to_json stats =
  `Assoc [
    ("totalRenders", `Int stats.total_renders);
    ("cacheHits", `Int stats.cache_hits);
    ("cacheMisses", `Int stats.cache_misses);
    ("errors", `Int stats.errors);
    ("timeouts", `Int stats.timeouts);
    ("avgRenderMs", `Float stats.avg_render_ms);
    ("cacheHitRate", `Float (cache_hit_rate stats));
  ]

(** Config to JSON *)
let config_to_json config =
  `Assoc [
    ("serverEntry", `String config.server_entry);
    ("manifestPath", `String config.manifest_path);
    ("workers", `Int config.workers);
    ("timeoutS", `Float config.timeout_s);
    ("memoryLimitMb", `Int config.memory_limit_mb);
    ("restartAfter", `Int config.restart_after);
    ("enableCache", `Bool config.enable_cache);
    ("cacheTtlS", `Float config.cache_ttl_s);
    ("cacheMaxSize", `Int config.cache_max_size);
  ]
