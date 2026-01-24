(** Vue/Nuxt SSR Engine

    Server-side rendering with worker pool and caching. *)

(** {1 SSR Configuration} *)

(** Cache strategy *)
type cache_strategy =
  | NoCache
  | Stale_while_revalidate of int  (* TTL seconds *)
  | Cache_control of string

(** SSR config *)
type config = {
  bundle: string;
  workers: int;
  timeout_s: float;
  memory_limit_mb: int;
  restart_after: int;
  cache_strategy: cache_strategy;
  prerender_routes: string list;
  fallback_html: string option;
}

(** Default config *)
let default_config = {
  bundle = "dist/server/entry.mjs";
  workers = 4;
  timeout_s = 5.0;
  memory_limit_mb = 512;
  restart_after = 10000;
  cache_strategy = NoCache;
  prerender_routes = [];
  fallback_html = None;
}

(** {1 SSR Result} *)

(** Render result *)
type render_result = {
  html: string;
  status: int;
  headers: (string * string) list;
  redirect: string option;
  render_time_ms: float;
  cache_hit: bool;
}

(** {1 SSR Stats} *)

(** Engine stats *)
type stats = {
  total_renders: int;
  cache_hits: int;
  cache_misses: int;
  errors: int;
  timeouts: int;
  avg_render_time_ms: float;
  active_workers: int;
  total_memory_mb: int;
}

(** Empty stats *)
let empty_stats = {
  total_renders = 0;
  cache_hits = 0;
  cache_misses = 0;
  errors = 0;
  timeouts = 0;
  avg_render_time_ms = 0.0;
  active_workers = 0;
  total_memory_mb = 0;
}

(** {1 SSR Engine} *)

(** Engine state *)
type t = {
  config: config;
  mutable stats: stats;
  mutable cache: (string, render_result * float) Hashtbl.t;
  mutable running: bool;
}

(** Create SSR engine *)
let create config = {
  config;
  stats = empty_stats;
  cache = Hashtbl.create 256;
  running = true;
}

(** {1 Caching} *)

(** Get cache TTL *)
let cache_ttl config =
  match config.cache_strategy with
  | NoCache -> 0
  | Stale_while_revalidate ttl -> ttl
  | Cache_control _ -> 60  (* Default 1 minute *)

(** Check cache *)
let check_cache t url =
  match Hashtbl.find_opt t.cache url with
  | None -> None
  | Some (result, timestamp) ->
    let ttl = Float.of_int (cache_ttl t.config) in
    let now = Unix.gettimeofday () in
    if now -. timestamp < ttl then
      Some result
    else begin
      Hashtbl.remove t.cache url;
      None
    end

(** Store in cache *)
let store_cache t url result =
  if cache_ttl t.config > 0 then
    Hashtbl.replace t.cache url (result, Unix.gettimeofday ())

(** Clear cache *)
let clear_cache t =
  Hashtbl.clear t.cache

(** {1 Rendering} *)

(** Render a page (simulated - real impl would use worker pool) *)
let render t ~url ?(headers=[]) ?(payload=None) () =
  if not t.running then
    Error "SSR engine is not running"
  else
    (* Check cache first *)
    match check_cache t url with
    | Some cached ->
      t.stats <- { t.stats with
        total_renders = t.stats.total_renders + 1;
        cache_hits = t.stats.cache_hits + 1;
      };
      Ok { cached with cache_hit = true }
    | None ->
      let start_time = Unix.gettimeofday () in
      (* In real implementation, this would call the worker pool *)
      let html = Printf.sprintf {|<!DOCTYPE html>
<html>
<head>
  <title>Vue SSR</title>
</head>
<body>
  <div id="__nuxt"><!-- SSR Content for %s --></div>
  <script type="module" src="/_nuxt/entry.js"></script>
</body>
</html>|} url in
      let end_time = Unix.gettimeofday () in
      let render_time_ms = (end_time -. start_time) *. 1000.0 in

      let result = {
        html;
        status = 200;
        headers = ("Content-Type", "text/html") :: headers;
        redirect = None;
        render_time_ms;
        cache_hit = false;
      } in

      (* Update stats *)
      let total = t.stats.total_renders + 1 in
      let total_time = t.stats.avg_render_time_ms *. Float.of_int t.stats.total_renders +. render_time_ms in
      t.stats <- { t.stats with
        total_renders = total;
        cache_misses = t.stats.cache_misses + 1;
        avg_render_time_ms = total_time /. Float.of_int total;
      };

      (* Store in cache *)
      store_cache t url result;
      ignore payload;

      Ok result

(** Render with fallback *)
let render_with_fallback t ~url ~fallback =
  match render t ~url () with
  | Ok result -> result.html
  | Error _ ->
    t.stats <- { t.stats with errors = t.stats.errors + 1 };
    fallback

(** {1 Prerendering} *)

(** Prerender routes for static generation *)
let prerender t routes =
  List.filter_map (fun url ->
    match render t ~url () with
    | Ok result -> Some (url, result.html)
    | Error _ -> None
  ) routes

(** {1 Lifecycle} *)

(** Get stats *)
let stats t = t.stats

(** Get config *)
let get_config t = t.config

(** Is running *)
let is_running t = t.running

(** Shutdown engine *)
let shutdown t =
  t.running <- false;
  clear_cache t

(** {1 Serialization} *)

(** Cache strategy to string *)
let cache_strategy_to_string = function
  | NoCache -> "no-cache"
  | Stale_while_revalidate ttl -> Printf.sprintf "swr-%d" ttl
  | Cache_control cc -> cc

(** Stats to JSON *)
let stats_to_json s =
  `Assoc [
    ("totalRenders", `Int s.total_renders);
    ("cacheHits", `Int s.cache_hits);
    ("cacheMisses", `Int s.cache_misses);
    ("cacheHitRate", `Float (
      if s.total_renders = 0 then 0.0
      else Float.of_int s.cache_hits /. Float.of_int s.total_renders
    ));
    ("errors", `Int s.errors);
    ("timeouts", `Int s.timeouts);
    ("avgRenderTimeMs", `Float s.avg_render_time_ms);
    ("activeWorkers", `Int s.active_workers);
    ("totalMemoryMb", `Int s.total_memory_mb);
  ]

(** Config to JSON *)
let config_to_json c =
  `Assoc [
    ("bundle", `String c.bundle);
    ("workers", `Int c.workers);
    ("timeoutS", `Float c.timeout_s);
    ("memoryLimitMb", `Int c.memory_limit_mb);
    ("restartAfter", `Int c.restart_after);
    ("cacheStrategy", `String (cache_strategy_to_string c.cache_strategy));
    ("prerenderRoutes", `List (List.map (fun r -> `String r) c.prerender_routes));
  ]

(** {1 Route-Specific Config} *)

(** Route SSR options *)
type route_options = {
  ssr: bool;
  cache: cache_strategy;
  prerender: bool;
  headers: (string * string) list;
}

(** Default route options *)
let default_route_options = {
  ssr = true;
  cache = NoCache;
  prerender = false;
  headers = [];
}

(** Apply route options *)
let with_route_options opts t ~url =
  if not opts.ssr then
    (* Return SPA shell only *)
    Ok {
      html = (match t.config.fallback_html with
        | Some fb -> fb
        | None -> {|<!DOCTYPE html>
<html>
<head><title>Vue App</title></head>
<body>
  <div id="__nuxt"></div>
  <script type="module" src="/_nuxt/entry.js"></script>
</body>
</html>|});
      status = 200;
      headers = opts.headers @ [("Content-Type", "text/html")];
      redirect = None;
      render_time_ms = 0.0;
      cache_hit = false;
    }
  else
    render t ~url ~headers:opts.headers ()
