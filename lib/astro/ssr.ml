(** Astro SSR Engine

    Server-side rendering with islands architecture. *)

(** {1 Configuration} *)

(** SSR configuration *)
type config = {
  workers: int;
  timeout_s: float;
  enable_cache: bool;
  cache_ttl_s: float;
  max_cache_entries: int;
  memory_limit_mb: int;
  restart_after: int;
  dist_path: string;
  integrations: Integration.t list;
}

(** Default configuration *)
let default_config = {
  workers = 4;
  timeout_s = 5.0;
  enable_cache = true;
  cache_ttl_s = 300.0;
  max_cache_entries = 1000;
  memory_limit_mb = 200;
  restart_after = 5000;
  dist_path = "dist";
  integrations = [];
}

(** {1 Cache} *)

(** Cache entry *)
type cache_entry = {
  html: string;
  head: string;
  created_at: float;
  ttl: float;
}

(** Cache *)
type cache = {
  mutable entries: (string * cache_entry) list;
  max_entries: int;
}

(** Create cache *)
let create_cache max_entries = {
  entries = [];
  max_entries;
}

(** Get from cache *)
let cache_get cache key =
  let now = Unix.gettimeofday () in
  match List.assoc_opt key cache.entries with
  | Some entry when now -. entry.created_at < entry.ttl -> Some entry
  | _ -> None

(** Put in cache *)
let cache_put cache key html head ttl =
  let entry = { html; head; created_at = Unix.gettimeofday (); ttl } in
  (* Remove old entry if exists *)
  let entries = List.filter (fun (k, _) -> k <> key) cache.entries in
  (* Add new entry *)
  let entries = (key, entry) :: entries in
  (* Trim if over limit *)
  let entries = if List.length entries > cache.max_entries then
    List.filteri (fun i _ -> i < cache.max_entries) entries
  else entries in
  cache.entries <- entries

(** Clear cache *)
let cache_clear cache =
  cache.entries <- []

(** {1 Statistics} *)

(** SSR statistics *)
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

(** Cache hit rate *)
let cache_hit_rate stats =
  let total = stats.cache_hits + stats.cache_misses in
  if total = 0 then 0.0
  else float_of_int stats.cache_hits /. float_of_int total

(** {1 Engine} *)

(** SSR engine *)
type t = {
  config: config;
  cache: cache option;
  stats: stats;
  mutable running: bool;
}

(** Create engine *)
let create config =
  let cache = if config.enable_cache then
    Some (create_cache config.max_cache_entries)
  else None in
  {
    config;
    cache;
    stats = create_stats ();
    running = true;
  }

(** Get stats *)
let get_stats engine = engine.stats

(** Shutdown engine *)
let shutdown engine =
  engine.running <- false

(** {1 Rendering} *)

(** Generate cache key *)
let cache_key ~url =
  url

(** Render page *)
let render engine ~url ?(props=`Assoc []) ?(islands=[]) () =
  let _ = props, islands in  (* Silence unused warnings *)
  if not engine.running then
    Error "Engine is not running"
  else begin
    engine.stats.total_renders <- engine.stats.total_renders + 1;

    (* Check cache first *)
    let key = cache_key ~url in
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
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Astro Page</title>
</head>
<body>
  <!-- Astro Islands SSR: %s -->
  <main>
    <slot></slot>
  </main>
  %s
</body>
</html>|} url (Island.hydration_script ()) in
        let elapsed = (Unix.gettimeofday () -. start) *. 1000.0 in
        engine.stats.avg_render_ms <-
          (engine.stats.avg_render_ms *. float_of_int (engine.stats.total_renders - 1) +. elapsed)
          /. float_of_int engine.stats.total_renders;
        (* Cache the result *)
        cache_put cache key html "" engine.config.cache_ttl_s;
        Ok html)
    | _ ->
      (* No cache, just render *)
      let html = Printf.sprintf {|<!DOCTYPE html>
<html lang="en">
<head><meta charset="UTF-8"></head>
<body>
  <!-- Astro: %s -->
  %s
</body>
</html>|} url (Island.hydration_script ()) in
      Ok html
  end

(** Render with fallback *)
let render_with_fallback engine ~url ~fallback =
  match render engine ~url () with
  | Ok html -> html
  | Error _ -> fallback

(** {1 Island Rendering} *)

(** Render single island *)
let render_island engine island ~ssr_html =
  let _ = engine in
  (* Wrap SSR HTML with island wrapper *)
  Island.render_wrapper island ssr_html

(** Render all islands on page *)
let render_islands engine islands =
  List.map (fun island ->
    (* In real impl, would render each component *)
    let placeholder = Printf.sprintf "<!-- Island: %s -->" island.Island.component in
    (island.Island.id, render_island engine island ~ssr_html:placeholder)
  ) islands

(** {1 Static Generation} *)

(** Generate static page *)
let generate_static engine ~url ~output_path () =
  match render engine ~url () with
  | Ok html ->
    (* Write to file *)
    Kirin.Fs_compat.save output_path html;
    Ok output_path
  | Error msg -> Error msg

(** Generate all static paths *)
let generate_all_static engine routes output_dir =
  List.filter_map (fun route ->
    if route.Route_def.prerender = Route_def.Static then
      let output_path = Filename.concat output_dir
        (if route.Route_def.path = "/" then "index.html"
         else route.Route_def.path ^ ".html") in
      Some (generate_static engine ~url:route.Route_def.path ~output_path ())
    else None
  ) routes

(** {1 Dev Server} *)

(** Hot reload script *)
let hot_reload_script port =
  Printf.sprintf {|<script>
(function() {
  const ws = new WebSocket('ws://localhost:%d/_astro/hmr');
  ws.onmessage = (e) => {
    if (e.data === 'reload') {
      location.reload();
    }
  };
})();
</script>|} port

(** Inject dev scripts *)
let inject_dev_scripts html port =
  let script = hot_reload_script port in
  Str.replace_first (Str.regexp "</body>") (script ^ "\n</body>") html
