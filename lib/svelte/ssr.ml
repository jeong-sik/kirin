(** Svelte SSR Engine

    Server-side rendering engine with worker pool and caching. *)

(** {1 Configuration} *)

(** SSR engine configuration *)
type config = {
  bundle: string;
  num_workers: int;
  timeout: float;
  memory_limit_mb: int;
  restart_after: int;
  cache_ttl: int;
  cache_max_size: int;
}

(** Default configuration *)
let default_config = {
  bundle = "dist/server/entry-server.js";
  num_workers = 4;
  timeout = 5.0;
  memory_limit_mb = 200;
  restart_after = 5000;
  cache_ttl = 60;
  cache_max_size = 100;
}

(** {1 Cache} *)

(** Cache entry *)
type cache_entry = {
  response: Protocol.render_response;
  expires_at: float;
}

(** Simple LRU cache *)
type cache = {
  mutable entries: (string, cache_entry) Hashtbl.t;
  max_size: int;
  ttl: int;
}

(** Create cache *)
let create_cache ~max_size ~ttl = {
  entries = Hashtbl.create max_size;
  max_size;
  ttl;
}

(** Cache key from request *)
let cache_key url props =
  let props_hash = Hashtbl.hash (Yojson.Safe.to_string props) in
  Printf.sprintf "%s:%d" url props_hash

(** Get from cache *)
let cache_get cache key =
  match Hashtbl.find_opt cache.entries key with
  | Some entry when entry.expires_at > Unix.gettimeofday () ->
    Some entry.response
  | Some _ ->
    Hashtbl.remove cache.entries key;
    None
  | None -> None

(** Put in cache *)
let cache_put cache key response =
  (* Simple eviction: if full, clear half *)
  if Hashtbl.length cache.entries >= cache.max_size then begin
    let to_remove = ref [] in
    let count = ref 0 in
    Hashtbl.iter (fun k _ ->
      if !count < cache.max_size / 2 then begin
        to_remove := k :: !to_remove;
        incr count
      end
    ) cache.entries;
    List.iter (Hashtbl.remove cache.entries) !to_remove
  end;

  let entry = {
    response;
    expires_at = Unix.gettimeofday () +. float_of_int cache.ttl;
  } in
  Hashtbl.replace cache.entries key entry

(** {1 Worker Pool} *)

(** SSR engine state *)
type t = {
  config: config;
  workers: Worker.t array;
  cache: cache;
  mutable next_worker: int;
  mutable total_renders: int;
  mutable cache_hits: int;
  mutable errors: int;
}

(** Create SSR engine *)
let create config =
  let workers = Array.init config.num_workers (fun _ ->
    Worker.create
      ~bundle:config.bundle
      ~max_requests:config.restart_after
      ~memory_limit_mb:config.memory_limit_mb
      ()
  ) in
  let cache = create_cache ~max_size:config.cache_max_size ~ttl:config.cache_ttl in
  {
    config;
    workers;
    cache;
    next_worker = 0;
    total_renders = 0;
    cache_hits = 0;
    errors = 0;
  }

(** Get next available worker (round-robin) *)
let get_worker engine =
  let start = engine.next_worker in
  let rec find_worker i =
    if i >= Array.length engine.workers then None
    else
      let idx = (start + i) mod Array.length engine.workers in
      let worker = engine.workers.(idx) in
      if Worker.is_healthy worker then begin
        engine.next_worker <- (idx + 1) mod Array.length engine.workers;
        Some worker
      end
      else find_worker (i + 1)
  in
  find_worker 0

(** {1 Rendering} *)

(** Render URL to HTML *)
let render engine ~url ?(props=`Assoc []) ?route_id ?(use_cache=true) () =
  engine.total_renders <- engine.total_renders + 1;

  (* Check cache first *)
  let key = cache_key url props in
  let cached =
    if use_cache then cache_get engine.cache key
    else None
  in
  match cached with
  | Some response ->
    engine.cache_hits <- engine.cache_hits + 1;
    Ok response
  | None ->
    (* Get worker and render *)
    match get_worker engine with
    | None ->
      engine.errors <- engine.errors + 1;
      Error "No healthy workers available"
    | Some worker ->
      match Worker.render worker ~url ~props ~route_id ~cookies:[] ~headers:[] () with
      | Ok response ->
        if use_cache then cache_put engine.cache key response;
        Ok response
      | Error msg ->
        engine.errors <- engine.errors + 1;
        Error msg

(** Render with fallback *)
let render_with_fallback engine ~url ?(props=`Assoc []) ~fallback () =
  match render engine ~url ~props () with
  | Ok response -> response.Protocol.html
  | Error _ -> fallback

(** {1 Preloading} *)

(** Preload routes into cache *)
let preload engine urls =
  List.iter (fun url ->
    let _ = render engine ~url ~use_cache:true () in ()
  ) urls

(** {1 Cache Management} *)

(** Invalidate cache entry *)
let invalidate engine url =
  let prefix = url ^ ":" in
  let to_remove = ref [] in
  Hashtbl.iter (fun k _ ->
    if String.length k >= String.length prefix &&
       String.sub k 0 (String.length prefix) = prefix then
      to_remove := k :: !to_remove
  ) engine.cache.entries;
  List.iter (Hashtbl.remove engine.cache.entries) !to_remove

(** Clear entire cache *)
let clear_cache engine =
  Hashtbl.clear engine.cache.entries

(** {1 Worker Management} *)

(** Check and restart unhealthy workers *)
let maintain_workers engine =
  Array.iter (fun worker ->
    if Worker.needs_restart worker then
      let _ = Worker.restart worker in ()
  ) engine.workers

(** Get ready worker count *)
let ready_workers engine =
  Array.fold_left (fun acc worker ->
    if Worker.is_healthy worker then acc + 1 else acc
  ) 0 engine.workers

(** {1 Statistics} *)

(** Engine statistics *)
type stats = {
  total_renders: int;
  cache_hits: int;
  cache_hit_rate: float;
  errors: int;
  cache_size: int;
  workers_ready: int;
  workers_total: int;
}

(** Get engine statistics *)
let get_stats (engine : t) =
  let hit_rate =
    if engine.total_renders = 0 then 0.0
    else float_of_int engine.cache_hits /. float_of_int engine.total_renders
  in
  {
    total_renders = engine.total_renders;
    cache_hits = engine.cache_hits;
    cache_hit_rate = hit_rate;
    errors = engine.errors;
    cache_size = Hashtbl.length engine.cache.entries;
    workers_ready = ready_workers engine;
    workers_total = Array.length engine.workers;
  }

(** Stats to JSON *)
let stats_to_json stats =
  `Assoc [
    ("totalRenders", `Int stats.total_renders);
    ("cacheHits", `Int stats.cache_hits);
    ("cacheHitRate", `Float stats.cache_hit_rate);
    ("errors", `Int stats.errors);
    ("cacheSize", `Int stats.cache_size);
    ("workersReady", `Int stats.workers_ready);
    ("workersTotal", `Int stats.workers_total);
  ]

(** {1 Lifecycle} *)

(** Start all workers *)
let start engine =
  Array.iter (fun worker ->
    let _ = Worker.start worker in ()
  ) engine.workers

(** Shutdown engine *)
let shutdown engine =
  Array.iter Worker.stop engine.workers;
  clear_cache engine
