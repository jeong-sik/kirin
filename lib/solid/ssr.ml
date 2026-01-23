(** Solid.js SSR Engine

    Server-side rendering with worker pool. *)

(** {1 Configuration} *)

(** SSR engine configuration *)
type config = {
  bundle: string;
  workers: int;
  timeout: float;
  max_requests_per_worker: int;
  memory_limit_mb: int;
  fallback_html: string option;
}

(** Default configuration *)
let default_config ~bundle = {
  bundle;
  workers = 4;
  timeout = 5.0;
  max_requests_per_worker = 5000;
  memory_limit_mb = 200;
  fallback_html = None;
}

(** {1 Worker Pool} *)

(** Worker pool *)
type t = {
  config: config;
  mutable pool: Worker.t array;
  mutable next_worker: int;
  mutable total_renders: int;
  mutable errors: int;
  mutable timeouts: int;
  cache: (string, Protocol.render_response) Hashtbl.t;
  cache_max_size: int;
}

(** Create SSR engine *)
let create config =
  let pool = Array.init config.workers (fun _ ->
    Worker.create
      ~bundle:config.bundle
      ~max_requests:config.max_requests_per_worker
      ~timeout:config.timeout
      ()
  ) in
  {
    config;
    pool;
    next_worker = 0;
    total_renders = 0;
    errors = 0;
    timeouts = 0;
    cache = Hashtbl.create 256;
    cache_max_size = 1000;
  }

(** Start all workers *)
let start engine =
  Array.iter (fun worker ->
    match Worker.start worker with
    | Ok () -> ()
    | Error msg ->
      Printf.eprintf "Failed to start SSR worker: %s\n" msg
  ) engine.pool

(** Stop all workers *)
let shutdown engine =
  Array.iter Worker.stop engine.pool

(** {1 Worker Selection} *)

(** Get next available worker (round-robin) *)
let get_worker engine =
  let start = engine.next_worker in
  let n = Array.length engine.pool in
  let rec find i =
    if i >= n then None
    else
      let idx = (start + i) mod n in
      let worker = engine.pool.(idx) in
      if Worker.is_ready worker then begin
        engine.next_worker <- (idx + 1) mod n;
        Some worker
      end else
        find (i + 1)
  in
  find 0

(** Ensure worker is running *)
let ensure_worker_ready worker =
  match Worker.get_state worker with
  | Worker.Stopped -> Worker.start worker
  | Worker.Unhealthy -> Worker.restart worker
  | _ -> Ok ()

(** {1 Rendering} *)

(** Cache key for request *)
let cache_key ~url ~props =
  Printf.sprintf "%s|%s" url (Yojson.Safe.to_string props)

(** Render with caching *)
let render_cached engine ~url ?(props = `Assoc []) () =
  let key = cache_key ~url ~props in

  (* Check cache *)
  match Hashtbl.find_opt engine.cache key with
  | Some response ->
    engine.total_renders <- engine.total_renders + 1;
    Ok response
  | None ->
    (* Get worker *)
    match get_worker engine with
    | None ->
      engine.errors <- engine.errors + 1;
      Error "No workers available"
    | Some worker ->
      let _ = ensure_worker_ready worker in
      match Worker.render worker ~url ~props () with
      | Ok response ->
        engine.total_renders <- engine.total_renders + 1;
        (* Cache if under limit *)
        if Hashtbl.length engine.cache < engine.cache_max_size then
          Hashtbl.replace engine.cache key response;
        Ok response
      | Error msg ->
        engine.errors <- engine.errors + 1;
        Error msg

(** Render without caching *)
let render engine ~url ?(props = `Assoc []) () =
  match get_worker engine with
  | None ->
    engine.errors <- engine.errors + 1;
    Error "No workers available"
  | Some worker ->
    let _ = ensure_worker_ready worker in
    match Worker.render worker ~url ~props () with
    | Ok response ->
      engine.total_renders <- engine.total_renders + 1;
      Ok response
    | Error msg ->
      engine.errors <- engine.errors + 1;
      Error msg

(** Render with fallback *)
let render_with_fallback engine ~url ?(props = `Assoc []) ~fallback () =
  match render engine ~url ~props () with
  | Ok response -> response.html
  | Error _ -> fallback

(** {1 Cache Management} *)

(** Clear render cache *)
let clear_cache engine =
  Hashtbl.clear engine.cache

(** Get cache size *)
let cache_size engine =
  Hashtbl.length engine.cache

(** Invalidate cache entry *)
let invalidate_cache engine ~url ~props =
  let key = cache_key ~url ~props in
  Hashtbl.remove engine.cache key

(** {1 Statistics} *)

(** Engine statistics *)
type stats = {
  total_renders: int;
  errors: int;
  timeouts: int;
  cache_size: int;
  cache_hit_rate: float;
  workers_ready: int;
  workers_total: int;
}

(** Get engine statistics *)
let stats engine =
  let workers_ready = Array.fold_left (fun acc w ->
    if Worker.is_ready w then acc + 1 else acc
  ) 0 engine.pool in
  let cache_hit_rate =
    if engine.total_renders > 0 then
      float_of_int (engine.total_renders - engine.errors) /.
      float_of_int engine.total_renders
    else 0.0
  in
  {
    total_renders = engine.total_renders;
    errors = engine.errors;
    timeouts = engine.timeouts;
    cache_size = Hashtbl.length engine.cache;
    cache_hit_rate;
    workers_ready;
    workers_total = Array.length engine.pool;
  }

(** {1 Health} *)

(** Check all workers health *)
let health_check engine =
  Array.map (fun worker ->
    match Worker.health_check worker with
    | Ok status -> (Worker.get_state worker, Some status)
    | Error _ -> (Worker.get_state worker, None)
  ) engine.pool

(** Restart unhealthy workers *)
let recover_unhealthy engine =
  Array.iter (fun worker ->
    if Worker.needs_restart worker then
      ignore (Worker.restart worker)
  ) engine.pool
