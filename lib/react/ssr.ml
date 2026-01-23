(** SSR Engine with Worker Pool

    Manages a pool of Node.js workers for React server-side rendering.
    Provides load balancing, failover, and automatic restarts.
*)

(** SSR configuration *)
type config = {
  bundle: string;
  workers: int;
  timeout: float;
  memory_limit_mb: int;
  restart_after: int;
}

let default_config = {
  bundle = "dist/server/entry-server.js";
  workers = 4;
  timeout = 5.0;
  memory_limit_mb = 200;
  restart_after = 5000;
}

(** SSR engine state *)
type t = {
  workers: Node_worker.t array;
  config: config;
  mutable next_worker: int;
  mutable total_renders: int;
  mutable total_errors: int;
  mutable total_timeouts: int;
  mutable cache_hits: int;
  mutable cache_misses: int;
  cache: Worker.Render_cache.t;
  on_event: Worker.event_handler;
}

(** Create SSR engine with worker pool *)
let create ?(on_event = Worker.null_handler) config =
  let worker_config = {
    Worker.bundle_path = config.bundle;
    timeout_ms = int_of_float (config.timeout *. 1000.0);
    memory_limit_mb = config.memory_limit_mb;
    restart_after_requests = config.restart_after;
    restart_after_errors = 10;
    env = [];
  } in
  let workers = Array.init config.workers (fun _ ->
    Node_worker.create ~on_event worker_config
  ) in
  {
    workers;
    config;
    next_worker = 0;
    total_renders = 0;
    total_errors = 0;
    total_timeouts = 0;
    cache_hits = 0;
    cache_misses = 0;
    cache = Worker.Render_cache.create ();
    on_event;
  }

(** Get next available worker (round-robin) *)
let get_worker engine =
  let n = Array.length engine.workers in
  let start = engine.next_worker in

  (* Find first idle worker starting from next_worker *)
  let rec find_idle i =
    if i >= n then None
    else
      let idx = (start + i) mod n in
      let w = engine.workers.(idx) in
      if Node_worker.status w = Worker.Idle then begin
        engine.next_worker <- (idx + 1) mod n;
        Some w
      end
      else find_idle (i + 1)
  in

  match find_idle 0 with
  | Some w -> Some w
  | None ->
    (* All busy, just use next in round-robin *)
    let idx = engine.next_worker in
    engine.next_worker <- (idx + 1) mod n;
    Some engine.workers.(idx)

(** Render URL to HTML *)
let render engine ~url ?(props = `Assoc []) () =
  (* Check cache first *)
  match Worker.Render_cache.get engine.cache ~url ~props with
  | Some html ->
    engine.cache_hits <- engine.cache_hits + 1;
    Ok html
  | None ->
    engine.cache_misses <- engine.cache_misses + 1;
    match get_worker engine with
    | None ->
      engine.total_errors <- engine.total_errors + 1;
      Result.Error "No workers available"
    | Some worker ->
      engine.total_renders <- engine.total_renders + 1;
      match Node_worker.render_with_restart worker ~url ~props with
      | Ok html ->
        (* Cache successful renders *)
        Worker.Render_cache.set engine.cache ~url ~props ~html ~ttl_seconds:60.0;
        Ok html
      | Error msg ->
        engine.total_errors <- engine.total_errors + 1;
        Result.Error msg

(** Render with fallback HTML on error *)
let render_with_fallback engine ~url ?(props = `Assoc []) ~fallback () =
  match render engine ~url ~props () with
  | Ok html -> html
  | Error _ -> fallback

(** Render and return full result with head tags *)
let render_full engine ~url ?(props = `Assoc []) () =
  match get_worker engine with
  | None ->
    engine.total_errors <- engine.total_errors + 1;
    Result.Error "No workers available"
  | Some worker ->
    engine.total_renders <- engine.total_renders + 1;
    let id = Protocol.next_id () in
    let req = Protocol.render_request ~id ~url ~props () in
    match Node_worker.send_request worker req with
    | Ok response ->
      Protocol.extract_render_result response
    | Error msg ->
      engine.total_errors <- engine.total_errors + 1;
      Result.Error msg

(** Pool statistics *)
type stats = {
  total_renders: int;
  errors: int;
  timeouts: int;
  avg_time_ms: float;
  cache_size: int;
  cache_hit_rate: float;
  active_workers: int;
  idle_workers: int;
}

let stats engine =
  let worker_stats = Array.map Node_worker.stats engine.workers in
  let total_time = Array.fold_left (fun acc s ->
    acc +. (s.Worker.avg_response_time_ms *. float_of_int s.requests_handled)
  ) 0.0 worker_stats in
  let total_requests = Array.fold_left (fun acc s ->
    acc + s.Worker.requests_handled
  ) 0 worker_stats in
  let avg_time = if total_requests > 0 then total_time /. float_of_int total_requests else 0.0 in

  let active, idle = Array.fold_left (fun (a, i) w ->
    match Node_worker.status w with
    | Worker.Idle -> (a, i + 1)
    | Worker.Busy -> (a + 1, i)
    | _ -> (a, i)
  ) (0, 0) engine.workers in

  let cache_total = engine.cache_hits + engine.cache_misses in
  let cache_hit_rate =
    if cache_total > 0 then float_of_int engine.cache_hits /. float_of_int cache_total
    else 0.0
  in

  {
    total_renders = engine.total_renders;
    errors = engine.total_errors;
    timeouts = engine.total_timeouts;
    avg_time_ms = avg_time;
    cache_size = Worker.Render_cache.size engine.cache;
    cache_hit_rate;
    active_workers = active;
    idle_workers = idle;
  }

(** Health check all workers *)
let health engine =
  Array.exists Node_worker.health_check engine.workers

(** Shutdown all workers *)
let shutdown engine =
  Array.iter Node_worker.close engine.workers

(** Clear render cache *)
let clear_cache engine =
  Worker.Render_cache.clear engine.cache

(** Warm up workers with initial requests *)
let warmup engine ~urls =
  List.iter (fun url ->
    let _ = render engine ~url () in
    ()
  ) urls

(** Create Kirin handler from SSR engine *)
let handler engine =
  fun (req : Kirin.Request.t) ->
    let url = Kirin.Request.path req in
    match render engine ~url () with
    | Ok html -> Kirin.Response.html html
    | Error msg ->
      Kirin.Response.text ~status:`Internal_server_error ("SSR Error: " ^ msg)

(** Create Kirin handler with custom fallback *)
let handler_with_fallback ~fallback engine =
  fun (req : Kirin.Request.t) ->
    let url = Kirin.Request.path req in
    let html = render_with_fallback engine ~url ~fallback () in
    Kirin.Response.html html

(** Integration: render into hydration shell *)
let render_hydrated engine ~url ?(props = `Assoc []) ~manifest ~entry () =
  match render_full engine ~url ~props () with
  | Ok result ->
    let options = {
      Hydrate.default_options with
      title = "";  (* Would extract from head *)
      initial_data = Some props;
      scripts = [match Manifest.resolve manifest entry with Some f -> f | None -> entry];
      styles = Manifest.css_for manifest entry;
      head_extra = (match result.head with Some h -> [h] | None -> []);
    } in
    (* Would inject SSR HTML into root div *)
    Ok (Hydrate.render options)
  | Error msg -> Result.Error msg
