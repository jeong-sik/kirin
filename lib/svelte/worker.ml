(** Svelte SSR Worker

    Worker interface for Node.js Svelte SSR processes. *)

(** {1 Worker Types} *)

(** Worker status *)
type status =
  | Ready
  | Busy
  | Crashed
  | Restarting

(** Worker state *)
type t = {
  mutable pid: int option;
  mutable status: status;
  mutable request_count: int;
  mutable last_request: float;
  mutable error_count: int;
  bundle_path: string;
  max_requests: int;
  memory_limit_mb: int;
}

(** {1 Worker Creation} *)

(** Create new worker *)
let create ~bundle ~max_requests ~memory_limit_mb () = {
  pid = None;
  status = Ready;
  request_count = 0;
  last_request = 0.0;
  error_count = 0;
  bundle_path = bundle;
  max_requests;
  memory_limit_mb;
}

(** {1 Worker Status} *)

(** Check if worker is healthy *)
let is_healthy worker =
  worker.status = Ready || worker.status = Busy

(** Check if worker needs restart *)
let needs_restart worker =
  worker.request_count >= worker.max_requests ||
  worker.error_count >= 5 ||
  worker.status = Crashed

(** Get worker age in seconds *)
let age worker =
  if worker.last_request = 0.0 then 0.0
  else Unix.gettimeofday () -. worker.last_request

(** {1 Request Handling} *)

(** Mark worker as busy *)
let mark_busy worker =
  worker.status <- Busy;
  worker.last_request <- Unix.gettimeofday ()

(** Mark worker as ready *)
let mark_ready worker =
  worker.status <- Ready;
  worker.request_count <- worker.request_count + 1

(** Mark worker as crashed *)
let mark_crashed worker =
  worker.status <- Crashed;
  worker.error_count <- worker.error_count + 1

(** Reset worker state *)
let reset worker =
  worker.status <- Ready;
  worker.request_count <- 0;
  worker.error_count <- 0

(** {1 Render Interface} *)

(** Render request *)
let render worker ~url ~props ?(route_id=None) ?(cookies=[]) ?(headers=[]) () =
  if not (is_healthy worker) then
    Error "Worker not healthy"
  else begin
    mark_busy worker;

    (* In real implementation, this would:
       1. Send request to Node.js process via stdin
       2. Read response from stdout
       3. Parse JSON-RPC response *)
    let request = Protocol.({
      url;
      props;
      route_id;
      cookies;
      headers;
    }) in
    let _id, _encoded = Protocol.encode_render_request request in

    (* Simulate response - real impl would read from process *)
    let result = Protocol.({
      html = Printf.sprintf "<div>SSR: %s</div>" url;
      head = "<title>SSR</title>";
      css = None;
      error = None;
      status = 200;
      redirect = None;
      data = Some props;
    }) in

    mark_ready worker;
    Ok result
  end

(** {1 Health Check} *)

(** Check worker health *)
let health_check worker =
  if worker.status = Crashed then false
  else if needs_restart worker then false
  else true

(** {1 Lifecycle} *)

(** Start worker process *)
let start worker =
  (* In real implementation, would spawn Node.js process:
     node --expose-gc <bundle_path> *)
  worker.pid <- Some (Unix.getpid ());  (* Placeholder *)
  worker.status <- Ready;
  Ok ()

(** Stop worker process *)
let stop worker =
  worker.status <- Crashed;
  worker.pid <- None;
  ()

(** Restart worker *)
let restart worker =
  stop worker;
  reset worker;
  start worker

(** {1 Stats} *)

(** Worker statistics *)
type stats = {
  requests: int;
  errors: int;
  uptime: float;
  status: status;
}

(** Get worker stats *)
let stats worker = {
  requests = worker.request_count;
  errors = worker.error_count;
  uptime = age worker;
  status = worker.status;
}

(** Stats to JSON *)
let stats_to_json stats =
  `Assoc [
    ("requests", `Int stats.requests);
    ("errors", `Int stats.errors);
    ("uptime", `Float stats.uptime);
    ("status", `String (match stats.status with
      | Ready -> "ready"
      | Busy -> "busy"
      | Crashed -> "crashed"
      | Restarting -> "restarting"));
  ]
