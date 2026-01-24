(** Vue/Nuxt SSR Worker Interface

    Abstract worker interface for SSR rendering. *)

(** {1 Worker State} *)

(** Worker state *)
type state =
  | Idle
  | Busy
  | Warming
  | Failed
  | Shutdown

(** Worker stats *)
type stats = {
  total_requests: int;
  successful_requests: int;
  failed_requests: int;
  total_render_time_ms: float;
  last_request_time: float option;
  memory_mb: int;
}

(** {1 Worker Interface} *)

(** Worker capabilities *)
module type WORKER = sig
  type t

  (** Create a new worker *)
  val create : bundle:string -> unit -> t

  (** Render a page *)
  val render :
    t ->
    url:string ->
    ?headers:(string * string) list ->
    ?payload:Yojson.Safe.t ->
    unit ->
    (Protocol.render_result, string) result

  (** Health check *)
  val health : t -> (Protocol.response_result, string) result

  (** Warmup the worker *)
  val warmup : t -> (unit, string) result

  (** Get worker state *)
  val state : t -> state

  (** Get worker stats *)
  val stats : t -> stats

  (** Check if worker is healthy *)
  val is_healthy : t -> bool

  (** Shutdown the worker *)
  val shutdown : t -> unit
end

(** {1 Worker Configuration} *)

(** Worker config *)
type config = {
  bundle: string;
  timeout_ms: int;
  max_requests: int;
  memory_limit_mb: int;
  warmup_on_create: bool;
  env: (string * string) list;
}

(** Default config *)
let default_config = {
  bundle = "";
  timeout_ms = 5000;
  max_requests = 10000;
  memory_limit_mb = 512;
  warmup_on_create = true;
  env = [];
}

(** {1 Worker Events} *)

(** Worker event *)
type event =
  | Created
  | Started
  | RenderStarted of string  (* URL *)
  | RenderCompleted of string * float  (* URL, time_ms *)
  | RenderFailed of string * string  (* URL, error *)
  | HealthCheckPassed
  | HealthCheckFailed of string
  | MemoryWarning of int  (* MB *)
  | Restarting of string  (* reason *)
  | Shutdown

(** Event callback *)
type event_handler = event -> unit

(** {1 Worker Pool Config} *)

(** Pool config *)
type pool_config = {
  worker_config: config;
  min_workers: int;
  max_workers: int;
  scale_threshold: float;  (* CPU/request ratio *)
  idle_timeout_s: int;
  health_check_interval_s: int;
}

(** Default pool config *)
let default_pool_config = {
  worker_config = default_config;
  min_workers = 2;
  max_workers = 8;
  scale_threshold = 0.8;
  idle_timeout_s = 300;
  health_check_interval_s = 30;
}

(** {1 Helper Functions} *)

(** Create empty stats *)
let empty_stats = {
  total_requests = 0;
  successful_requests = 0;
  failed_requests = 0;
  total_render_time_ms = 0.0;
  last_request_time = None;
  memory_mb = 0;
}

(** Calculate average render time *)
let avg_render_time stats =
  if stats.successful_requests = 0 then 0.0
  else stats.total_render_time_ms /. Float.of_int stats.successful_requests

(** State to string *)
let state_to_string = function
  | Idle -> "idle"
  | Busy -> "busy"
  | Warming -> "warming"
  | Failed -> "failed"
  | Shutdown -> "shutdown"

(** Event to string *)
let event_to_string = function
  | Created -> "worker.created"
  | Started -> "worker.started"
  | RenderStarted url -> Printf.sprintf "render.started[%s]" url
  | RenderCompleted (url, time) -> Printf.sprintf "render.completed[%s %.2fms]" url time
  | RenderFailed (url, err) -> Printf.sprintf "render.failed[%s: %s]" url err
  | HealthCheckPassed -> "health.passed"
  | HealthCheckFailed reason -> Printf.sprintf "health.failed[%s]" reason
  | MemoryWarning mb -> Printf.sprintf "memory.warning[%dMB]" mb
  | Restarting reason -> Printf.sprintf "worker.restarting[%s]" reason
  | Shutdown -> "worker.shutdown"

(** {1 Serialization} *)

(** Stats to JSON *)
let stats_to_json stats =
  `Assoc [
    ("totalRequests", `Int stats.total_requests);
    ("successfulRequests", `Int stats.successful_requests);
    ("failedRequests", `Int stats.failed_requests);
    ("avgRenderTimeMs", `Float (avg_render_time stats));
    ("memoryMb", `Int stats.memory_mb);
  ]

(** Config to JSON *)
let config_to_json config =
  `Assoc [
    ("bundle", `String config.bundle);
    ("timeoutMs", `Int config.timeout_ms);
    ("maxRequests", `Int config.max_requests);
    ("memoryLimitMb", `Int config.memory_limit_mb);
    ("warmupOnCreate", `Bool config.warmup_on_create);
  ]
