type state = Idle | Busy | Warming | Failed | Shutdown
type stats = {
  total_requests : int;
  successful_requests : int;
  failed_requests : int;
  total_render_time_ms : float;
  last_request_time : float option;
  memory_mb : int;
}
module type WORKER =
  sig
    type t
    val create : bundle:string -> unit -> t
    val render :
      t ->
      url:string ->
      ?headers:(string * string) list ->
      ?payload:Yojson.Safe.t ->
      unit -> (Protocol.render_result, string) result
    val health : t -> (Protocol.response_result, string) result
    val warmup : t -> (unit, string) result
    val state : t -> state
    val stats : t -> stats
    val is_healthy : t -> bool
    val shutdown : t -> unit
  end
type config = {
  bundle : string;
  timeout_ms : int;
  max_requests : int;
  memory_limit_mb : int;
  warmup_on_create : bool;
  env : (string * string) list;
}
val default_config : config
type event =
    Created
  | Started
  | RenderStarted of string
  | RenderCompleted of string * float
  | RenderFailed of string * string
  | HealthCheckPassed
  | HealthCheckFailed of string
  | MemoryWarning of int
  | Restarting of string
  | Shutdown
type event_handler = event -> unit
type pool_config = {
  worker_config : config;
  min_workers : int;
  max_workers : int;
  scale_threshold : float;
  idle_timeout_s : int;
  health_check_interval_s : int;
}
val default_pool_config : pool_config
val empty_stats : stats
val avg_render_time : stats -> float
val state_to_string : state -> string
val event_to_string : event -> string
val stats_to_json :
  stats -> [> `Assoc of (string * [> `Float of float | `Int of int ]) list ]
val config_to_json :
  config ->
  [> `Assoc of
       (string * [> `Bool of bool | `Int of int | `String of string ]) list ]
