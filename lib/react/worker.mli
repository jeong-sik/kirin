type status = Idle | Busy | Restarting | Dead
val status_to_string : status -> string
type stats = {
  requests_handled : int;
  errors : int;
  avg_response_time_ms : float;
  last_request_time : float option;
  memory_mb : int option;
  uptime_seconds : float;
}
val empty_stats : stats
type config = {
  bundle_path : string;
  timeout_ms : int;
  memory_limit_mb : int;
  restart_after_requests : int;
  restart_after_errors : int;
  env : (string * string) list;
}
val default_config : config
module type WORKER =
  sig
    type t
    val create : config -> t
    val render :
      t -> url:string -> props:Yojson.Safe.t -> (string, string) result
    val health_check : t -> bool
    val stats : t -> stats
    val status : t -> status
    val restart : t -> unit
    val close : t -> unit
  end
type event =
    Started
  | Request_started of { id : int; url : string; }
  | Request_completed of { id : int; duration_ms : float; }
  | Request_failed of { id : int; error : string; }
  | Memory_warning of { current_mb : int; limit_mb : int; }
  | Restarting of { reason : string; }
  | Stopped
type event_handler = event -> unit
val null_handler : event_handler
val log_handler : event_handler
module type POOL =
  sig
    type t
    type worker
    val create :
      size:int -> config:config -> ?on_event:event_handler -> unit -> t
    val render :
      t -> url:string -> props:Yojson.Safe.t -> (string, string) result
    val render_any :
      t -> url:string -> props:Yojson.Safe.t -> (string, string) result
    val health : t -> bool
    val stats : t -> stats list
    val shutdown : t -> unit
  end
module Render_cache :
  sig
    type entry = { html : string; created_at : float; ttl_seconds : float; }
    type t = (string, entry) Hashtbl.t
    val create : unit -> t
    val cache_key : url:string -> props:Yojson.Safe.t -> string
    val get :
      (string, entry) Hashtbl.t ->
      url:string -> props:Yojson.Safe.t -> string option
    val set :
      (string, entry) Hashtbl.t ->
      url:string ->
      props:Yojson.Safe.t -> html:string -> ttl_seconds:float -> unit
    val clear : ('a, 'b) Hashtbl.t -> unit
    val size : ('a, 'b) Hashtbl.t -> int
  end
val placeholder_render : url:string -> props:Yojson.Safe.t -> string
