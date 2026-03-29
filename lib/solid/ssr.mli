type config =
  { bundle : string
  ; workers : int
  ; timeout : float
  ; max_requests_per_worker : int
  ; memory_limit_mb : int
  ; fallback_html : string option
  }

val default_config : bundle:string -> config

type t =
  { config : config
  ; mutable pool : Worker.t array
  ; next_worker : int Atomic.t
  ; total_renders : int Atomic.t
  ; errors : int Atomic.t
  ; timeouts : int Atomic.t
  ; cache : (string, Protocol.render_response) Hashtbl.t
  ; cache_max_size : int
  }

val create : config -> t
val start : t -> unit
val shutdown : t -> unit
val get_worker : t -> Worker.t option
val ensure_worker_ready : Worker.t -> (unit, string) result
val cache_key : url:string -> props:Yojson.Safe.t -> string

val render_cached
  :  t
  -> url:string
  -> ?props:Yojson.Safe.t
  -> unit
  -> (Protocol.render_response, string) result

val render
  :  t
  -> url:string
  -> ?props:Yojson.Safe.t
  -> unit
  -> (Protocol.render_response, string) result

val render_with_fallback
  :  t
  -> url:string
  -> ?props:Yojson.Safe.t
  -> fallback:string
  -> unit
  -> string

val clear_cache : t -> unit
val cache_size : t -> int
val invalidate_cache : t -> url:string -> props:Yojson.Safe.t -> unit

type stats =
  { total_renders : int
  ; errors : int
  ; timeouts : int
  ; cache_size : int
  ; cache_hit_rate : float
  ; workers_ready : int
  ; workers_total : int
  }

val stats : t -> stats
val health_check : t -> (Worker.state * Protocol.health_status option) array
val recover_unhealthy : t -> unit
