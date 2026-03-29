type config =
  { bundle : string
  ; workers : int
  ; timeout : float
  ; memory_limit_mb : int
  ; restart_after : int
  }

val default_config : config

type t =
  { workers : Node_worker.t array
  ; config : config
  ; mutable next_worker : int
  ; mutable total_renders : int
  ; mutable total_errors : int
  ; mutable total_timeouts : int
  ; mutable cache_hits : int
  ; mutable cache_misses : int
  ; cache : Worker.Render_cache.t
  ; on_event : Worker.event_handler
  }

val create : ?on_event:Worker.event_handler -> config -> t
val get_worker : t -> Node_worker.t option
val render : t -> url:string -> ?props:Yojson.Safe.t -> unit -> (string, string) Result.t

val render_with_fallback
  :  t
  -> url:string
  -> ?props:Yojson.Safe.t
  -> fallback:string
  -> unit
  -> string

val render_full
  :  t
  -> url:string
  -> ?props:Yojson.Safe.t
  -> unit
  -> (Protocol.render_result, string) Result.t

type stats =
  { total_renders : int
  ; errors : int
  ; timeouts : int
  ; avg_time_ms : float
  ; cache_size : int
  ; cache_hit_rate : float
  ; active_workers : int
  ; idle_workers : int
  }

val stats : t -> stats
val health : t -> bool
val shutdown : t -> unit
val clear_cache : t -> unit
val warmup : t -> urls:string list -> unit
val handler : t -> Kirin.Request.t -> Kirin.Response.t
val handler_with_fallback : fallback:string -> t -> Kirin.Request.t -> Kirin.Response.t

val render_hydrated
  :  t
  -> url:string
  -> ?props:Yojson.Safe.t
  -> manifest:Manifest.t
  -> entry:string
  -> unit
  -> (string, string) Result.t
