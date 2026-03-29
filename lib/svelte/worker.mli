type status =
  | Ready
  | Busy
  | Crashed
  | Restarting

type t =
  { mutable pid : int option
  ; mutable status : status
  ; request_count : int Atomic.t
  ; mutable last_request : float
  ; error_count : int Atomic.t
  ; bundle_path : string
  ; max_requests : int
  ; memory_limit_mb : int
  }

val create : bundle:string -> max_requests:int -> memory_limit_mb:int -> unit -> t
val is_healthy : t -> bool
val needs_restart : t -> bool
val age : t -> float
val mark_busy : t -> unit
val mark_ready : t -> unit
val mark_crashed : t -> unit
val reset : t -> unit

val render
  :  t
  -> url:string
  -> props:Yojson.Safe.t
  -> ?route_id:string option
  -> ?cookies:(string * string) list
  -> ?headers:(string * string) list
  -> unit
  -> (Protocol.render_response, string) result

val health_check : t -> bool
val start : t -> (unit, 'a) result
val stop : t -> unit
val restart : t -> (unit, 'a) result

type stats =
  { requests : int
  ; errors : int
  ; uptime : float
  ; status : status
  }

val stats : t -> stats

val stats_to_json
  :  stats
  -> [> `Assoc of (string * [> `Float of float | `Int of int | `String of string ]) list ]
