type state =
  | Starting
  | Ready
  | Busy
  | Unhealthy
  | Stopped

type t =
  { mutable pid : int option
  ; mutable state : state
  ; request_count : int Atomic.t
  ; mutable last_health_check : float
  ; mutable stdin : out_channel option
  ; mutable stdout : in_channel option
  ; bundle : string
  ; max_requests : int
  ; timeout : float
  }

val create : bundle:string -> ?max_requests:int -> ?timeout:float -> unit -> t
val start : t -> (unit, string) result
val stop : t -> unit
val restart : t -> (unit, string) result
val send_request : t -> string -> (string, string) result
val health_check : t -> (Protocol.health_status, string) result

val render
  :  t
  -> url:string
  -> ?props:Yojson.Safe.t
  -> unit
  -> (Protocol.render_response, string) result

val get_state : t -> state
val get_request_count : t -> int
val is_ready : t -> bool
val needs_restart : t -> bool
