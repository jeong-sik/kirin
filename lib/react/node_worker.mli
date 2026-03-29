type process =
  { pid : int
  ; in_ch : in_channel
  ; out_ch : out_channel
  }

type t =
  { mutable process : process option
  ; config : Worker.config
  ; mutable stats : Worker.stats
  ; mutable status : Worker.status
  ; mutable request_count : int
  ; mutable error_count : int
  ; start_time : float
  ; on_event : Worker.event_handler
  }

val start_process : Worker.config -> process option
val create : ?on_event:Worker.event_handler -> Worker.config -> t
val send_request : t -> Protocol.request -> (Protocol.response, string) Result.t
val render : t -> url:string -> props:Yojson.Safe.t -> (string, string) Result.t
val should_restart : t -> bool
val restart : t -> unit
val health_check : t -> bool
val stats : t -> Worker.stats
val status : t -> Worker.status
val close : t -> unit

val render_with_restart
  :  t
  -> url:string
  -> props:Yojson.Safe.t
  -> (string, string) Result.t

module Impl : Worker.WORKER with type t = t
