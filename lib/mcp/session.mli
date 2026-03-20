type state = Uninitialized | Initializing | Ready | Closing | Closed
type t = {
  mutable state : state;
  mutable client_info : Protocol.implementation_info option;
  mutable client_capabilities :
    Protocol.client_capabilities option;
  mutable server_info : Protocol.implementation_info;
  mutable server_capabilities : Protocol.server_capabilities;
  mutable request_id : int;
  mutable session_id : string option;
}
val generate_session_id : unit -> string
val create : server_name:string -> server_version:string -> unit -> t
val state : t -> state
val is_ready : t -> bool
val is_closed : t -> bool
val enable_tools : t -> unit
val enable_resources : t -> unit
val enable_prompts : t -> unit
val enable_logging : t -> unit
val server_capabilities : t -> Protocol.server_capabilities
val client_capabilities :
  t -> Protocol.client_capabilities option
val handle_initialize :
  t ->
  Protocol.initialize_params ->
  (Protocol.initialize_result, string) result
val handle_initialized : t -> (unit, string) result
val next_request_id : t -> Jsonrpc.id
val begin_shutdown : t -> unit
val complete_shutdown : t -> unit
val server_info : t -> Protocol.implementation_info
val client_info : t -> Protocol.implementation_info option
val session_id : t -> string option
