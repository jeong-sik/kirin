type t =
  { transport : Transport.t
  ; mutable server_capabilities : Protocol.server_capabilities option
  ; mutable server_info : Protocol.implementation_info option
  ; mutable request_id : int
  }

exception Client_error of string

val create : Transport.t -> t
val next_id : t -> Jsonrpc.id

val send_request
  :  t
  -> method_:string
  -> ?params:Yojson.Safe.t
  -> unit
  -> Yojson.Safe.t option

val send_notification : t -> method_:string -> ?params:Yojson.Safe.t -> unit -> unit

val initialize
  :  t
  -> ?client_name:string
  -> ?client_version:string
  -> unit
  -> Protocol.server_capabilities option

val list_tools : t -> Protocol.tool list

val call_tool
  :  t
  -> name:string
  -> ?arguments:Yojson.Safe.t
  -> unit
  -> Protocol.tool_result

val list_resources : t -> Protocol.resource list
val read_resource : t -> uri:string -> Protocol.resource_contents
val list_prompts : t -> Protocol.prompt list
val ping : t -> bool
val server_capabilities : t -> Protocol.server_capabilities option
val server_info : t -> Protocol.implementation_info option
val is_initialized : t -> bool
