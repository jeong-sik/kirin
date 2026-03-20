type eio_clock = float Eio.Time.clock_ty Eio.Resource.t
type eio_ctx = { sw : Eio.Switch.t; clock : eio_clock; }
type tool_handler =
    Sync of (Yojson.Safe.t -> Yojson.Safe.t)
  | Async of (eio_ctx -> Yojson.Safe.t -> Yojson.Safe.t)
type resource_handler = unit -> string * string
type prompt_handler =
    (string * string) list -> Protocol.prompt_message list
type registered_tool = {
  tool : Protocol.tool;
  handler : tool_handler;
}
type registered_resource = {
  resource : Protocol.resource;
  handler : resource_handler;
}
type registered_prompt = {
  prompt : Protocol.prompt;
  handler : prompt_handler option;
}
type t = {
  mutable tools : registered_tool list;
  mutable resources : registered_resource list;
  mutable prompts : registered_prompt list;
  logging : Logging.t;
  session : Session.t;
  task_registry : Tasks.registry;
}
val create :
  ?name:string ->
  ?version:string ->
  ?log_level:Logging.log_level ->
  ?log_handler:(Logging.log_message -> unit) -> unit -> t
val add_tool :
  t ->
  name:string ->
  description:string ->
  schema:Yojson.Safe.t ->
  ?annotations:Protocol.tool_annotations ->
  ?icon:Protocol.icon -> handler:tool_handler -> unit -> unit
val add_tool_sync :
  t ->
  name:string ->
  description:string ->
  schema:Yojson.Safe.t ->
  ?annotations:Protocol.tool_annotations ->
  ?icon:Protocol.icon ->
  handler:(Yojson.Safe.t -> Yojson.Safe.t) -> unit -> unit
val list_tools : t -> Protocol.tool list
val call_tool :
  t ->
  ctx:eio_ctx ->
  name:string ->
  arguments:Yojson.Safe.t option ->
  (Protocol.tool_result, string) result
val add_resource :
  t ->
  uri:string ->
  name:string ->
  ?description:string ->
  ?mime_type:string ->
  ?icon:Protocol.icon -> handler:resource_handler -> unit -> unit
val list_resources : t -> Protocol.resource list
val read_resource :
  t -> uri:string -> (Protocol.resource_contents, string) result
val add_prompt :
  t ->
  name:string ->
  ?description:string ->
  ?arguments:Protocol.prompt_argument list ->
  ?icon:Protocol.icon -> ?handler:prompt_handler -> unit -> unit
val list_prompts : t -> Protocol.prompt list
val get_prompt :
  t ->
  name:string ->
  arguments:(string * string) list ->
  (Protocol.prompt_message list, string) result
val handle_request :
  t ->
  ctx:eio_ctx -> Jsonrpc.request -> Jsonrpc.response
val handle_notification : t -> Jsonrpc.notification -> unit
val handle_message :
  t ->
  ctx:eio_ctx ->
  Jsonrpc.message -> Jsonrpc.message option
val run : t -> ctx:eio_ctx -> Transport.t -> unit
val session : t -> Session.t
val logging : t -> Logging.t
