(** Kirin MCP Integration

    Provides MCP (Model Context Protocol) support for Kirin applications.
    Enables Kirin apps to act as MCP servers or clients.

    {b Features:}
    - MCP Server with Streamable HTTP transport (2025-11-25 spec)
    - Tool, Resource, and Prompt registration
    - Kirin handler to MCP tool conversion
    - Direct-style async (Eio-native) *)

(** {1 Re-exports from MCP library} *)

(** JSON-RPC 2.0 protocol. *)
module Jsonrpc = Kirin_mcp.Jsonrpc

(** MCP protocol types. *)
module Protocol = Kirin_mcp.Protocol

(** JSON Schema builder. *)
module Schema = Kirin_mcp.Schema

(** Transport layer. *)
module Transport = Kirin_mcp.Transport

(** Session management. *)
module Session = Kirin_mcp.Session

(** MCP Server. *)
module Server = Kirin_mcp.Server

(** MCP Client. *)
module Client = Kirin_mcp.Client

(** Governance layer. *)
module Governance = Kirin_mcp.Governance

(** Logging capability. *)
module Logging = Kirin_mcp.Logging

(** {1 Kirin Integration} *)

(** [tool_of_json_handler ~name ~description ~schema handler] converts a JSON handler
    to an MCP tool definition and handler pair.

    Returns [(tool, handler)] where tool is the MCP protocol tool definition. *)
val tool_of_json_handler :
  name:string ->
  description:string ->
  schema:Yojson.Safe.t ->
  (Yojson.Safe.t -> Yojson.Safe.t) ->
  Protocol.tool * (Yojson.Safe.t -> Yojson.Safe.t)

(** {1 Streamable HTTP Helpers} *)

(** Header name for MCP session ID (2025-11-25 spec). *)
val session_header : string

(** [is_initialize_request json] checks if a JSON-RPC body contains an initialize request.
    Handles both single requests and batches (arrays). *)
val is_initialize_request : Yojson.Safe.t -> bool

(** [validate_session req server] validates Mcp-Session-Id header against session state.
    Returns [None] on success, [Some error_response] on failure. *)
val validate_session : Request.t -> Server.t -> Response.t option

(** [with_session_header server resp] adds Mcp-Session-Id header to response
    if the session has one. *)
val with_session_header : Server.t -> Response.t -> Response.t

(** [routes ?prefix ~ctx server] creates Kirin routes for MCP server
    (Streamable HTTP transport, 2025-11-25).

    Returns routes for:
    - POST /mcp - Handle JSON-RPC requests with session ID management
    - GET /mcp - SSE stream for server-initiated notifications
    - DELETE /mcp - Session termination
    - OPTIONS /mcp - CORS preflight *)
val routes :
  ?prefix:string ->
  ctx:Server.eio_ctx ->
  Server.t ->
  Router.route list

(** [create_http_client ()] creates a Streamable HTTP MCP client transport. *)
val create_http_client : unit -> Transport.t

(** {1 Quick Access} *)

(** [create_server] creates an MCP server. Delegates to [Server.create]. *)
val create_server : ?name:string -> ?version:string -> ?log_level:Logging.log_level -> ?log_handler:(Logging.log_message -> unit) -> unit -> Server.t

(** [add_tool] adds a tool to server (variant handler: Sync or Async).
    Delegates to [Server.add_tool]. *)
val add_tool :
  Server.t ->
  name:string ->
  description:string ->
  schema:Yojson.Safe.t ->
  ?annotations:Protocol.tool_annotations ->
  ?icon:Protocol.icon ->
  handler:Server.tool_handler ->
  unit ->
  unit

(** [add_tool_sync] adds a synchronous tool to server.
    Delegates to [Server.add_tool_sync]. *)
val add_tool_sync :
  Server.t ->
  name:string ->
  description:string ->
  schema:Yojson.Safe.t ->
  ?annotations:Protocol.tool_annotations ->
  ?icon:Protocol.icon ->
  handler:(Yojson.Safe.t -> Yojson.Safe.t) ->
  unit ->
  unit

(** [add_resource] adds a resource to server.
    Delegates to [Server.add_resource]. *)
val add_resource :
  Server.t ->
  uri:string ->
  name:string ->
  ?description:string ->
  ?mime_type:string ->
  ?icon:Protocol.icon ->
  handler:Server.resource_handler ->
  unit ->
  unit

(** [add_prompt] adds a prompt to server.
    Delegates to [Server.add_prompt]. *)
val add_prompt :
  Server.t ->
  name:string ->
  ?description:string ->
  ?arguments:Protocol.prompt_argument list ->
  ?icon:Protocol.icon ->
  ?handler:Server.prompt_handler ->
  unit ->
  unit

(** Protocol version. *)
val version : string
