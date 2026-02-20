(** Kirin MCP Integration

    This module provides MCP (Model Context Protocol) support for Kirin applications.
    It enables Kirin apps to act as MCP servers that AI agents can connect to,
    or as MCP clients that call external MCP servers.

    {b Features:}
    - MCP Server with HTTP+SSE transport
    - Tool, Resource, and Prompt registration
    - Kirin handler to MCP tool conversion
    - Direct-style async (Eio-native)

    {b Quick Example - Server:}
    {[
      open Kirin.Mcp

      let mcp = Server.create () in

      Server.add_tool mcp
        ~name:"greet"
        ~description:"Greet a person"
        ~schema:(Schema.object_ [
          "name", Schema.string ~description:"Person's name" ()
        ] ~required:["name"])
        ~handler:(fun params ->
          let name = Yojson.Safe.Util.(params |> member "name" |> to_string) in
          `String (Printf.sprintf "Hello, %s!" name));

      let routes = routes mcp in
      Kirin.start ~port:8080 @@ Kirin.router routes
    ]}

    {b Quick Example - Client:}
    {[
      let result = with_mcp_client ~endpoint:"http://localhost:3000/mcp"
        (fun client _req ->
          let _ = Client.initialize client () in
          let tools = Client.list_tools client in
          Kirin.json (`List (List.map Protocol.tool_to_json tools)))
        req
    ]}
*)

(** {1 Re-exports from MCP library} *)

(** JSON-RPC 2.0 protocol *)
module Jsonrpc = Kirin_mcp.Jsonrpc

(** MCP protocol types *)
module Protocol = Kirin_mcp.Protocol

(** JSON Schema builder *)
module Schema = Kirin_mcp.Schema

(** Transport layer *)
module Transport = Kirin_mcp.Transport

(** Session management *)
module Session = Kirin_mcp.Session

(** MCP Server *)
module Server = Kirin_mcp.Server

(** MCP Client *)
module Client = Kirin_mcp.Client

(** Governance layer *)
module Governance = Kirin_mcp.Governance

(** Logging capability *)
module Logging = Kirin_mcp.Logging

(** {1 Kirin Integration} *)

(** Convert an MCP tool handler that works with JSON params
    to a Kirin-style typed handler.

    Note: Due to Kirin's Request requiring http internals,
    direct conversion from Kirin handlers is not straightforward.
    Use Server.add_tool directly with JSON handlers instead.
*)
let tool_of_json_handler ~name ~description ~(schema : Yojson.Safe.t)
    (handler : Yojson.Safe.t -> Yojson.Safe.t) =
  let tool : Protocol.tool = {
    name;
    description = Some description;
    input_schema = schema;
    annotations = None;
    icon = None;
  } in
  (tool, handler)

(** {1 Streamable HTTP Helpers} *)

(** Header name for MCP session ID (2025-11-25 spec) *)
let session_header = "mcp-session-id"

(** Check if JSON-RPC body contains an initialize request.
    The body may be a single request or a batch (array). *)
let is_initialize_request json =
  match json with
  | `Assoc fields ->
    (match List.assoc_opt "method" fields with
     | Some (`String m) -> m = Protocol.Method.initialize
     | _ -> false)
  | `List items ->
    List.exists (fun item ->
      match item with
      | `Assoc fields ->
        (match List.assoc_opt "method" fields with
         | Some (`String m) -> m = Protocol.Method.initialize
         | _ -> false)
      | _ -> false
    ) items
  | _ -> false

(** Validate Mcp-Session-Id header against session state.
    Returns None on success, Some error_response on failure. *)
let validate_session req server =
  let session = Server.session server in
  match Session.session_id session with
  | None ->
    (* No session yet — only initialize is allowed (caller checks) *)
    None
  | Some expected_id ->
    match Request.header session_header req with
    | Some id when id = expected_id -> None
    | Some _ ->
      Some (Response.json ~status:`Not_found
        (`Assoc ["error", `String "Session not found"]))
    | None ->
      Some (Response.bad_request ~body:"Missing Mcp-Session-Id header" ())

(** Add Mcp-Session-Id header to response if session has one *)
let with_session_header server resp =
  let session = Server.session server in
  match Session.session_id session with
  | Some id -> Response.with_header session_header id resp
  | None -> resp

(** Create Kirin routes for MCP server (Streamable HTTP transport, 2025-11-25)

    Returns routes for:
    - POST /mcp - Handle JSON-RPC requests with session ID management
    - GET /mcp - SSE stream for server-initiated notifications
    - DELETE /mcp - Session termination
    - OPTIONS /mcp - CORS preflight

    {[
      Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      let clock = Eio.Stdenv.clock env in
      let mcp = Server.create () in
      Server.add_tool_sync mcp ~name:"hello" ...;

      let all_routes = routes ~sw ~clock mcp @ [
        Kirin.get "/" (fun _ -> Kirin.html "Home");
      ] in
      Kirin.start ~port:8080 @@ Kirin.router all_routes
    ]}
*)
let routes ?(prefix = "/mcp") ~sw ~clock (server : Server.t) : Router.route list =
  let broadcaster = Sse.Broadcaster.create () in

  let handle_post req =
    let body = Request.body req in
    try
      let json = Yojson.Safe.from_string body in
      let is_init = is_initialize_request json in
      (* Validate session ID for non-initialize requests *)
      if not is_init then
        match validate_session req server with
        | Some err -> err
        | None ->
          let msg = Jsonrpc.decode json in
          (match Server.handle_message server ~sw ~clock msg with
           | Some (Jsonrpc.Response resp) ->
             Response.json (Jsonrpc.encode_response resp)
             |> with_session_header server
           | Some _ ->
             Response.json (`Assoc [])
             |> with_session_header server
           | None ->
             Response.empty `Accepted
             |> with_session_header server)
      else
        let msg = Jsonrpc.decode json in
        match Server.handle_message server ~sw ~clock msg with
        | Some (Jsonrpc.Response resp) ->
          Response.json (Jsonrpc.encode_response resp)
          |> with_session_header server
        | Some _ ->
          Response.json (`Assoc [])
          |> with_session_header server
        | None ->
          Response.empty `Accepted
    with
    | Yojson.Json_error msg ->
      let error = Jsonrpc.make_error
        ~code:Jsonrpc.Parse_error
        ~message:(Printf.sprintf "JSON parse error: %s" msg) () in
      let resp = Jsonrpc.error_response ~id:Jsonrpc.Null error in
      Response.json (Jsonrpc.encode_response resp)
    | e ->
      let error = Jsonrpc.make_error
        ~code:Jsonrpc.Internal_error
        ~message:(Printexc.to_string e) () in
      let resp = Jsonrpc.error_response ~id:Jsonrpc.Null error in
      Response.json (Jsonrpc.encode_response resp)
  in

  let handle_get req =
    (* SSE endpoint for server-initiated notifications *)
    match validate_session req server with
    | Some err -> err
    | None -> Sse.handler broadcaster req
  in

  let handle_delete req =
    match validate_session req server with
    | Some err -> err
    | None ->
      let session = Server.session server in
      Session.begin_shutdown session;
      Session.complete_shutdown session;
      Response.empty `OK
      |> with_session_header server
  in

  let handle_options _req =
    Response.empty `OK
    |> Response.with_header "Access-Control-Allow-Origin" "*"
    |> Response.with_header "Access-Control-Allow-Methods"
         "GET, POST, DELETE, OPTIONS"
    |> Response.with_header "Access-Control-Allow-Headers"
         "Content-Type, Accept, Mcp-Session-Id"
    |> Response.with_header "Access-Control-Expose-Headers"
         "Mcp-Session-Id"
  in
  [
    Router.post prefix handle_post;
    Router.get prefix handle_get;
    Router.delete prefix handle_delete;
    Router.options prefix handle_options;
  ]

(** Create a Streamable HTTP MCP client transport (2025-11-25).

    Note: For a production client, you'd need to handle:
    1. HTTP POST requests to the MCP endpoint
    2. Optional GET SSE for server-initiated messages
    3. Mcp-Session-Id header management

    For now, this returns the basic transport structure.
*)
let create_http_client () =
  Transport.create_streamable_http ()

(** {1 Quick Access} *)

(** Create an MCP server *)
let create_server = Server.create

(** Add a tool to server (variant handler: [Sync] or [Async]) *)
let add_tool = Server.add_tool

(** Add a synchronous tool to server (convenience) *)
let add_tool_sync = Server.add_tool_sync

(** Add a resource to server *)
let add_resource = Server.add_resource

(** Add a prompt to server *)
let add_prompt = Server.add_prompt

(** Protocol version *)
let version = Protocol.protocol_version
