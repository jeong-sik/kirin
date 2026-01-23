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
  } in
  (tool, handler)

(** Create Kirin routes for MCP server (HTTP+SSE transport)

    Returns routes for:
    - POST /mcp - Handle JSON-RPC requests
    - GET /mcp/sse - SSE endpoint for server-to-client messages

    {[
      let mcp = Server.create () in
      Server.add_tool mcp ~name:"hello" ...;

      let all_routes = routes mcp @ [
        Kirin.get "/" (fun _ -> Kirin.html "Home");
      ] in
      Kirin.start ~port:8080 @@ Kirin.router all_routes
    ]}
*)
let routes ?(prefix = "/mcp") (server : Server.t) : Router.route list =
  let handle_post req =
    let body = Request.body req in
    try
      let json = Yojson.Safe.from_string body in
      let msg = Jsonrpc.decode json in
      match Server.handle_message server msg with
      | Some (Jsonrpc.Response resp) ->
        let json = Jsonrpc.encode_response resp in
        Response.json json
      | Some _ ->
        Response.json (`Assoc [])
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
  let handle_sse _req =
    (* SSE endpoint for server-initiated messages *)
    (* For now, just return a simple keep-alive stream *)
    Sse.response [
      Sse.event ~event_type:"connected" "MCP SSE connected";
    ]
  in
  let handle_options _req =
    Response.empty `OK
    |> Response.with_header "Access-Control-Allow-Origin" "*"
    |> Response.with_header "Access-Control-Allow-Methods" "GET, POST, OPTIONS"
    |> Response.with_header "Access-Control-Allow-Headers" "Content-Type"
  in
  [
    Router.post prefix handle_post;
    Router.get (prefix ^ "/sse") handle_sse;
    Router.options prefix handle_options;
  ]

(** Create an HTTP+SSE MCP client transport.

    Note: For a production client, you'd need to handle:
    1. HTTP POST requests to the MCP endpoint
    2. SSE connection for server-initiated messages

    For now, this returns the basic transport structure.
*)
let create_http_client () =
  Transport.of_http_sse ()

(** {1 Quick Access} *)

(** Create an MCP server *)
let create_server = Server.create

(** Add a tool to server *)
let add_tool = Server.add_tool

(** Add a resource to server *)
let add_resource = Server.add_resource

(** Add a prompt to server *)
let add_prompt = Server.add_prompt

(** Protocol version *)
let version = Protocol.protocol_version
