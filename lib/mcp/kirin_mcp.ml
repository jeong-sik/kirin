(** Kirin MCP - Model Context Protocol Support

    This module provides MCP (Model Context Protocol) integration for Kirin.
    It enables Kirin applications to act as MCP servers or clients,
    allowing AI agents to interact with the application.

    {b Example - MCP Server:}
    {[
      open Kirin_mcp

      let () =
        let server = Server.create () in

        (* Register a tool *)
        Server.add_tool server
          ~name:"greet"
          ~description:"Greet a person"
          ~schema:(Schema.object_ [
            "name", Schema.string ~description:"Person's name" ()
          ] ~required:["name"])
          ~handler:(fun params ->
            let open Yojson.Safe.Util in
            let name = params |> member "name" |> to_string in
            `String (Printf.sprintf "Hello, %s!" name)
          );

        (* Run on stdio *)
        Server.run server (Transport.of_stdio ~ic:... ~oc:...)
    ]}

    {b Example - MCP Client:}
    {[
      open Kirin_mcp

      let () =
        let client = Client.create transport in
        let _ = Client.initialize client () in

        (* List and call tools *)
        let tools = Client.list_tools client in
        let result = Client.call_tool client
          ~name:"greet"
          ~arguments:(`Assoc ["name", `String "World"]) () in
        ...
    ]}
*)

(** JSON-RPC 2.0 protocol implementation *)
module Jsonrpc = Jsonrpc

(** MCP protocol types and message definitions *)
module Protocol = Protocol

(** JSON Schema builder for tool input definitions *)
module Schema = Schema

(** Transport layer (stdio, Streamable HTTP) *)
module Transport = Transport

(** Tasks primitive for async tool execution *)
module Tasks = Tasks

(** Session management and lifecycle *)
module Session = Session

(** MCP Server implementation *)
module Server = Server

(** MCP Client implementation *)
module Client = Client

(** {1 Quick Access} *)

(** Create an MCP server *)
let create_server = Server.create

(** Create an MCP client *)
let create_client = Client.create

(** Protocol version *)
let version = Protocol.protocol_version
