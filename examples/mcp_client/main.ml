(** Kirin MCP Client Example

    Demonstrates how to create an MCP client that uses Streamable HTTP transport.
    This is the typical setup for web-based MCP servers (2025-11-25 spec).

    In a real application, you would connect to an MCP server endpoint
    and use the client to call tools, read resources, etc.

    This example shows the client API structure.
*)

open Kirin

(** Demo showing MCP client API usage *)
let () =
  Printf.printf "Kirin MCP Client Example\n";
  Printf.printf "========================\n\n";

  (* Create Streamable HTTP transport (for web-based MCP servers) *)
  let transport = Mcp.Transport.create_streamable_http () in
  Printf.printf "Created Streamable HTTP transport\n";

  (* Create client *)
  let client = Mcp.Client.create transport in
  Printf.printf "Created MCP client\n";

  (* Show client API *)
  Printf.printf "\nAvailable client methods:\n";
  Printf.printf "  - Mcp.Client.initialize client ()\n";
  Printf.printf "  - Mcp.Client.list_tools client\n";
  Printf.printf "  - Mcp.Client.call_tool client ~name ~arguments ()\n";
  Printf.printf "  - Mcp.Client.list_resources client\n";
  Printf.printf "  - Mcp.Client.read_resource client ~uri\n";
  Printf.printf "  - Mcp.Client.list_prompts client\n";
  Printf.printf "  - Mcp.Client.get_prompt client ~name ~arguments ()\n";

  Printf.printf "\nExample: Calling a tool\n";
  Printf.printf {|
  let result = Mcp.Client.call_tool client
    ~name:"greet"
    ~arguments:(`Assoc ["name", `String "World"])
    ()
  in
  List.iter (function
    | Mcp.Protocol.Text s -> print_endline s
    | _ -> ()
  ) result.content
|};

  Printf.printf "\nTo connect to a real MCP server:\n";
  Printf.printf "  1. Start an MCP server (e.g., run mcp_server example)\n";
  Printf.printf "  2. Client POSTs JSON-RPC to /mcp endpoint\n";
  Printf.printf "  3. Server responds with JSON or SSE stream\n";
  Printf.printf "  4. Mcp-Session-Id header tracks the session\n";

  (* Show transport check *)
  Printf.printf "\nTransport type: %s\n"
    (if Mcp.Transport.is_streamable_http transport then "Streamable HTTP" else "stdio");

  ignore client;
  Printf.printf "\nMCP client demo complete.\n"
