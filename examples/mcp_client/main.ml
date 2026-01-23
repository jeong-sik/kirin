(** Kirin MCP Client Example

    Demonstrates how to create an MCP client that uses HTTP+SSE transport.
    This is the typical setup for web-based MCP servers.

    In a real application, you would connect to an MCP server endpoint
    and use the client to call tools, read resources, etc.

    This example shows the client API structure.
*)

open Kirin

(** Demo showing MCP client API usage *)
let () =
  Printf.printf "Kirin MCP Client Example\n";
  Printf.printf "========================\n\n";

  (* Create HTTP+SSE transport (for web-based MCP servers) *)
  let transport = Mcp.Transport.of_http_sse () in
  Printf.printf "Created HTTP+SSE transport\n";

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
  Printf.printf "  2. The client would POST to /mcp endpoint\n";
  Printf.printf "  3. Server-sent events come from /mcp/sse\n";

  (* Show transport check *)
  Printf.printf "\nTransport type: %s\n"
    (if Mcp.Transport.is_http_sse transport then "HTTP+SSE" else "stdio");

  ignore client;
  Printf.printf "\nMCP client demo complete.\n"
