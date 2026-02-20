(** Kirin MCP Server Example

    Demonstrates how to create an MCP server with Kirin.
    AI agents can connect to this server to use the registered tools.

    Run with: dune exec examples/mcp_server/main.exe
    Test with: npx @anthropic/mcp-inspector http://localhost:8080/mcp
*)

open Kirin

let () =
  (* Create MCP server *)
  let mcp = Mcp.Server.create ~name:"kirin-example" ~version:"1.0.0" () in

  (* Register a greeting tool (synchronous) *)
  Mcp.Server.add_tool_sync mcp
    ~name:"greet"
    ~description:"Greet a person by name"
    ~schema:(Mcp.Schema.object_ [
      "name", Mcp.Schema.string ~description:"The name of the person to greet" ()
    ] ~required:["name"])
    ~handler:(fun params ->
      let open Yojson.Safe.Util in
      let name = params |> member "name" |> to_string in
      `String (Printf.sprintf "Hello, %s! Welcome to Kirin MCP." name)
    ) ();

  (* Register a calculator tool (synchronous) *)
  Mcp.Server.add_tool_sync mcp
    ~name:"add"
    ~description:"Add two numbers together"
    ~schema:(Mcp.Schema.object_ [
      "a", Mcp.Schema.number ~description:"First number" ();
      "b", Mcp.Schema.number ~description:"Second number" ();
    ] ~required:["a"; "b"])
    ~handler:(fun params ->
      let open Yojson.Safe.Util in
      (* Handle both int and float JSON numbers *)
      let to_number json = match json with
        | `Int i -> float_of_int i
        | `Float f -> f
        | _ -> failwith "Expected number"
      in
      let a = params |> member "a" |> to_number in
      let b = params |> member "b" |> to_number in
      `Float (a +. b)
    ) ();

  (* Register a time resource *)
  Mcp.Server.add_resource mcp
    ~uri:"time://current"
    ~name:"Current Time"
    ~mime_type:"text/plain"
    ~handler:(fun () ->
      let time = Unix.gettimeofday () in
      let tm = Unix.localtime time in
      let str = Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d"
        (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
        tm.tm_hour tm.tm_min tm.tm_sec in
      (str, "text/plain")
    )
    ();

  (* Create Kirin routes — sw and clock are needed for async tool execution *)
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let mcp_routes = Mcp.routes ~sw ~clock mcp in

  let routes = router ([
    get "/" (fun _ -> html {|
      <!DOCTYPE html>
      <html>
      <head><title>Kirin MCP Server</title></head>
      <body>
        <h1>Kirin MCP Server</h1>
        <p>This server provides MCP endpoints for AI agents.</p>
        <h2>Available Tools:</h2>
        <ul>
          <li><strong>greet</strong> - Greet a person by name</li>
          <li><strong>add</strong> - Add two numbers together</li>
        </ul>
        <h2>Available Resources:</h2>
        <ul>
          <li><strong>time://current</strong> - Current server time</li>
        </ul>
        <h2>Endpoints:</h2>
        <ul>
          <li>POST /mcp - JSON-RPC endpoint</li>
          <li>GET /mcp/sse - SSE endpoint</li>
        </ul>
        <p>Test with: <code>npx @anthropic/mcp-inspector http://localhost:8080/mcp</code></p>
      </body>
      </html>
    |});
  ] @ mcp_routes) in

  (* Start the server *)
  Printf.printf "Starting Kirin MCP Server on http://localhost:8080\n%!";
  Printf.printf "MCP endpoint: http://localhost:8080/mcp\n%!";
  Printf.printf "Test with: npx @anthropic/mcp-inspector http://localhost:8080/mcp\n%!";
  start ~port:8080 @@ logger @@ routes
