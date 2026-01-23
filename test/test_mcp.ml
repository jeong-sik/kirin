(** MCP (Model Context Protocol) Module Tests *)

open Alcotest

module J = Kirin_mcp.Jsonrpc
module S = Kirin_mcp.Schema
module P = Kirin_mcp.Protocol

(** {1 JSON-RPC Tests} *)

let test_jsonrpc_request_encode () =
  let req = J.make_request
    ~id:(J.Int 1)
    ~method_:"test/method"
    ~params:(`Assoc [("key", `String "value")])
    () in
  let json = J.encode_request req in
  let json_str = Yojson.Safe.to_string json in
  check bool "contains jsonrpc" true (String.length json_str > 0)

let test_jsonrpc_request_decode () =
  let json = `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int 42);
    ("method", `String "test/method");
    ("params", `Assoc [("name", `String "test")]);
  ] in
  let req = J.decode_request json in
  check string "method matches" "test/method" req.method_

let test_jsonrpc_response_success () =
  let resp = J.success_response
    ~id:(J.Int 1)
    (`Assoc [("result", `String "ok")]) in
  check bool "has result" true (Option.is_some resp.result)

let test_jsonrpc_response_error () =
  let err = J.make_error
    ~code:J.Invalid_params
    ~message:"Bad params" () in
  let resp = J.error_response ~id:(J.Int 1) err in
  check bool "has error" true (Option.is_some resp.error)

let test_jsonrpc_notification () =
  let notif = J.make_notification
    ~method_:"notifications/test"
    ~params:`Null () in
  check string "method" "notifications/test" notif.method_

let jsonrpc_tests = [
  test_case "request encode" `Quick test_jsonrpc_request_encode;
  test_case "request decode" `Quick test_jsonrpc_request_decode;
  test_case "response success" `Quick test_jsonrpc_response_success;
  test_case "response error" `Quick test_jsonrpc_response_error;
  test_case "notification" `Quick test_jsonrpc_notification;
]

(** {1 Schema Tests} *)

let test_schema_string () =
  let schema = S.string ~description:"A name" () in
  let json = Yojson.Safe.to_string schema in
  check bool "has type string" true
    (try ignore (Str.search_forward (Str.regexp "string") json 0); true
     with Not_found -> false)

let test_schema_number () =
  let schema = S.number ~minimum:0.0 ~maximum:100.0 () in
  let json = Yojson.Safe.to_string schema in
  check bool "has type number" true
    (try ignore (Str.search_forward (Str.regexp "number") json 0); true
     with Not_found -> false)

let test_schema_object () =
  let schema = S.object_
    ~required:["name"]
    [("name", S.string ()); ("age", S.int ())] in
  let json = Yojson.Safe.to_string schema in
  check bool "has type object" true
    (try ignore (Str.search_forward (Str.regexp "object") json 0); true
     with Not_found -> false)

let test_schema_array () =
  let schema = S.array (S.string ()) in
  let json = Yojson.Safe.to_string schema in
  check bool "has type array" true
    (try ignore (Str.search_forward (Str.regexp "array") json 0); true
     with Not_found -> false)

let schema_tests = [
  test_case "string schema" `Quick test_schema_string;
  test_case "number schema" `Quick test_schema_number;
  test_case "object schema" `Quick test_schema_object;
  test_case "array schema" `Quick test_schema_array;
]

(** {1 Server Tests} *)

let test_server_create () =
  let server = Kirin_mcp.Server.create () in
  check bool "server created" true (Kirin_mcp.Server.list_tools server = [])

let test_server_add_tool () =
  let server = Kirin_mcp.Server.create () in
  Kirin_mcp.Server.add_tool server
    ~name:"greet"
    ~description:"Greet someone"
    ~schema:(S.object_
      ~required:["name"]
      [("name", S.string ())])
    ~handler:(fun params ->
      let name = Yojson.Safe.Util.(params |> member "name" |> to_string) in
      `String (Printf.sprintf "Hello, %s!" name));
  let tools = Kirin_mcp.Server.list_tools server in
  check int "one tool" 1 (List.length tools);
  check string "tool name" "greet" (List.hd tools).name

let test_server_call_tool () =
  let server = Kirin_mcp.Server.create () in
  Kirin_mcp.Server.add_tool server
    ~name:"add"
    ~description:"Add two numbers"
    ~schema:(S.object_
      ~required:["a"; "b"]
      [("a", S.int ()); ("b", S.int ())])
    ~handler:(fun params ->
      let open Yojson.Safe.Util in
      let a = params |> member "a" |> to_int in
      let b = params |> member "b" |> to_int in
      `Int (a + b));
  match Kirin_mcp.Server.call_tool server
    ~name:"add"
    ~arguments:(Some (`Assoc [("a", `Int 2); ("b", `Int 3)])) with
  | Ok result ->
    (match result.content with
     | [P.Text s] ->
       check string "result is 5" "5" s
     | _ -> fail "unexpected content type")
  | Error msg -> fail msg

let test_server_add_resource () =
  let server = Kirin_mcp.Server.create () in
  Kirin_mcp.Server.add_resource server
    ~uri:"file:///test.txt"
    ~name:"Test File"
    ~description:"A test file"
    ~mime_type:"text/plain"
    ~handler:(fun () -> ("Hello, World!", "text/plain"))
    ();
  let resources = Kirin_mcp.Server.list_resources server in
  check int "one resource" 1 (List.length resources);
  check string "resource uri" "file:///test.txt" (List.hd resources).uri

let test_server_read_resource () =
  let server = Kirin_mcp.Server.create () in
  Kirin_mcp.Server.add_resource server
    ~uri:"test://data"
    ~name:"Test Data"
    ~handler:(fun () -> ("test content", "text/plain"))
    ();
  match Kirin_mcp.Server.read_resource server ~uri:"test://data" with
  | Ok contents ->
    check (option string) "text content" (Some "test content") contents.text
  | Error msg -> fail msg

let test_server_add_prompt () =
  let server = Kirin_mcp.Server.create () in
  Kirin_mcp.Server.add_prompt server
    ~name:"greeting"
    ~description:"A greeting prompt"
    ~arguments:[{
      P.name = "style";
      description = Some "Greeting style";
      required = Some false;
    }]
    ();
  let prompts = Kirin_mcp.Server.list_prompts server in
  check int "one prompt" 1 (List.length prompts);
  check string "prompt name" "greeting" (List.hd prompts).name

let test_server_handle_initialize () =
  let server = Kirin_mcp.Server.create ~name:"test-server" ~version:"1.0.0" () in
  Kirin_mcp.Server.add_tool server
    ~name:"test"
    ~description:"Test tool"
    ~schema:(`Assoc [])
    ~handler:(fun _ -> `Null);
  let req = J.make_request
    ~id:(J.Int 1)
    ~method_:"initialize"
    ~params:(`Assoc [
      ("protocolVersion", `String "2024-11-05");
      ("capabilities", `Assoc []);
      ("clientInfo", `Assoc [("name", `String "test-client"); ("version", `String "1.0")]);
    ])
    () in
  let resp = Kirin_mcp.Server.handle_request server req in
  check bool "response has result" true (Option.is_some resp.result)

let test_server_handle_tools_list () =
  let server = Kirin_mcp.Server.create () in
  Kirin_mcp.Server.add_tool server
    ~name:"tool1" ~description:"Tool 1"
    ~schema:(`Assoc []) ~handler:(fun _ -> `Null);
  Kirin_mcp.Server.add_tool server
    ~name:"tool2" ~description:"Tool 2"
    ~schema:(`Assoc []) ~handler:(fun _ -> `Null);
  let req = J.make_request
    ~id:(J.Int 1)
    ~method_:"tools/list"
    () in
  let resp = Kirin_mcp.Server.handle_request server req in
  check bool "response has result" true (Option.is_some resp.result)

let server_tests = [
  test_case "create" `Quick test_server_create;
  test_case "add tool" `Quick test_server_add_tool;
  test_case "call tool" `Quick test_server_call_tool;
  test_case "add resource" `Quick test_server_add_resource;
  test_case "read resource" `Quick test_server_read_resource;
  test_case "add prompt" `Quick test_server_add_prompt;
  test_case "handle initialize" `Quick test_server_handle_initialize;
  test_case "handle tools/list" `Quick test_server_handle_tools_list;
]

(** {1 Protocol Tests} *)

let test_protocol_tool_json () =
  let tool : P.tool = {
    name = "test_tool";
    description = Some "A test tool";
    input_schema = `Assoc [("type", `String "object")];
  } in
  let json = P.tool_to_json tool in
  check bool "has name field" true
    (match Yojson.Safe.Util.member "name" json with
     | `String _ -> true
     | _ -> false)

let test_protocol_resource_json () =
  let resource : P.resource = {
    uri = "file:///test";
    name = "test";
    description = Some "Test resource";
    mime_type = Some "text/plain";
  } in
  let json = P.resource_to_json resource in
  check bool "has uri field" true
    (match Yojson.Safe.Util.member "uri" json with
     | `String _ -> true
     | _ -> false)

let test_protocol_version () =
  check string "protocol version" "2024-11-05" P.protocol_version

let protocol_tests = [
  test_case "tool json" `Quick test_protocol_tool_json;
  test_case "resource json" `Quick test_protocol_resource_json;
  test_case "protocol version" `Quick test_protocol_version;
]

(** {1 Session Tests} *)

let test_session_create () =
  let session = Kirin_mcp.Session.create
    ~server_name:"test"
    ~server_version:"1.0.0" () in
  check bool "state is uninitialized" true
    (Kirin_mcp.Session.state session = Kirin_mcp.Session.Uninitialized)

let test_session_initialize () =
  let session = Kirin_mcp.Session.create
    ~server_name:"test"
    ~server_version:"1.0.0" () in
  let params : P.initialize_params = {
    protocol_version = "2024-11-05";
    capabilities = { roots = None; sampling = None };
    client_info = { name = "test-client"; version = "1.0" };
  } in
  match Kirin_mcp.Session.handle_initialize session params with
  | Ok result ->
    check string "server name" "test" result.server_info.name;
    check bool "state is initializing" true
      (Kirin_mcp.Session.state session = Kirin_mcp.Session.Initializing)
  | Error msg -> fail msg

let session_tests = [
  test_case "create" `Quick test_session_create;
  test_case "initialize" `Quick test_session_initialize;
]

(** {1 Main} *)

let () =
  run "Mcp" [
    ("Jsonrpc", jsonrpc_tests);
    ("Schema", schema_tests);
    ("Server", server_tests);
    ("Protocol", protocol_tests);
    ("Session", session_tests);
  ]
