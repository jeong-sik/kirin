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
      `String (Printf.sprintf "Hello, %s!" name))
    ();
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
      `Int (a + b))
    ();
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
    ~handler:(fun _ -> `Null)
    ();
  let req = J.make_request
    ~id:(J.Int 1)
    ~method_:"initialize"
    ~params:(`Assoc [
      ("protocolVersion", `String "2025-11-25");
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
    ~schema:(`Assoc []) ~handler:(fun _ -> `Null) ();
  Kirin_mcp.Server.add_tool server
    ~name:"tool2" ~description:"Tool 2"
    ~schema:(`Assoc []) ~handler:(fun _ -> `Null) ();
  let req = J.make_request
    ~id:(J.Int 1)
    ~method_:"tools/list"
    () in
  let resp = Kirin_mcp.Server.handle_request server req in
  check bool "response has result" true (Option.is_some resp.result)

let test_server_add_tool_with_annotations () =
  let server = Kirin_mcp.Server.create () in
  let ann : P.tool_annotations = {
    title = Some "My Reader";
    read_only_hint = Some true;
    destructive_hint = Some false;
    idempotent_hint = Some true;
    open_world_hint = None;
  } in
  let icon : P.icon = { uri = "https://example.com/icon.png" } in
  Kirin_mcp.Server.add_tool server
    ~name:"reader"
    ~description:"Read a file"
    ~schema:(`Assoc [])
    ~annotations:ann
    ~icon
    ~handler:(fun _ -> `Null)
    ();
  let tools = Kirin_mcp.Server.list_tools server in
  check int "one tool" 1 (List.length tools);
  let tool = List.hd tools in
  check string "tool name" "reader" tool.name;
  check bool "has annotations" true (Option.is_some tool.annotations);
  let a = Option.get tool.annotations in
  check (option string) "title" (Some "My Reader") a.title;
  check (option bool) "read_only_hint" (Some true) a.read_only_hint;
  check bool "has icon" true (Option.is_some tool.icon);
  check string "icon uri" "https://example.com/icon.png" (Option.get tool.icon).uri

let server_tests = [
  test_case "create" `Quick test_server_create;
  test_case "add tool" `Quick test_server_add_tool;
  test_case "call tool" `Quick test_server_call_tool;
  test_case "add resource" `Quick test_server_add_resource;
  test_case "read resource" `Quick test_server_read_resource;
  test_case "add prompt" `Quick test_server_add_prompt;
  test_case "handle initialize" `Quick test_server_handle_initialize;
  test_case "handle tools/list" `Quick test_server_handle_tools_list;
  test_case "add tool with annotations/icon" `Quick test_server_add_tool_with_annotations;
]

(** {1 Protocol Tests} *)

let test_protocol_tool_json () =
  let tool : P.tool = {
    name = "test_tool";
    description = Some "A test tool";
    input_schema = `Assoc [("type", `String "object")];
    annotations = None;
    icon = None;
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
    icon = None;
  } in
  let json = P.resource_to_json resource in
  check bool "has uri field" true
    (match Yojson.Safe.Util.member "uri" json with
     | `String _ -> true
     | _ -> false)

let test_protocol_version () =
  check string "protocol version" "2025-11-25" P.protocol_version

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
    protocol_version = "2025-11-25";
    capabilities = { roots = None; sampling = None; experimental = None };
    client_info = { name = "test-client"; version = "1.0"; description = None };
    _meta = None;
  } in
  match Kirin_mcp.Session.handle_initialize session params with
  | Ok result ->
    check string "server name" "test" result.server_info.name;
    check bool "state is initializing" true
      (Kirin_mcp.Session.state session = Kirin_mcp.Session.Initializing)
  | Error msg -> fail msg

let test_session_id_generation () =
  let session = Kirin_mcp.Session.create
    ~server_name:"test"
    ~server_version:"1.0.0" () in
  check (option string) "no session id before init" None
    (Kirin_mcp.Session.session_id session);
  let params : P.initialize_params = {
    protocol_version = "2025-11-25";
    capabilities = { roots = None; sampling = None; experimental = None };
    client_info = { name = "test-client"; version = "1.0"; description = None };
    _meta = None;
  } in
  match Kirin_mcp.Session.handle_initialize session params with
  | Ok _ ->
    let sid = Kirin_mcp.Session.session_id session in
    check bool "session id is Some" true (Option.is_some sid);
    let id_str = Option.get sid in
    check bool "session id is 32 hex chars" true (String.length id_str = 32)
  | Error msg -> fail msg

let test_session_id_uniqueness () =
  let s1 = Kirin_mcp.Session.create ~server_name:"s" ~server_version:"1" () in
  let s2 = Kirin_mcp.Session.create ~server_name:"s" ~server_version:"1" () in
  let params : P.initialize_params = {
    protocol_version = "2025-11-25";
    capabilities = { roots = None; sampling = None; experimental = None };
    client_info = { name = "test"; version = "1.0"; description = None };
    _meta = None;
  } in
  ignore (Kirin_mcp.Session.handle_initialize s1 params);
  ignore (Kirin_mcp.Session.handle_initialize s2 params);
  let id1 = Option.get (Kirin_mcp.Session.session_id s1) in
  let id2 = Option.get (Kirin_mcp.Session.session_id s2) in
  check bool "two sessions have different IDs" true (id1 <> id2)

let test_session_id_hex_format () =
  let session = Kirin_mcp.Session.create ~server_name:"s" ~server_version:"1" () in
  let params : P.initialize_params = {
    protocol_version = "2025-11-25";
    capabilities = { roots = None; sampling = None; experimental = None };
    client_info = { name = "test"; version = "1.0"; description = None };
    _meta = None;
  } in
  ignore (Kirin_mcp.Session.handle_initialize session params);
  let id_str = Option.get (Kirin_mcp.Session.session_id session) in
  let is_hex c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') in
  let all_hex = String.to_seq id_str |> Seq.for_all is_hex in
  check bool "all chars are hex [0-9a-f]" true all_hex

let session_tests = [
  test_case "create" `Quick test_session_create;
  test_case "initialize" `Quick test_session_initialize;
  test_case "session id generation" `Quick test_session_id_generation;
  test_case "session id uniqueness" `Quick test_session_id_uniqueness;
  test_case "session id hex format" `Quick test_session_id_hex_format;
]

(** {1 Tasks Tests} *)

module T = Kirin_mcp.Tasks

let test_tasks_create () =
  let reg = T.create_registry () in
  let task = T.create_task reg ~tool_name:"test-tool" in
  check string "task id" "task-1" task.id;
  check string "tool name" "test-tool" task.tool_name;
  check bool "state is working" true (task.state = T.Working);
  check bool "no progress" true (Option.is_none task.progress)

let test_tasks_complete () =
  let reg = T.create_registry () in
  let task = T.create_task reg ~tool_name:"add" in
  let result : P.tool_result = {
    content = [P.Text "42"];
    is_error = None;
    _meta = None;
  } in
  (match T.complete_task reg ~id:task.id ~result with
   | Ok () ->
     let t = Option.get (T.get_task reg ~id:task.id) in
     check bool "completed" true (t.state = T.Completed);
     check bool "has result" true (Option.is_some t.result);
     check bool "progress is 1.0" true (t.progress = Some 1.0)
   | Error msg -> fail msg)

let test_tasks_fail () =
  let reg = T.create_registry () in
  let task = T.create_task reg ~tool_name:"fail-tool" in
  (match T.fail_task reg ~id:task.id ~error:"something broke" with
   | Ok () ->
     let t = Option.get (T.get_task reg ~id:task.id) in
     check bool "failed" true (t.state = T.Failed);
     check (option string) "error msg" (Some "something broke") t.error
   | Error msg -> fail msg)

let test_tasks_cancel () =
  let reg = T.create_registry () in
  let task = T.create_task reg ~tool_name:"cancel-me" in
  (match T.cancel_task reg ~id:task.id with
   | Ok () ->
     let t = Option.get (T.get_task reg ~id:task.id) in
     check bool "cancelled" true (t.state = T.Cancelled)
   | Error msg -> fail msg)

let test_tasks_cancel_completed_fails () =
  let reg = T.create_registry () in
  let task = T.create_task reg ~tool_name:"done" in
  let result : P.tool_result = {
    content = [P.Text "ok"]; is_error = None; _meta = None;
  } in
  ignore (T.complete_task reg ~id:task.id ~result);
  match T.cancel_task reg ~id:task.id with
  | Ok () -> fail "should not cancel completed task"
  | Error _ -> ()

let test_tasks_progress () =
  let reg = T.create_registry () in
  let task = T.create_task reg ~tool_name:"slow" in
  (match T.update_progress reg ~id:task.id ~progress:0.5 ~message:"halfway" () with
   | Ok () ->
     let t = Option.get (T.get_task reg ~id:task.id) in
     check bool "progress 50%" true (t.progress = Some 0.5);
     check (option string) "progress msg" (Some "halfway") t.progress_message
   | Error msg -> fail msg)

let test_tasks_input_required () =
  let reg = T.create_registry () in
  let task = T.create_task reg ~tool_name:"interactive" in
  (match T.request_input reg ~id:task.id with
   | Ok () ->
     let t = Option.get (T.get_task reg ~id:task.id) in
     check bool "input_required" true (t.state = T.Input_required);
     (* Can cancel from input_required *)
     (match T.cancel_task reg ~id:task.id with
      | Ok () -> check bool "cancelled" true
          ((Option.get (T.get_task reg ~id:task.id)).state = T.Cancelled)
      | Error msg -> fail msg)
   | Error msg -> fail msg)

let test_tasks_resume () =
  let reg = T.create_registry () in
  let task = T.create_task reg ~tool_name:"interactive" in
  ignore (T.request_input reg ~id:task.id);
  (match T.resume_task reg ~id:task.id with
   | Ok () ->
     check bool "working again" true
       ((Option.get (T.get_task reg ~id:task.id)).state = T.Working)
   | Error msg -> fail msg)

let test_tasks_list () =
  let reg = T.create_registry () in
  ignore (T.create_task reg ~tool_name:"a");
  ignore (T.create_task reg ~tool_name:"b");
  ignore (T.create_task reg ~tool_name:"c");
  check int "three tasks" 3 (List.length (T.list_tasks reg))

let test_tasks_json_roundtrip () =
  let reg = T.create_registry () in
  let task = T.create_task reg ~tool_name:"json-test" in
  ignore (T.update_progress reg ~id:task.id ~progress:0.75 ~message:"almost" ());
  let json = T.task_to_json task in
  match T.task_of_json json with
  | Ok decoded ->
    check string "id matches" task.id decoded.id;
    check string "tool_name matches" "json-test" decoded.tool_name;
    check bool "state matches" true (decoded.state = T.Working);
    check bool "progress matches" true (decoded.progress = Some 0.75);
    check (option string) "message matches" (Some "almost") decoded.progress_message
  | Error msg -> fail msg

let test_tasks_state_strings () =
  check string "working" "working" (T.task_state_to_string T.Working);
  check string "inputRequired" "inputRequired" (T.task_state_to_string T.Input_required);
  check string "completed" "completed" (T.task_state_to_string T.Completed);
  check string "failed" "failed" (T.task_state_to_string T.Failed);
  check string "cancelled" "cancelled" (T.task_state_to_string T.Cancelled);
  (* Round-trip *)
  check bool "working rt" true (T.task_state_of_string "working" = Ok T.Working);
  check bool "inputRequired rt" true (T.task_state_of_string "inputRequired" = Ok T.Input_required);
  check bool "input_required compat" true (T.task_state_of_string "input_required" = Ok T.Input_required);
  check bool "unknown fails" true (Result.is_error (T.task_state_of_string "bogus"))

let test_tasks_on_state_change () =
  let changes = ref [] in
  let reg = T.create_registry
    ~on_state_change:(fun task -> changes := (task.id, task.state) :: !changes)
    () in
  let task = T.create_task reg ~tool_name:"cb-test" in
  (* update_progress fires callback *)
  ignore (T.update_progress reg ~id:task.id ~progress:0.5 ());
  check int "1 callback after progress" 1 (List.length !changes);
  (* complete fires callback *)
  let result : P.tool_result = {
    content = [P.Text "done"];
    is_error = None;
    _meta = None;
  } in
  ignore (T.complete_task reg ~id:task.id ~result);
  check int "2 callbacks total" 2 (List.length !changes);
  (* Most recent is Completed *)
  let (_, last_state) = List.hd !changes in
  check bool "last state is completed" true (last_state = T.Completed)

let test_tasks_set_on_state_change () =
  let called = ref false in
  let reg = T.create_registry () in
  let task = T.create_task reg ~tool_name:"late-cb" in
  (* Set callback after creation *)
  T.set_on_state_change reg (fun _ -> called := true);
  ignore (T.fail_task reg ~id:task.id ~error:"oops");
  check bool "late-bound callback fires" true !called

let test_tasks_complete_from_failed () =
  let reg = T.create_registry () in
  let task = T.create_task reg ~tool_name:"cfail" in
  ignore (T.fail_task reg ~id:task.id ~error:"boom");
  let result = { P.content = [P.Text "late"]; is_error = None; _meta = None } in
  match T.complete_task reg ~id:task.id ~result with
  | Error msg -> check bool "mentions working state" true (String.length msg > 0)
  | Ok () -> fail "should not complete a Failed task"

let test_tasks_fail_from_completed () =
  let reg = T.create_registry () in
  let task = T.create_task reg ~tool_name:"fc" in
  let result = { P.content = [P.Text "ok"]; is_error = None; _meta = None } in
  ignore (T.complete_task reg ~id:task.id ~result);
  match T.fail_task reg ~id:task.id ~error:"too-late" with
  | Error msg -> check bool "mentions working state" true (String.length msg > 0)
  | Ok () -> fail "should not fail a Completed task"

let test_tasks_not_found () =
  let reg = T.create_registry () in
  let result = { P.content = [P.Text "x"]; is_error = None; _meta = None } in
  let check_err label r =
    match r with Error msg -> check bool label true (String.length msg > 0) | Ok () -> fail label
  in
  check_err "complete not found" (T.complete_task reg ~id:"ghost" ~result);
  check_err "fail not found" (T.fail_task reg ~id:"ghost" ~error:"e");
  check_err "cancel not found" (T.cancel_task reg ~id:"ghost");
  check_err "progress not found" (T.update_progress reg ~id:"ghost" ~progress:0.5 ());
  check_err "request_input not found" (T.request_input reg ~id:"ghost");
  check_err "resume not found" (T.resume_task reg ~id:"ghost");
  check (option (of_pp Fmt.nop)) "get not found" None (T.get_task reg ~id:"ghost")

let tasks_tests = [
  test_case "create" `Quick test_tasks_create;
  test_case "complete" `Quick test_tasks_complete;
  test_case "fail" `Quick test_tasks_fail;
  test_case "cancel" `Quick test_tasks_cancel;
  test_case "cancel completed fails" `Quick test_tasks_cancel_completed_fails;
  test_case "progress" `Quick test_tasks_progress;
  test_case "input required" `Quick test_tasks_input_required;
  test_case "resume" `Quick test_tasks_resume;
  test_case "list" `Quick test_tasks_list;
  test_case "json roundtrip" `Quick test_tasks_json_roundtrip;
  test_case "state strings" `Quick test_tasks_state_strings;
  test_case "on_state_change callback" `Quick test_tasks_on_state_change;
  test_case "set_on_state_change late" `Quick test_tasks_set_on_state_change;
  test_case "complete from failed" `Quick test_tasks_complete_from_failed;
  test_case "fail from completed" `Quick test_tasks_fail_from_completed;
  test_case "not found" `Quick test_tasks_not_found;
]

(** {1 Tool Annotations Tests} *)

let test_annotations_defaults () =
  let ann : P.tool_annotations = {
    title = None;
    read_only_hint = None;
    destructive_hint = None;
    idempotent_hint = None;
    open_world_hint = None;
  } in
  let json = P.tool_annotations_to_json ann in
  (* All None fields should produce empty object *)
  check bool "empty annotations" true
    (match json with `Assoc [] -> true | _ -> false)

let test_annotations_populated () =
  let ann : P.tool_annotations = {
    title = Some "Read File";
    read_only_hint = Some true;
    destructive_hint = Some false;
    idempotent_hint = Some true;
    open_world_hint = Some false;
  } in
  let json = P.tool_annotations_to_json ann in
  let json_str = Yojson.Safe.to_string json in
  check bool "has title" true (String.length json_str > 0);
  check bool "has readOnlyHint" true
    (Yojson.Safe.Util.member "readOnlyHint" json = `Bool true)

let test_annotations_in_tool () =
  let tool : P.tool = {
    name = "safe_read";
    description = Some "Read a file safely";
    input_schema = `Assoc [("type", `String "object")];
    annotations = Some {
      title = Some "Safe Read";
      read_only_hint = Some true;
      destructive_hint = Some false;
      idempotent_hint = Some true;
      open_world_hint = Some false;
    };
    icon = None;
  } in
  let json = P.tool_to_json tool in
  check bool "has annotations" true
    (Yojson.Safe.Util.member "annotations" json <> `Null)

let annotations_tests = [
  test_case "defaults (empty)" `Quick test_annotations_defaults;
  test_case "populated" `Quick test_annotations_populated;
  test_case "in tool" `Quick test_annotations_in_tool;
]

(** {1 Icon Tests} *)

let test_icon_encoding () =
  let icon : P.icon = { uri = "https://example.com/icon.png" } in
  let json = P.icon_to_json icon in
  check bool "has uri" true
    (Yojson.Safe.Util.member "uri" json = `String "https://example.com/icon.png")

let test_icon_in_tool () =
  let tool : P.tool = {
    name = "with_icon";
    description = None;
    input_schema = `Assoc [];
    annotations = None;
    icon = Some { uri = "data:image/svg+xml;base64,abc" };
  } in
  let json = P.tool_to_json tool in
  check bool "has icon field" true
    (Yojson.Safe.Util.member "icon" json <> `Null)

let icon_tests = [
  test_case "encoding" `Quick test_icon_encoding;
  test_case "in tool" `Quick test_icon_in_tool;
]

(** {1 Meta Tests} *)

let test_meta_extraction () =
  let params = Some (`Assoc [
    ("name", `String "test");
    ("_meta", `Assoc [("progressToken", `String "tok-1")]);
  ]) in
  match J.extract_meta params with
  | Some meta ->
    check bool "has progressToken" true
      (Yojson.Safe.Util.member "progressToken" meta = `String "tok-1")
  | None -> fail "expected _meta"

let test_meta_absent () =
  let params = Some (`Assoc [("name", `String "test")]) in
  check bool "no _meta" true (Option.is_none (J.extract_meta params))

let test_meta_null_params () =
  check bool "null params" true (Option.is_none (J.extract_meta None))

let meta_tests = [
  test_case "extraction" `Quick test_meta_extraction;
  test_case "absent" `Quick test_meta_absent;
  test_case "null params" `Quick test_meta_null_params;
]

(** {1 Capabilities Tests} *)

let test_capabilities_structured () =
  let caps : P.server_capabilities = {
    tools = Some { list_changed = Some true };
    resources = Some { subscribe = Some true; list_changed = None };
    prompts = Some { list_changed = None };
    logging = Some true;
  } in
  let json = P.server_capabilities_to_json caps in
  let tools_json = Yojson.Safe.Util.member "tools" json in
  check bool "tools not null" true (tools_json <> `Null);
  check bool "tools.listChanged" true
    (Yojson.Safe.Util.member "listChanged" tools_json = `Bool true)

let test_capabilities_empty () =
  let caps : P.server_capabilities = {
    tools = None;
    resources = None;
    prompts = None;
    logging = None;
  } in
  let json = P.server_capabilities_to_json caps in
  check bool "tools is null" true
    (Yojson.Safe.Util.member "tools" json = `Null)

let capabilities_tests = [
  test_case "structured" `Quick test_capabilities_structured;
  test_case "empty" `Quick test_capabilities_empty;
]

(** {1 Adapter (Streamable HTTP) Tests} *)

module A = Kirin.Mcp

let test_adapter_session_header () =
  check string "header name" "mcp-session-id" A.session_header

let test_adapter_is_init_single () =
  let json = `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int 1);
    ("method", `String P.Method.initialize);
    ("params", `Assoc []);
  ] in
  check bool "single initialize request" true
    (A.is_initialize_request json)

let test_adapter_is_init_batch () =
  let init_req = `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int 1);
    ("method", `String P.Method.initialize);
  ] in
  let ping_req = `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int 2);
    ("method", `String P.Method.ping);
  ] in
  check bool "batch containing initialize" true
    (A.is_initialize_request (`List [init_req; ping_req]));
  check bool "batch without initialize" false
    (A.is_initialize_request (`List [ping_req]))

let test_adapter_is_init_non_init () =
  let json = `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int 1);
    ("method", `String P.Method.tools_list);
  ] in
  check bool "tools/list is not initialize" false
    (A.is_initialize_request json)

let test_adapter_is_init_malformed () =
  check bool "null is not initialize" false
    (A.is_initialize_request `Null);
  check bool "string is not initialize" false
    (A.is_initialize_request (`String "hello"));
  check bool "object without method" false
    (A.is_initialize_request (`Assoc [("id", `Int 1)]));
  check bool "empty batch" false
    (A.is_initialize_request (`List []))

let adapter_tests = [
  test_case "session header constant" `Quick test_adapter_session_header;
  test_case "is_initialize single" `Quick test_adapter_is_init_single;
  test_case "is_initialize batch" `Quick test_adapter_is_init_batch;
  test_case "is_initialize non-init" `Quick test_adapter_is_init_non_init;
  test_case "is_initialize malformed" `Quick test_adapter_is_init_malformed;
]

(** {1 Transport Tests} *)

module Tr = Kirin_mcp.Transport

let test_transport_stdio_create () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let r, w = Eio_unix.pipe sw in
  let ic = Eio.Buf_read.of_flow r ~max_size:4096 in
  Eio.Buf_write.with_flow w @@ fun oc ->
  let transport = Tr.of_stdio ~ic ~oc in
  check bool "is_stdio" true (Tr.is_stdio transport);
  check bool "not streamable" false (Tr.is_streamable_http transport)

let test_transport_streamable_http_create () =
  Eio_main.run @@ fun _env ->
  let transport = Tr.of_streamable_http () in
  check bool "is_streamable_http" true (Tr.is_streamable_http transport);
  check bool "not stdio" false (Tr.is_stdio transport)

let test_transport_session_id_stdio () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let r, w = Eio_unix.pipe sw in
  let ic = Eio.Buf_read.of_flow r ~max_size:4096 in
  Eio.Buf_write.with_flow w @@ fun oc ->
  let transport = Tr.of_stdio ~ic ~oc in
  check (option string) "stdio session_id is None" None (Tr.session_id transport);
  Tr.set_session_id transport "ignored";
  check (option string) "still None after set" None (Tr.session_id transport)

let test_transport_session_id_streamable () =
  Eio_main.run @@ fun _env ->
  let transport = Tr.of_streamable_http () in
  check (option string) "initial session_id is None" None (Tr.session_id transport);
  Tr.set_session_id transport "test-session-42";
  check (option string) "session_id after set" (Some "test-session-42") (Tr.session_id transport)

let test_transport_queue_message_streamable () =
  Eio_main.run @@ fun _env ->
  let transport = Tr.of_streamable_http () in
  let msg = J.Notification (J.make_notification ~method_:"test/ping" ()) in
  Tr.queue_message transport msg;
  let read_msg = Tr.read_message transport in
  match read_msg with
  | J.Notification n ->
    check string "method matches" "test/ping" n.method_
  | _ -> fail "expected Notification"

let test_transport_queue_on_stdio_fails () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let r, w = Eio_unix.pipe sw in
  let ic = Eio.Buf_read.of_flow r ~max_size:4096 in
  Eio.Buf_write.with_flow w @@ fun oc ->
  let transport = Tr.of_stdio ~ic ~oc in
  let msg = J.Notification (J.make_notification ~method_:"test/ping" ()) in
  match Tr.queue_message transport msg with
  | exception Tr.Transport_error _ -> ()
  | _ -> fail "expected Transport_error"

let transport_tests = [
  test_case "stdio create" `Quick test_transport_stdio_create;
  test_case "streamable http create" `Quick test_transport_streamable_http_create;
  test_case "session id stdio" `Quick test_transport_session_id_stdio;
  test_case "session id streamable" `Quick test_transport_session_id_streamable;
  test_case "queue message streamable" `Quick test_transport_queue_message_streamable;
  test_case "queue on stdio fails" `Quick test_transport_queue_on_stdio_fails;
]

(** {1 Client Tests} *)

module C = Kirin_mcp.Client

let test_client_create () =
  Eio_main.run @@ fun _env ->
  let transport = Tr.of_streamable_http () in
  let client = C.create transport in
  check bool "not initialized" false (C.is_initialized client);
  check (option (of_pp Fmt.nop)) "no server_caps" None (C.server_capabilities client);
  check (option (of_pp Fmt.nop)) "no server_info" None (C.server_info client)

let test_client_next_id () =
  Eio_main.run @@ fun _env ->
  let transport = Tr.of_streamable_http () in
  let client = C.create transport in
  let id0 = C.next_id client in
  let id1 = C.next_id client in
  let id2 = C.next_id client in
  check bool "id0 is Int 0" true (id0 = J.Int 0);
  check bool "id1 is Int 1" true (id1 = J.Int 1);
  check bool "id2 is Int 2" true (id2 = J.Int 2)

let test_client_next_id_monotonic () =
  Eio_main.run @@ fun _env ->
  let transport = Tr.of_streamable_http () in
  let client = C.create transport in
  let ids = List.init 100 (fun _ -> C.next_id client) in
  let ints = List.map (function J.Int i -> i | _ -> -1) ids in
  let rec is_sorted = function
    | [] | [_] -> true
    | a :: (b :: _ as rest) -> a < b && is_sorted rest
  in
  check bool "100 IDs strictly increasing" true (is_sorted ints)

let client_tests = [
  test_case "create" `Quick test_client_create;
  test_case "next_id" `Quick test_client_next_id;
  test_case "next_id monotonic" `Quick test_client_next_id_monotonic;
]

(** {1 Main} *)

let () =
  run "Mcp" [
    ("Jsonrpc", jsonrpc_tests);
    ("Schema", schema_tests);
    ("Server", server_tests);
    ("Protocol", protocol_tests);
    ("Session", session_tests);
    ("Tasks", tasks_tests);
    ("Annotations", annotations_tests);
    ("Icons", icon_tests);
    ("Meta", meta_tests);
    ("Capabilities", capabilities_tests);
    ("Adapter", adapter_tests);
    ("Transport", transport_tests);
    ("Client", client_tests);
  ]
