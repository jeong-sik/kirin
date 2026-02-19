(** Kirin MCP - Server Implementation

    MCP server that exposes tools, resources, and prompts to AI agents.
    Designed for Eio direct-style async.
*)

(** {1 Types} *)

(** Tool handler function *)
type tool_handler = Yojson.Safe.t -> Yojson.Safe.t

(** Resource handler function: returns (content, mime_type) *)
type resource_handler = unit -> string * string

(** Prompt handler function: returns list of messages *)
type prompt_handler = (string * string) list -> Protocol.prompt_message list

(** Registered tool *)
type registered_tool = {
  tool : Protocol.tool;
  handler : tool_handler;
}

(** Registered resource *)
type registered_resource = {
  resource : Protocol.resource;
  handler : resource_handler;
}

(** Registered prompt *)
type registered_prompt = {
  prompt : Protocol.prompt;
  handler : prompt_handler option;
}

(** MCP Server *)
type t = {
  mutable tools : registered_tool list;
  mutable resources : registered_resource list;
  mutable prompts : registered_prompt list;
  session : Session.t;
  task_registry : Tasks.registry;
}

(** {1 Constructor} *)

(** Create a new MCP server *)
let create ?(name = "kirin-mcp") ?(version = "1.0.0") () =
  {
    tools = [];
    resources = [];
    prompts = [];
    session = Session.create ~server_name:name ~server_version:version ();
    task_registry = Tasks.create_registry ();
  }

(** {1 Tool Registration} *)

(** Add a tool to the server *)
let add_tool t ~name ~description ~schema ~handler =
  Session.enable_tools t.session;
  let tool = {
    Protocol.name;
    description = Some description;
    input_schema = schema;
    annotations = None;
    icon = None;
  } in
  t.tools <- { tool; handler } :: t.tools

(** List all tools *)
let list_tools t =
  List.map (fun rt -> rt.tool) t.tools

(** Call a tool by name *)
let call_tool t ~name ~arguments =
  match List.find_opt (fun rt -> rt.tool.name = name) t.tools with
  | Some rt ->
    let args = Option.value arguments ~default:`Null in
    let result = rt.handler args in
    Ok {
      Protocol.content = [Protocol.Text (Yojson.Safe.to_string result)];
      is_error = None;
      _meta = None;
    }
  | None ->
    Error (Printf.sprintf "Tool not found: %s" name)

(** {1 Resource Registration} *)

(** Add a resource to the server *)
let add_resource t ~uri ~name ?description ?mime_type ~handler () =
  Session.enable_resources t.session;
  let resource = {
    Protocol.uri;
    name;
    description;
    mime_type;
    icon = None;
  } in
  t.resources <- { resource; handler } :: t.resources

(** List all resources *)
let list_resources t =
  List.map (fun rr -> rr.resource) t.resources

(** Read a resource by URI *)
let read_resource t ~uri =
  match List.find_opt (fun rr -> rr.resource.uri = uri) t.resources with
  | Some rr ->
    let text, mime_type = rr.handler () in
    Ok {
      Protocol.uri;
      mime_type = Some mime_type;
      text = Some text;
      blob = None;
    }
  | None ->
    Error (Printf.sprintf "Resource not found: %s" uri)

(** {1 Prompt Registration} *)

(** Add a prompt to the server *)
let add_prompt t ~name ?description ?arguments ?handler () =
  Session.enable_prompts t.session;
  let prompt = {
    Protocol.name;
    description;
    arguments;
    icon = None;
  } in
  t.prompts <- { prompt; handler } :: t.prompts

(** List all prompts *)
let list_prompts t =
  List.map (fun rp -> rp.prompt) t.prompts

(** {1 Request Handling} *)

(** Handle a JSON-RPC request *)
let handle_request t (req : Jsonrpc.request) : Jsonrpc.response =
  let open Protocol.Method in
  match req.method_ with
  | m when m = initialize ->
    (match req.params with
     | Some params ->
       let init_params = Protocol.initialize_params_of_json params in
       (match Session.handle_initialize t.session init_params with
        | Ok result ->
          Jsonrpc.success_response ~id:req.id
            (Protocol.initialize_result_to_json result)
        | Error msg ->
          Jsonrpc.error_response ~id:req.id
            (Jsonrpc.make_error ~code:Jsonrpc.Invalid_request ~message:msg ()))
     | None ->
       Jsonrpc.error_response ~id:req.id
         (Jsonrpc.make_error ~code:Jsonrpc.Invalid_params
            ~message:"Missing initialize params" ()))

  | m when m = ping ->
    Jsonrpc.success_response ~id:req.id `Null

  | m when m = tools_list ->
    let tools_json = `List (List.map Protocol.tool_to_json (list_tools t)) in
    Jsonrpc.success_response ~id:req.id (`Assoc ["tools", tools_json])

  | m when m = tools_call ->
    (match req.params with
     | Some params ->
       let call = Protocol.tool_call_of_json params in
       (match call_tool t ~name:call.name ~arguments:call.arguments with
        | Ok result ->
          Jsonrpc.success_response ~id:req.id (Protocol.tool_result_to_json result)
        | Error msg ->
          Jsonrpc.error_response ~id:req.id
            (Jsonrpc.make_error ~code:Jsonrpc.Invalid_params ~message:msg ()))
     | None ->
       Jsonrpc.error_response ~id:req.id
         (Jsonrpc.make_error ~code:Jsonrpc.Invalid_params
            ~message:"Missing tool call params" ()))

  | m when m = resources_list ->
    let resources_json = `List (List.map Protocol.resource_to_json (list_resources t)) in
    Jsonrpc.success_response ~id:req.id (`Assoc ["resources", resources_json])

  | m when m = resources_read ->
    (match req.params with
     | Some params ->
       let uri = Yojson.Safe.Util.(params |> member "uri" |> to_string) in
       (match read_resource t ~uri with
        | Ok contents ->
          Jsonrpc.success_response ~id:req.id
            (`Assoc ["contents", `List [Protocol.resource_contents_to_json contents]])
        | Error msg ->
          Jsonrpc.error_response ~id:req.id
            (Jsonrpc.make_error ~code:Jsonrpc.Invalid_params ~message:msg ()))
     | None ->
       Jsonrpc.error_response ~id:req.id
         (Jsonrpc.make_error ~code:Jsonrpc.Invalid_params
            ~message:"Missing resource read params" ()))

  | m when m = prompts_list ->
    let prompts_json = `List (List.map Protocol.prompt_to_json (list_prompts t)) in
    Jsonrpc.success_response ~id:req.id (`Assoc ["prompts", prompts_json])

  | m when m = tasks_get ->
    (match req.params with
     | Some params ->
       let id = Yojson.Safe.Util.(params |> member "id" |> to_string) in
       (match Tasks.get_task t.task_registry ~id with
        | Some task ->
          Jsonrpc.success_response ~id:req.id
            (`Assoc ["task", Tasks.task_to_json task])
        | None ->
          Jsonrpc.error_response ~id:req.id
            (Jsonrpc.make_error ~code:Jsonrpc.Invalid_params
               ~message:(Printf.sprintf "Task not found: %s" id) ()))
     | None ->
       Jsonrpc.error_response ~id:req.id
         (Jsonrpc.make_error ~code:Jsonrpc.Invalid_params
            ~message:"Missing task ID" ()))

  | m when m = tasks_list ->
    let tasks_json = `List (List.map Tasks.task_to_json (Tasks.list_tasks t.task_registry)) in
    Jsonrpc.success_response ~id:req.id (`Assoc ["tasks", tasks_json])

  | m when m = tasks_cancel ->
    (match req.params with
     | Some params ->
       let id = Yojson.Safe.Util.(params |> member "id" |> to_string) in
       (match Tasks.cancel_task t.task_registry ~id with
        | Ok () ->
          Jsonrpc.success_response ~id:req.id (`Assoc [])
        | Error msg ->
          Jsonrpc.error_response ~id:req.id
            (Jsonrpc.make_error ~code:Jsonrpc.Invalid_params ~message:msg ()))
     | None ->
       Jsonrpc.error_response ~id:req.id
         (Jsonrpc.make_error ~code:Jsonrpc.Invalid_params
            ~message:"Missing task ID" ()))

  | m when m = tasks_result ->
    (match req.params with
     | Some params ->
       let id = Yojson.Safe.Util.(params |> member "id" |> to_string) in
       (match Tasks.get_task t.task_registry ~id with
        | Some task when task.state = Tasks.Completed ->
          (match task.result with
           | Some result ->
             Jsonrpc.success_response ~id:req.id
               (Protocol.tool_result_to_json result)
           | None ->
             Jsonrpc.error_response ~id:req.id
               (Jsonrpc.make_error ~code:Jsonrpc.Internal_error
                  ~message:"Task completed but no result available" ()))
        | Some _ ->
          Jsonrpc.error_response ~id:req.id
            (Jsonrpc.make_error ~code:Jsonrpc.Invalid_params
               ~message:"Task is not completed" ())
        | None ->
          Jsonrpc.error_response ~id:req.id
            (Jsonrpc.make_error ~code:Jsonrpc.Invalid_params
               ~message:(Printf.sprintf "Task not found: %s" id) ()))
     | None ->
       Jsonrpc.error_response ~id:req.id
         (Jsonrpc.make_error ~code:Jsonrpc.Invalid_params
            ~message:"Missing task ID" ()))

  | _ ->
    Jsonrpc.error_response ~id:req.id
      (Jsonrpc.make_error ~code:Jsonrpc.Method_not_found
         ~message:(Printf.sprintf "Unknown method: %s" req.method_) ())

(** Handle a JSON-RPC notification *)
let handle_notification t (notif : Jsonrpc.notification) =
  let open Protocol.Method in
  match notif.method_ with
  | m when m = initialized ->
    let _ = Session.handle_initialized t.session in
    ()
  | m when m = cancelled ->
    (* Handle cancellation - for now just log *)
    ()
  | _ ->
    (* Unknown notification - ignore *)
    ()

(** Handle any incoming message *)
let handle_message t (msg : Jsonrpc.message) : Jsonrpc.message option =
  match msg with
  | Jsonrpc.Request req ->
    Some (Jsonrpc.Response (handle_request t req))
  | Jsonrpc.Notification notif ->
    handle_notification t notif;
    None
  | Jsonrpc.Response _ ->
    (* Server shouldn't receive responses *)
    None

(** {1 Server Loop} *)

(** Run the server on a transport (blocking, direct-style) *)
let run t transport =
  let rec loop () =
    let msg = Transport.read_message transport in
    (match handle_message t msg with
     | Some response -> Transport.write_message transport response
     | None -> ());
    if Session.is_ready t.session || Session.state t.session = Session.Initializing then
      loop ()
  in
  loop ()

(** {1 Accessors} *)

(** Get the session *)
let session t = t.session
