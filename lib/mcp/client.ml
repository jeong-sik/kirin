(** Kirin MCP - Client Implementation

    MCP client that connects to external MCP servers.
    Designed for Eio direct-style async.
*)

(** {1 Types} *)

(** MCP Client *)
type t = {
  transport : Transport.t;
  mutable server_capabilities : Protocol.server_capabilities option;
  mutable server_info : Protocol.implementation_info option;
  mutable request_id : int;
}

(** Client error *)
exception Client_error of string

(** {1 Constructor} *)

(** Create a client connected to a transport *)
let create transport =
  {
    transport;
    server_capabilities = None;
    server_info = None;
    request_id = 0;
  }

(** {1 Request ID Generation} *)

(** Generate next request ID *)
let next_id t =
  let id = t.request_id in
  t.request_id <- id + 1;
  Jsonrpc.Int id

(** {1 Low-Level Communication} *)

(** Send a request and wait for response *)
let send_request t ~method_ ?params () =
  let id = next_id t in
  let request = Jsonrpc.make_request ~id ~method_ ?params () in
  let response = Transport.send_request t.transport request in
  match response.error with
  | Some err ->
    raise (Client_error (Printf.sprintf "RPC error %d: %s"
                           (Jsonrpc.error_code_to_int err.code) err.message))
  | None ->
    response.result

(** Send a notification (no response expected) *)
let send_notification t ~method_ ?params () =
  let notif = Jsonrpc.make_notification ~method_ ?params () in
  Transport.send_notification t.transport notif

(** {1 Initialization} *)

(** Initialize connection with server *)
let initialize t ?(client_name = "kirin-client") ?(client_version = "1.0.0") () =
  let params = `Assoc [
    "protocolVersion", `String Protocol.protocol_version;
    "capabilities", Protocol.client_capabilities_to_json {
      Protocol.roots = None;
      sampling = None;
      experimental = None;
    };
    "clientInfo", Protocol.implementation_info_to_json {
      Protocol.name = client_name;
      version = client_version;
      description = None;
    };
  ] in
  match send_request t ~method_:Protocol.Method.initialize ~params () with
  | Some result ->
    let open Yojson.Safe.Util in
    t.server_info <- Some {
      Protocol.name = result |> member "serverInfo" |> member "name" |> to_string;
      version = result |> member "serverInfo" |> member "version" |> to_string;
      description = (match result |> member "serverInfo" |> member "description" with
        | `String s -> Some s
        | _ -> None);
    };
    (* Parse server capabilities *)
    let caps_json = result |> member "capabilities" in
    t.server_capabilities <- Some {
      Protocol.tools = (match caps_json |> member "tools" with
        | `Null -> None
        | _ -> Some { Protocol.list_changed = None });
      resources = (match caps_json |> member "resources" with
        | `Null -> None
        | _ -> Some { Protocol.subscribe = None; list_changed = None });
      prompts = (match caps_json |> member "prompts" with
        | `Null -> None
        | _ -> Some { Protocol.list_changed = None });
      logging = (match caps_json |> member "logging" with
        | `Null -> None
        | _ -> Some true);
    };
    (* Send initialized notification *)
    send_notification t ~method_:Protocol.Method.initialized ();
    t.server_capabilities
  | None ->
    raise (Client_error "No result from initialize")

(** {1 Tools} *)

(** List available tools *)
let list_tools t =
  match send_request t ~method_:Protocol.Method.tools_list () with
  | Some result ->
    let open Yojson.Safe.Util in
    result |> member "tools" |> to_list |> List.map (fun json ->
      {
        Protocol.name = json |> member "name" |> to_string;
        description = (match json |> member "description" with
          | `String s -> Some s
          | _ -> None);
        input_schema = json |> member "inputSchema";
        annotations = None;
        icon = None;
      }
    )
  | None -> []

(** Call a tool *)
let call_tool t ~name ?(arguments = `Null) () =
  let params = `Assoc [
    "name", `String name;
    "arguments", arguments;
  ] in
  match send_request t ~method_:Protocol.Method.tools_call ~params () with
  | Some result ->
    let open Yojson.Safe.Util in
    let content = result |> member "content" |> to_list in
    let is_error = match result |> member "isError" with
      | `Bool b -> Some b
      | _ -> None
    in
    {
      Protocol.content = List.map (fun c ->
        match c |> member "type" |> to_string with
        | "text" -> Protocol.Text (c |> member "text" |> to_string)
        | "image" -> Protocol.Image {
            data = c |> member "data" |> to_string;
            mime_type = c |> member "mimeType" |> to_string;
          }
        | _ -> Protocol.Text ""
      ) content;
      is_error;
      _meta = None;
    }
  | None ->
    raise (Client_error "No result from tool call")

(** {1 Resources} *)

(** List available resources *)
let list_resources t =
  match send_request t ~method_:Protocol.Method.resources_list () with
  | Some result ->
    let open Yojson.Safe.Util in
    result |> member "resources" |> to_list |> List.map (fun json ->
      {
        Protocol.uri = json |> member "uri" |> to_string;
        name = json |> member "name" |> to_string;
        description = (match json |> member "description" with
          | `String s -> Some s
          | _ -> None);
        mime_type = (match json |> member "mimeType" with
          | `String s -> Some s
          | _ -> None);
        icon = None;
      }
    )
  | None -> []

(** Read a resource *)
let read_resource t ~uri =
  let params = `Assoc ["uri", `String uri] in
  match send_request t ~method_:Protocol.Method.resources_read ~params () with
  | Some result ->
    let open Yojson.Safe.Util in
    let contents = result |> member "contents" |> to_list in
    (match contents with
     | first :: _ ->
       {
         Protocol.uri = first |> member "uri" |> to_string;
         mime_type = (match first |> member "mimeType" with
           | `String s -> Some s
           | _ -> None);
         text = (match first |> member "text" with
           | `String s -> Some s
           | _ -> None);
         blob = (match first |> member "blob" with
           | `String s -> Some s
           | _ -> None);
       }
     | [] -> raise (Client_error "Empty resource contents"))
  | None ->
    raise (Client_error "No result from resource read")

(** {1 Prompts} *)

(** List available prompts *)
let list_prompts t =
  match send_request t ~method_:Protocol.Method.prompts_list () with
  | Some result ->
    let open Yojson.Safe.Util in
    result |> member "prompts" |> to_list |> List.map (fun json ->
      {
        Protocol.name = json |> member "name" |> to_string;
        description = (match json |> member "description" with
          | `String s -> Some s
          | _ -> None);
        arguments = (match json |> member "arguments" with
          | `List args -> Some (List.map (fun arg ->
              {
                Protocol.name = arg |> member "name" |> to_string;
                description = (match arg |> member "description" with
                  | `String s -> Some s
                  | _ -> None);
                required = (match arg |> member "required" with
                  | `Bool b -> Some b
                  | _ -> None);
              }
            ) args)
          | _ -> None);
        icon = None;
      }
    )
  | None -> []

(** {1 Ping} *)

(** Ping the server *)
let ping t =
  match send_request t ~method_:Protocol.Method.ping () with
  | Some _ -> true
  | None -> true (* Ping returns empty result on success *)

(** {1 Accessors} *)

(** Get server capabilities (after initialize) *)
let server_capabilities t = t.server_capabilities

(** Get server info (after initialize) *)
let server_info t = t.server_info

(** Check if connected and initialized *)
let is_initialized t = Option.is_some t.server_capabilities
