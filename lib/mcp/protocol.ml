(** Kirin MCP - Protocol Types

    MCP (Model Context Protocol) 2024-11 specification types.
    Defines all MCP message structures for tools, resources, and prompts.
*)

(** {1 Basic Types} *)

(** Implementation info (client or server) *)
type implementation_info = {
  name : string;
  version : string;
}

(** {1 Capabilities} *)

(** Server capabilities *)
type server_capabilities = {
  tools : bool option;
  resources : bool option;
  prompts : bool option;
  logging : bool option;
}

(** Client capabilities *)
type client_capabilities = {
  roots : bool option;
  sampling : bool option;
}

(** {1 Tools} *)

(** Tool definition *)
type tool = {
  name : string;
  description : string option;
  input_schema : Yojson.Safe.t;
}

(** Tool call request *)
type tool_call = {
  name : string;
  arguments : Yojson.Safe.t option;
}

(** Tool call result *)
type tool_result = {
  content : content list;
  is_error : bool option;
}

(** Content types *)
and content =
  | Text of string
  | Image of { data : string; mime_type : string }
  | Resource of { uri : string; text : string option; blob : string option }

(** {1 Resources} *)

(** Resource definition *)
type resource = {
  uri : string;
  name : string;
  description : string option;
  mime_type : string option;
}

(** Resource contents *)
type resource_contents = {
  uri : string;
  mime_type : string option;
  text : string option;
  blob : string option;
}

(** {1 Prompts} *)

(** Prompt argument definition *)
type prompt_argument = {
  name : string;
  description : string option;
  required : bool option;
}

(** Prompt definition *)
type prompt = {
  name : string;
  description : string option;
  arguments : prompt_argument list option;
}

(** Prompt message *)
type prompt_message = {
  role : [ `user | `assistant ];
  content : content;
}

(** {1 Initialize} *)

(** Initialize request params *)
type initialize_params = {
  protocol_version : string;
  capabilities : client_capabilities;
  client_info : implementation_info;
}

(** Initialize result *)
type initialize_result = {
  protocol_version : string;
  capabilities : server_capabilities;
  server_info : implementation_info;
}

(** {1 JSON Encoding} *)

let implementation_info_to_json (info : implementation_info) =
  `Assoc [
    "name", `String info.name;
    "version", `String info.version;
  ]

let server_capabilities_to_json (caps : server_capabilities) =
  let fields = [] in
  let fields = match caps.tools with
    | Some true -> ("tools", `Assoc []) :: fields
    | _ -> fields
  in
  let fields = match caps.resources with
    | Some true -> ("resources", `Assoc []) :: fields
    | _ -> fields
  in
  let fields = match caps.prompts with
    | Some true -> ("prompts", `Assoc []) :: fields
    | _ -> fields
  in
  let fields = match caps.logging with
    | Some true -> ("logging", `Assoc []) :: fields
    | _ -> fields
  in
  `Assoc fields

let client_capabilities_to_json (caps : client_capabilities) =
  let fields = [] in
  let fields = match caps.roots with
    | Some true -> ("roots", `Assoc []) :: fields
    | _ -> fields
  in
  let fields = match caps.sampling with
    | Some true -> ("sampling", `Assoc []) :: fields
    | _ -> fields
  in
  `Assoc fields

let tool_to_json (t : tool) =
  let base = [
    "name", `String t.name;
    "inputSchema", t.input_schema;
  ] in
  let with_desc = match t.description with
    | Some d -> ("description", `String d) :: base
    | None -> base
  in
  `Assoc with_desc

let content_to_json = function
  | Text text -> `Assoc [
      "type", `String "text";
      "text", `String text;
    ]
  | Image { data; mime_type } -> `Assoc [
      "type", `String "image";
      "data", `String data;
      "mimeType", `String mime_type;
    ]
  | Resource { uri; text; blob } ->
    let base = [
      "type", `String "resource";
      "resource", `Assoc (
        ["uri", `String uri] @
        (match text with Some t -> ["text", `String t] | None -> []) @
        (match blob with Some b -> ["blob", `String b] | None -> [])
      );
    ] in
    `Assoc base

let tool_result_to_json (result : tool_result) =
  let content_json = `List (List.map content_to_json result.content) in
  let base = ["content", content_json] in
  let with_error = match result.is_error with
    | Some true -> ("isError", `Bool true) :: base
    | _ -> base
  in
  `Assoc with_error

let resource_to_json (res : resource) =
  let base = [
    "uri", `String res.uri;
    "name", `String res.name;
  ] in
  let with_desc = match res.description with
    | Some d -> ("description", `String d) :: base
    | None -> base
  in
  let with_mime = match res.mime_type with
    | Some m -> ("mimeType", `String m) :: with_desc
    | None -> with_desc
  in
  `Assoc with_mime

let resource_contents_to_json (rc : resource_contents) =
  let base = ["uri", `String rc.uri] in
  let with_mime = match rc.mime_type with
    | Some m -> ("mimeType", `String m) :: base
    | None -> base
  in
  let with_text = match rc.text with
    | Some t -> ("text", `String t) :: with_mime
    | None -> with_mime
  in
  let with_blob = match rc.blob with
    | Some b -> ("blob", `String b) :: with_text
    | None -> with_text
  in
  `Assoc with_blob

let prompt_argument_to_json (arg : prompt_argument) =
  let base = ["name", `String arg.name] in
  let with_desc = match arg.description with
    | Some d -> ("description", `String d) :: base
    | None -> base
  in
  let with_req = match arg.required with
    | Some r -> ("required", `Bool r) :: with_desc
    | None -> with_desc
  in
  `Assoc with_req

let prompt_to_json (p : prompt) =
  let base = ["name", `String p.name] in
  let with_desc = match p.description with
    | Some d -> ("description", `String d) :: base
    | None -> base
  in
  let with_args = match p.arguments with
    | Some args -> ("arguments", `List (List.map prompt_argument_to_json args)) :: with_desc
    | None -> with_desc
  in
  `Assoc with_args

let initialize_result_to_json (result : initialize_result) =
  `Assoc [
    "protocolVersion", `String result.protocol_version;
    "capabilities", server_capabilities_to_json result.capabilities;
    "serverInfo", implementation_info_to_json result.server_info;
  ]

(** {1 JSON Decoding} *)

let implementation_info_of_json json =
  let open Yojson.Safe.Util in
  {
    name = json |> member "name" |> to_string;
    version = json |> member "version" |> to_string;
  }

let client_capabilities_of_json json =
  let open Yojson.Safe.Util in
  {
    roots = (match json |> member "roots" with `Null -> None | _ -> Some true);
    sampling = (match json |> member "sampling" with `Null -> None | _ -> Some true);
  }

let initialize_params_of_json json =
  let open Yojson.Safe.Util in
  {
    protocol_version = json |> member "protocolVersion" |> to_string;
    capabilities = json |> member "capabilities" |> client_capabilities_of_json;
    client_info = json |> member "clientInfo" |> implementation_info_of_json;
  }

let tool_call_of_json json =
  let open Yojson.Safe.Util in
  {
    name = json |> member "name" |> to_string;
    arguments = match json |> member "arguments" with
      | `Null -> None
      | args -> Some args;
  }

(** {1 Protocol Version} *)

let protocol_version = "2024-11-05"

(** {1 Method Names} *)

module Method = struct
  let initialize = "initialize"
  let initialized = "notifications/initialized"
  let ping = "ping"

  (* Tools *)
  let tools_list = "tools/list"
  let tools_call = "tools/call"

  (* Resources *)
  let resources_list = "resources/list"
  let resources_read = "resources/read"
  let resources_subscribe = "resources/subscribe"
  let resources_unsubscribe = "resources/unsubscribe"

  (* Prompts *)
  let prompts_list = "prompts/list"
  let prompts_get = "prompts/get"

  (* Logging *)
  let logging_set_level = "logging/setLevel"

  (* Notifications *)
  let cancelled = "notifications/cancelled"
  let progress = "notifications/progress"
  let resources_updated = "notifications/resources/updated"
  let resources_list_changed = "notifications/resources/list_changed"
  let tools_list_changed = "notifications/tools/list_changed"
  let prompts_list_changed = "notifications/prompts/list_changed"
end
