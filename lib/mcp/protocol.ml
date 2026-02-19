(** Kirin MCP - Protocol Types

    MCP (Model Context Protocol) 2025-11-25 specification types.
    Defines all MCP message structures for tools, resources, and prompts.
*)

(** {1 Basic Types} *)

(** Implementation info (client or server) *)
type implementation_info = {
  name : string;
  version : string;
  description : string option;  (** Human-readable description (2025-11-25) *)
}

(** Icon metadata for tools, resources, and prompts (2025-11-25) *)
type icon = {
  uri : string;  (** data: URI or https: URI *)
}

(** {1 Capabilities} *)

(** Tool annotations — behavioral hints for AI agents (2025-11-25) *)
type tool_annotations = {
  title : string option;              (** Human-readable title *)
  read_only_hint : bool option;       (** Hint: tool only reads data (default true) *)
  destructive_hint : bool option;     (** Hint: tool may have destructive side-effects (default true) *)
  idempotent_hint : bool option;      (** Hint: calling repeatedly has same effect (default false) *)
  open_world_hint : bool option;      (** Hint: tool interacts with external entities (default true) *)
}

(** Server tools capability (2025-11-25) *)
type tools_capability = {
  list_changed : bool option;  (** Supports notifications/tools/list_changed *)
}

(** Server resources capability (2025-11-25) *)
type resources_capability = {
  subscribe : bool option;     (** Supports resources/subscribe *)
  list_changed : bool option;  (** Supports notifications/resources/list_changed *)
}

(** Server prompts capability (2025-11-25) *)
type prompts_capability = {
  list_changed : bool option;  (** Supports notifications/prompts/list_changed *)
}

(** Server capabilities *)
type server_capabilities = {
  tools : tools_capability option;
  resources : resources_capability option;
  prompts : prompts_capability option;
  logging : bool option;  (** Empty object = supported *)
}

(** Roots capability for client *)
type roots_capability = {
  list_changed : bool option;
}

(** Sampling capability for client *)
type sampling_capability = unit  (** Empty object = supported *)

(** Client capabilities *)
type client_capabilities = {
  roots : roots_capability option;
  sampling : sampling_capability option;
  experimental : Yojson.Safe.t option;  (** Experimental capabilities (2025-11-25) *)
}

(** {1 Tools} *)

(** Tool definition *)
type tool = {
  name : string;
  description : string option;
  input_schema : Yojson.Safe.t;
  annotations : tool_annotations option;  (** Behavioral hints (2025-11-25) *)
  icon : icon option;                     (** Tool icon (2025-11-25) *)
}

(** Tool call request *)
type tool_call = {
  name : string;
  arguments : Yojson.Safe.t option;
  _meta : Yojson.Safe.t option;  (** Request metadata (2025-11-25) *)
}

(** Tool call result *)
type tool_result = {
  content : content list;
  is_error : bool option;
  _meta : Yojson.Safe.t option;  (** Result metadata (2025-11-25) *)
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
  icon : icon option;  (** Resource icon (2025-11-25) *)
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
  icon : icon option;  (** Prompt icon (2025-11-25) *)
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
  _meta : Yojson.Safe.t option;  (** Request metadata (2025-11-25) *)
}

(** Initialize result *)
type initialize_result = {
  protocol_version : string;
  capabilities : server_capabilities;
  server_info : implementation_info;
  _meta : Yojson.Safe.t option;  (** Result metadata (2025-11-25) *)
}

(** {1 JSON Encoding} *)

let implementation_info_to_json (info : implementation_info) =
  let base = [
    "name", `String info.name;
    "version", `String info.version;
  ] in
  let fields = match info.description with
    | Some d -> ("description", `String d) :: base
    | None -> base
  in
  `Assoc fields

let icon_to_json (i : icon) =
  `Assoc ["uri", `String i.uri]

let tool_annotations_to_json (a : tool_annotations) =
  let fields = [] in
  let fields = match a.title with
    | Some t -> ("title", `String t) :: fields
    | None -> fields
  in
  let fields = match a.read_only_hint with
    | Some b -> ("readOnlyHint", `Bool b) :: fields
    | None -> fields
  in
  let fields = match a.destructive_hint with
    | Some b -> ("destructiveHint", `Bool b) :: fields
    | None -> fields
  in
  let fields = match a.idempotent_hint with
    | Some b -> ("idempotentHint", `Bool b) :: fields
    | None -> fields
  in
  let fields = match a.open_world_hint with
    | Some b -> ("openWorldHint", `Bool b) :: fields
    | None -> fields
  in
  `Assoc fields

let tools_capability_to_json (tc : tools_capability) =
  let fields = match tc.list_changed with
    | Some b -> [("listChanged", `Bool b)]
    | None -> []
  in
  `Assoc fields

let resources_capability_to_json (rc : resources_capability) =
  let fields = [] in
  let fields = match rc.subscribe with
    | Some b -> ("subscribe", `Bool b) :: fields
    | None -> fields
  in
  let fields = match rc.list_changed with
    | Some b -> ("listChanged", `Bool b) :: fields
    | None -> fields
  in
  `Assoc fields

let prompts_capability_to_json (pc : prompts_capability) =
  let fields = match pc.list_changed with
    | Some b -> [("listChanged", `Bool b)]
    | None -> []
  in
  `Assoc fields

let server_capabilities_to_json (caps : server_capabilities) =
  let fields = [] in
  let fields = match caps.tools with
    | Some tc -> ("tools", tools_capability_to_json tc) :: fields
    | None -> fields
  in
  let fields = match caps.resources with
    | Some rc -> ("resources", resources_capability_to_json rc) :: fields
    | None -> fields
  in
  let fields = match caps.prompts with
    | Some pc -> ("prompts", prompts_capability_to_json pc) :: fields
    | None -> fields
  in
  let fields = match caps.logging with
    | Some true -> ("logging", `Assoc []) :: fields
    | _ -> fields
  in
  `Assoc fields

let roots_capability_to_json (rc : roots_capability) =
  let fields = match rc.list_changed with
    | Some b -> [("listChanged", `Bool b)]
    | None -> []
  in
  `Assoc fields

let client_capabilities_to_json (caps : client_capabilities) =
  let fields = [] in
  let fields = match caps.roots with
    | Some rc -> ("roots", roots_capability_to_json rc) :: fields
    | None -> fields
  in
  let fields = match caps.sampling with
    | Some () -> ("sampling", `Assoc []) :: fields
    | None -> fields
  in
  let fields = match caps.experimental with
    | Some exp -> ("experimental", exp) :: fields
    | None -> fields
  in
  `Assoc fields

let tool_to_json (t : tool) =
  let base = [
    "name", `String t.name;
    "inputSchema", t.input_schema;
  ] in
  let fields = match t.description with
    | Some d -> ("description", `String d) :: base
    | None -> base
  in
  let fields = match t.annotations with
    | Some a -> ("annotations", tool_annotations_to_json a) :: fields
    | None -> fields
  in
  let fields = match t.icon with
    | Some i -> ("icon", icon_to_json i) :: fields
    | None -> fields
  in
  `Assoc fields

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
  let fields = match result.is_error with
    | Some true -> ("isError", `Bool true) :: base
    | _ -> base
  in
  let fields = match result._meta with
    | Some m -> ("_meta", m) :: fields
    | None -> fields
  in
  `Assoc fields

let resource_to_json (res : resource) =
  let base = [
    "uri", `String res.uri;
    "name", `String res.name;
  ] in
  let fields = match res.description with
    | Some d -> ("description", `String d) :: base
    | None -> base
  in
  let fields = match res.mime_type with
    | Some m -> ("mimeType", `String m) :: fields
    | None -> fields
  in
  let fields = match res.icon with
    | Some i -> ("icon", icon_to_json i) :: fields
    | None -> fields
  in
  `Assoc fields

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
  let fields = match p.description with
    | Some d -> ("description", `String d) :: base
    | None -> base
  in
  let fields = match p.arguments with
    | Some args -> ("arguments", `List (List.map prompt_argument_to_json args)) :: fields
    | None -> fields
  in
  let fields = match p.icon with
    | Some i -> ("icon", icon_to_json i) :: fields
    | None -> fields
  in
  `Assoc fields

let initialize_result_to_json (result : initialize_result) =
  let base = [
    "protocolVersion", `String result.protocol_version;
    "capabilities", server_capabilities_to_json result.capabilities;
    "serverInfo", implementation_info_to_json result.server_info;
  ] in
  let fields = match result._meta with
    | Some m -> ("_meta", m) :: base
    | None -> base
  in
  `Assoc fields

(** {1 JSON Decoding} *)

let implementation_info_of_json json =
  let open Yojson.Safe.Util in
  {
    name = json |> member "name" |> to_string;
    version = json |> member "version" |> to_string;
    description = (match json |> member "description" with
      | `String s -> Some s
      | _ -> None);
  }

let icon_of_json json =
  let open Yojson.Safe.Util in
  { uri = json |> member "uri" |> to_string }

let tool_annotations_of_json json =
  let open Yojson.Safe.Util in
  {
    title = (match json |> member "title" with `String s -> Some s | _ -> None);
    read_only_hint = (match json |> member "readOnlyHint" with `Bool b -> Some b | _ -> None);
    destructive_hint = (match json |> member "destructiveHint" with `Bool b -> Some b | _ -> None);
    idempotent_hint = (match json |> member "idempotentHint" with `Bool b -> Some b | _ -> None);
    open_world_hint = (match json |> member "openWorldHint" with `Bool b -> Some b | _ -> None);
  }

let roots_capability_of_json json =
  let open Yojson.Safe.Util in
  {
    list_changed = (match json |> member "listChanged" with `Bool b -> Some b | _ -> None);
  }

let client_capabilities_of_json json =
  let open Yojson.Safe.Util in
  {
    roots = (match json |> member "roots" with `Null -> None | j -> Some (roots_capability_of_json j));
    sampling = (match json |> member "sampling" with `Null -> None | _ -> Some ());
    experimental = (match json |> member "experimental" with `Null -> None | j -> Some j);
  }

let initialize_params_of_json json =
  let open Yojson.Safe.Util in
  {
    protocol_version = json |> member "protocolVersion" |> to_string;
    capabilities = json |> member "capabilities" |> client_capabilities_of_json;
    client_info = json |> member "clientInfo" |> implementation_info_of_json;
    _meta = (match json |> member "_meta" with `Null -> None | m -> Some m);
  }

let tool_call_of_json json =
  let open Yojson.Safe.Util in
  {
    name = json |> member "name" |> to_string;
    arguments = (match json |> member "arguments" with
      | `Null -> None
      | args -> Some args);
    _meta = (match json |> member "_meta" with `Null -> None | m -> Some m);
  }

let tool_of_json json =
  let open Yojson.Safe.Util in
  {
    name = json |> member "name" |> to_string;
    description = (match json |> member "description" with `String s -> Some s | _ -> None);
    input_schema = json |> member "inputSchema";
    annotations = (match json |> member "annotations" with `Null -> None | j -> Some (tool_annotations_of_json j));
    icon = (match json |> member "icon" with `Null -> None | j -> Some (icon_of_json j));
  }

(** {1 Protocol Version} *)

let protocol_version = "2025-11-25"

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

  (* Tasks — 2025-11-25 *)
  let tasks_get = "tasks/get"
  let tasks_result = "tasks/result"
  let tasks_list = "tasks/list"
  let tasks_cancel = "tasks/cancel"

  (* Notifications *)
  let cancelled = "notifications/cancelled"
  let progress = "notifications/progress"
  let resources_updated = "notifications/resources/updated"
  let resources_list_changed = "notifications/resources/list_changed"
  let tools_list_changed = "notifications/tools/list_changed"
  let prompts_list_changed = "notifications/prompts/list_changed"
  let tasks_status = "notifications/tasks/status"  (** Task status change (2025-11-25) *)
end
