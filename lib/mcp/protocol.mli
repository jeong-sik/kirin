type implementation_info = {
  name : string;
  version : string;
  description : string option;
}
type icon = { uri : string; }
type tool_annotations = {
  title : string option;
  read_only_hint : bool option;
  destructive_hint : bool option;
  idempotent_hint : bool option;
  open_world_hint : bool option;
}
type tools_capability = { list_changed : bool option; }
type resources_capability = {
  subscribe : bool option;
  list_changed : bool option;
}
type prompts_capability = { list_changed : bool option; }
type server_capabilities = {
  tools : tools_capability option;
  resources : resources_capability option;
  prompts : prompts_capability option;
  logging : bool option;
}
type roots_capability = { list_changed : bool option; }
type sampling_capability = unit
type client_capabilities = {
  roots : roots_capability option;
  sampling : sampling_capability option;
  experimental : Yojson.Safe.t option;
}
type tool = {
  name : string;
  description : string option;
  input_schema : Yojson.Safe.t;
  annotations : tool_annotations option;
  icon : icon option;
}
type tool_call = {
  name : string;
  arguments : Yojson.Safe.t option;
  _meta : Yojson.Safe.t option;
}
type tool_result = {
  content : content list;
  is_error : bool option;
  _meta : Yojson.Safe.t option;
}
and content =
    Text of string
  | Image of { data : string; mime_type : string; }
  | Resource of { uri : string; text : string option; blob : string option; }
type resource = {
  uri : string;
  name : string;
  description : string option;
  mime_type : string option;
  icon : icon option;
}
type resource_contents = {
  uri : string;
  mime_type : string option;
  text : string option;
  blob : string option;
}
type prompt_argument = {
  name : string;
  description : string option;
  required : bool option;
}
type prompt = {
  name : string;
  description : string option;
  arguments : prompt_argument list option;
  icon : icon option;
}
type prompt_message = { role : [ `assistant | `user ]; content : content; }
type initialize_params = {
  protocol_version : string;
  capabilities : client_capabilities;
  client_info : implementation_info;
  _meta : Yojson.Safe.t option;
}
type initialize_result = {
  protocol_version : string;
  capabilities : server_capabilities;
  server_info : implementation_info;
  _meta : Yojson.Safe.t option;
}
val implementation_info_to_json :
  implementation_info ->
  [> `Assoc of (string * [> `String of string ]) list ]
val icon_to_json :
  icon -> [> `Assoc of (string * [> `String of string ]) list ]
val tool_annotations_to_json :
  tool_annotations ->
  [> `Assoc of (string * [> `Bool of bool | `String of string ]) list ]
val tools_capability_to_json :
  tools_capability -> [> `Assoc of (string * [> `Bool of bool ]) list ]
val resources_capability_to_json :
  resources_capability -> [> `Assoc of (string * [> `Bool of bool ]) list ]
val prompts_capability_to_json :
  prompts_capability -> [> `Assoc of (string * [> `Bool of bool ]) list ]
val server_capabilities_to_json :
  server_capabilities ->
  [> `Assoc of
       (string * [> `Assoc of (string * [> `Bool of bool ]) list ]) list ]
val roots_capability_to_json :
  roots_capability -> [> `Assoc of (string * [> `Bool of bool ]) list ]
val client_capabilities_to_json :
  client_capabilities -> [> `Assoc of (string * Yojson.Safe.t) list ]
val tool_to_json : tool -> [> `Assoc of (string * Yojson.Safe.t) list ]
val content_to_json :
  content ->
  [> `Assoc of
       (string *
        [> `Assoc of (string * [> `String of string ]) list
         | `String of string ])
       list ]
val tool_result_to_json :
  tool_result -> [> `Assoc of (string * Yojson.Safe.t) list ]
val resource_to_json :
  resource ->
  [> `Assoc of
       (string *
        [> `Assoc of (string * [> `String of string ]) list
         | `String of string ])
       list ]
val resource_contents_to_json :
  resource_contents -> [> `Assoc of (string * [> `String of string ]) list ]
val prompt_argument_to_json :
  prompt_argument ->
  [> `Assoc of (string * [> `Bool of bool | `String of string ]) list ]
val prompt_to_json :
  prompt ->
  [> `Assoc of
       (string *
        [> `Assoc of (string * [> `String of string ]) list
         | `List of
             [> `Assoc of
                  (string * [> `Bool of bool | `String of string ]) list ]
             list
         | `String of string ])
       list ]
val prompt_message_to_json :
  prompt_message ->
  [> `Assoc of
       (string *
        [> `Assoc of
             (string *
              [> `Assoc of (string * [> `String of string ]) list
               | `String of string ])
             list
         | `String of string ])
       list ]
val initialize_result_to_json :
  initialize_result -> [> `Assoc of (string * Yojson.Safe.t) list ]
val implementation_info_of_json : Yojson__Safe.t -> implementation_info
val icon_of_json : Yojson__Safe.t -> icon
val tool_annotations_of_json : Yojson__Safe.t -> tool_annotations
val roots_capability_of_json : Yojson__Safe.t -> roots_capability
val client_capabilities_of_json : Yojson__Safe.t -> client_capabilities
val initialize_params_of_json : Yojson__Safe.t -> initialize_params
val tool_call_of_json : Yojson__Safe.t -> tool_call
val tool_of_json : Yojson__Safe.t -> tool
val protocol_version : string
module Method :
  sig
    val initialize : string
    val initialized : string
    val ping : string
    val tools_list : string
    val tools_call : string
    val resources_list : string
    val resources_read : string
    val resources_subscribe : string
    val resources_unsubscribe : string
    val prompts_list : string
    val prompts_get : string
    val logging_set_level : string
    val tasks_get : string
    val tasks_result : string
    val tasks_list : string
    val tasks_cancel : string
    val cancelled : string
    val progress : string
    val resources_updated : string
    val resources_list_changed : string
    val tools_list_changed : string
    val prompts_list_changed : string
    val tasks_status : string
  end
