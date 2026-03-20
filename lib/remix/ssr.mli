type ssr_config = {
  entry_client : string;
  entry_server : string;
  public_path : string;
  enable_streaming : bool;
  defer_timeout : float;
}
val default_config : ssr_config
type render_context = {
  url : string;
  matches : Route.match_result list;
  loader_data : (string * Yojson.Safe.t) list;
  action_data : Yojson.Safe.t option;
  errors : (string * string) list;
}
val create_context :
  url:string ->
  matches:Route.match_result list ->
  loader_data:(string * Yojson.Safe.t) list ->
  ?action_data:Yojson.Safe.t ->
  ?errors:(string * string) list -> unit -> render_context
val load_data :
  Route.match_result list ->
  Loader.loader_context -> (string * Yojson.Safe.t) list
val check_redirects :
  Route.match_result list ->
  Loader.loader_context -> (string * int) option
val hydration_script :
  loader_data:(string * Yojson.Safe.t) list ->
  action_data:Yojson.Safe.t option -> string
val entry_script : ssr_config -> string
val meta_tags :
  loader_data:(string *
               [> `Assoc of
                    (string *
                     [> `List of
                          [> `Assoc of (string * [> `String of string ]) list
                          ] list ])
                    list ])
              list ->
  string
val link_tags :
  loader_data:(string *
               [> `Assoc of
                    (string *
                     [> `List of
                          [> `Assoc of (string * [> `String of string ]) list
                          ] list ])
                    list ])
              list ->
  string
val render_document :
  config:ssr_config -> context:render_context -> body:string -> string
type stream_chunk =
    Shell of string
  | DeferredData of string * Yojson.Safe.t
  | Complete
val render_streaming :
  config:'a ->
  context:'b ->
  render_shell:('b -> string) -> on_chunk:(stream_chunk -> unit) -> unit
val render_error_boundary : error:string -> route:string -> string
val render_catch_boundary : status:int -> message:string -> string
val handler :
  config:ssr_config ->
  routes:Route.t list ->
  render_component:(render_context -> string) ->
  Kirin.Request.t -> Kirin.Response.t
val config_to_json :
  ssr_config ->
  [> `Assoc of
       (string * [> `Bool of bool | `Float of float | `String of string ])
       list ]
val context_to_json :
  render_context -> [> `Assoc of (string * Yojson.Safe.t) list ]
