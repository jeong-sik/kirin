type ssr_config = {
  cdn_url : string;
  version : string;
  include_cloak_style : bool;
  defer_init : bool;
  plugins : string list;
}
val default_config : ssr_config
val script_tag : ssr_config -> string
val plugin_scripts : ssr_config -> string
val cloak_style : string
val head_scripts : ssr_config -> string
type ssr_state = {
  components : Component.t list;
  stores : Store.t list;
  initial_data : (string * Yojson.Safe.t) list;
}
val empty_state : ssr_state
val with_component : Component.t -> ssr_state -> ssr_state
val with_store : Store.t -> ssr_state -> ssr_state
val with_data : string -> Yojson.Safe.t -> ssr_state -> ssr_state
val init_script : ssr_state -> string
val render_element :
  tag:string ->
  directives:Directive.t list ->
  ?attrs:(string * string) list -> ?children:string -> unit -> string
val render_cloaked :
  tag:string ->
  directives:Directive.t list ->
  ?attrs:(string * string) list -> ?children:string -> unit -> string
val eval_expression : data:('a * Yojson.Safe.t) list -> 'a -> string
val render_text_static : data:('a * Yojson.Safe.t) list -> 'a -> string
val render_for_static :
  data:('a * [> `List of ([> `Assoc of ('c * 'b) list ] as 'b) list ]) list ->
  item:'c -> items:'a -> template:(('c * 'b) list -> string) -> string
val render_page :
  config:ssr_config -> state:ssr_state -> body:string -> string
val response :
  config:ssr_config -> state:ssr_state -> body:string -> Kirin.Response.t
val handler :
  config:ssr_config ->
  get_state:('a -> ssr_state) ->
  render_body:('a -> ssr_state -> string) -> 'a -> Kirin.Response.t
val config_to_json :
  ssr_config ->
  [> `Assoc of
       (string *
        [> `Bool of bool
         | `List of [> `String of string ] list
         | `String of string ])
       list ]
val state_to_json :
  ssr_state ->
  [> `Assoc of
       (string *
        [> `Assoc of (string * Yojson.Safe.t) list
         | `List of
             [> `Assoc of
                  (string *
                   [> `List of
                        [> `Assoc of (string * Yojson.Safe.t) list ] list
                    | `Null
                    | `String of string ])
                  list ]
             list ])
       list ]
