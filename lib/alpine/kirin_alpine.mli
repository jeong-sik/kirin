(** @since 1.0.0
    @status experimental
    This module's API may change in future versions. *)

module Directive = Directive
module Component = Component
module Store = Store
module Ssr = Ssr
val data : string -> Directive.t
val init : string -> Directive.t
val show : string -> Directive.t
val if_ : string -> Directive.t
val bind : attr:string -> string -> Directive.t
val on :
  ?modifiers:Directive.modifier list -> string -> string -> Directive.t
val text : string -> Directive.t
val html : string -> Directive.t
val model : string -> Directive.t
val for_ :
  item:string -> ?index:string -> items:string -> unit -> Directive.t
val ref_ : string -> Directive.t
val cloak : Directive.t
val prevent : Directive.modifier
val stop : Directive.modifier
val outside : Directive.modifier
val window : Directive.modifier
val document : Directive.modifier
val once : Directive.modifier
val debounce : int -> Directive.modifier
val throttle : int -> Directive.modifier
val self : Directive.modifier
val passive : Directive.modifier
val capture : Directive.modifier
val component : string -> Component.t
val prop :
  ?default:Yojson.Safe.t ->
  [ `Array | `Boolean | `Number | `Object | `String ] ->
  string -> Component.t -> Component.t
val string_prop : ?default:string -> string -> Component.property
val number_prop : ?default:float -> string -> Component.property
val boolean_prop : ?default:bool -> string -> Component.property
val array_prop : ?default:Yojson.Safe.t list -> string -> Component.property
val object_prop :
  ?default:(string * Yojson.Safe.t) list -> string -> Component.property
val method_ : string -> string list -> string -> Component.t -> Component.t
val store : string -> Store.t
val store_from_object : string -> (string * Yojson.Safe.t) list -> Store.t
val store_accessor : Store.t -> string -> string
val default_config : Ssr.ssr_config
val ssr_state : Ssr.ssr_state
val head_scripts : Ssr.ssr_config -> string
val init_script : Ssr.ssr_state -> string
val render_page :
  config:Ssr.ssr_config -> state:Ssr.ssr_state -> body:string -> string
val response :
  config:Ssr.ssr_config ->
  state:Ssr.ssr_state -> body:string -> Kirin.Response.t
val handler :
  config:Ssr.ssr_config ->
  get_state:('a -> Ssr.ssr_state) ->
  render_body:('a -> Ssr.ssr_state -> string) -> 'a -> Kirin.Response.t
val element :
  tag:string ->
  directives:Directive.t list ->
  ?attrs:(string * string) list -> ?children:string -> unit -> string
val cloaked :
  tag:string ->
  directives:Directive.t list ->
  ?attrs:(string * string) list -> ?children:string -> unit -> string
val directive_to_json :
  Directive.t ->
  [> `Assoc of
       (string *
        [> `Int of int
         | `List of [> `String of string ] list
         | `Null
         | `String of string ])
       list ]
val component_to_json :
  Component.t ->
  [> `Assoc of
       (string *
        [> `List of [> `Assoc of (string * Yojson.Safe.t) list ] list
         | `Null
         | `String of string ])
       list ]
val store_to_json :
  Store.t ->
  [> `Assoc of
       (string *
        [> `List of [> `Assoc of (string * Yojson.Safe.t) list ] list
         | `String of string ])
       list ]
val config_to_json :
  Ssr.ssr_config ->
  [> `Assoc of
       (string *
        [> `Bool of bool
         | `List of [> `String of string ] list
         | `String of string ])
       list ]
val state_to_json :
  Ssr.ssr_state ->
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
