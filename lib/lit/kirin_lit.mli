(** @since 1.0.0
    @status experimental
    This module's API may change in future versions. *)

module Element = Element
module Template = Template
module Shadow_dom = Shadow_dom
module Ssr = Ssr
val element :
  tag_name:string ->
  class_name:string ->
  ?props:Element.property list -> ?styles:string list -> string -> Element.t
val prop :
  ?attribute:bool ->
  ?reflect:bool -> ?default:string option -> string -> Element.property
val number :
  ?attribute:bool ->
  ?reflect:bool -> ?default:float option -> string -> Element.property
val boolean :
  ?attribute:bool ->
  ?reflect:bool -> ?default:bool option -> string -> Element.property
val object_ : ?default:Yojson.Safe.t option -> string -> Element.property
val array : ?default:Yojson.Safe.t option -> string -> Element.property
val state : Element.property_type -> string -> Element.property
val shadow :
  ?mode:Shadow_dom.shadow_mode ->
  ?styles:string list -> string -> Shadow_dom.t
val declarative_shadow : Shadow_dom.t -> string
val render_component :
  tag_name:string ->
  attributes:(string * string) list ->
  ?light_dom:string -> Shadow_dom.t -> string
val with_polyfill : string -> string
val template : Template.expression list -> Template.t
val text : string -> Template.t
val property : string -> Template.t
val computed : string -> Template.t
val event : ?options:Template.event_options -> string -> string -> Template.t
val slot : ?name:string -> ?fallback:string -> unit -> Template.t
val when_ :
  string -> then_:Template.t -> ?else_:Template.t -> unit -> Template.t
val repeat :
  items:string -> item:string -> ?index:string -> Template.t -> Template.t
val to_html : Template.t -> string
val render_static :
  data:(string * Yojson.Safe.t) list -> Template.t -> string
val class_map : (string * string) list -> Template.t
val style_map : (string * string) list -> Template.t
val if_defined : string -> Template.t
val live : string -> Template.t
val ref_ : string -> Template.t
val cache : Template.t -> Template.t
val create_ssr : ?config:Ssr.ssr_config -> unit -> Ssr.t
val ssr :
  Ssr.t ->
  tag_name:string ->
  ?props:[> `Assoc of (string * Yojson.Safe.t) list | `Null ] ->
  ?slots:(string option * string) list -> unit -> (string, 'a) result
val ssr_with_fallback : Ssr.t -> tag_name:string -> fallback:string -> string
val ssr_page :
  Ssr.t ->
  components:(string * [> `Assoc of (string * Yojson.Safe.t) list | `Null ] *
              (string option * string) list)
             list ->
  string
val handler : Ssr.t -> tag_name:string -> 'a -> Kirin.Response.t
val handler_with_props :
  get_props:('a -> [> `Assoc of (string * Yojson.Safe.t) list | `Null ]) ->
  Ssr.t -> tag_name:string -> 'a -> Kirin.Response.t
val to_typescript : Element.t -> string
val to_javascript : Element.t -> string
val element_to_json :
  Element.t ->
  [> `Assoc of
       (string *
        [> `List of
             [> `Assoc of (string * Yojson.Safe.t) list | `String of string ]
             list
         | `String of string ])
       list ]
val template_to_json :
  Template.t ->
  ([> `Assoc of
        (string *
         [> `Bool of bool | `List of [> `Assoc of (string * 'a) list ] list ])
        list
    | `List of [> `String of string ] list
    | `Null
    | `String of string ]
   as 'a)
val shadow_to_json :
  Shadow_dom.t ->
  [> `Assoc of
       (string *
        [> `Bool of bool
         | `List of
             [> `Assoc of (string * [> `Null | `String of string ]) list
              | `String of string ]
             list
         | `String of string ])
       list ]
val config_to_json :
  Ssr.ssr_config ->
  [> `Assoc of
       (string *
        [> `Bool of bool | `Float of float | `Int of int | `String of string
        ])
       list ]
val stats_to_json :
  Ssr.ssr_stats ->
  [> `Assoc of (string * [> `Float of float | `Int of int ]) list ]
