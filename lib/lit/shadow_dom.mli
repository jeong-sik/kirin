type shadow_mode = Open | Closed
type slot = { slot_name : string option; slot_fallback : string option; }
type t = {
  mode : shadow_mode;
  delegates_focus : bool;
  slot_assignment : [ `Manual | `Named ];
  styles : string list;
  content : string;
  slots : slot list;
}
val create :
  ?mode:shadow_mode ->
  ?delegates_focus:bool ->
  ?slot_assignment:[ `Manual | `Named ] ->
  ?styles:string list -> ?slots:slot list -> string -> t
val with_default_slot : ?fallback:string -> string -> t
val with_named_slots : (string * string option) list -> string -> t
val default_slot : ?fallback:string -> unit -> slot
val named_slot : string -> ?fallback:string -> unit -> slot
val render_slot : slot -> string
val to_declarative_shadow_dom : t -> string
val render_component :
  tag_name:string ->
  attributes:(string * string) list -> ?light_dom:string -> t -> string
type css_module = { css_id : string; css_content : string; }
val css_module : id:string -> string -> css_module
val adopted_stylesheet_script : css_module list -> string
type part = { part_name : string; part_element : string; }
val part : name:string -> string -> part
val render_with_part : part -> string
val part_selector : host_selector:string -> part_name:string -> string
val exportparts : (string * string option) list -> string
val exportparts_attr : (string * string option) list -> string
val polyfill_check : string
val with_polyfill : string -> string
val shadow_mode_to_json : shadow_mode -> [> `String of string ]
val slot_to_json :
  slot -> [> `Assoc of (string * [> `Null | `String of string ]) list ]
val to_json :
  t ->
  [> `Assoc of
       (string *
        [> `Bool of bool
         | `List of
             [> `Assoc of (string * [> `Null | `String of string ]) list
              | `String of string ]
             list
         | `String of string ])
       list ]
