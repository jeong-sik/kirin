type modifier =
    Prevent
  | Stop
  | Outside
  | Window
  | Document
  | Once
  | Debounce of int
  | Throttle of int
  | Self
  | Camel
  | Dot
  | Passive
  | Capture
type event_directive = {
  event_name : string;
  handler : string;
  modifiers : modifier list;
}
type bind_directive = { attribute : string; expression : string; }
type for_directive = {
  item : string;
  index : string option;
  items : string;
}
type t =
    Data of string
  | Init of string
  | Show of string
  | If of string
  | Bind of bind_directive
  | On of event_directive
  | Text of string
  | Html of string
  | Model of string
  | ModelNumber of string
  | ModelDebounce of string * int
  | For of for_directive
  | Transition of transition_config option
  | Effect of string
  | Ref of string
  | Cloak
  | Ignore
  | Id of string
  | Teleport of string
and transition_config = {
  enter : string option;
  enter_start : string option;
  enter_end : string option;
  leave : string option;
  leave_start : string option;
  leave_end : string option;
}
val data : string -> t
val init : string -> t
val show : string -> t
val if_ : string -> t
val bind : attr:string -> string -> t
val on : ?modifiers:modifier list -> string -> string -> t
val text : string -> t
val html : string -> t
val model : string -> t
val model_number : string -> t
val model_debounce : ms:int -> string -> t
val for_ : item:string -> ?index:string -> items:string -> unit -> t
val transition : ?config:transition_config -> unit -> t
val transition_config :
  ?enter:string ->
  ?enter_start:string ->
  ?enter_end:string ->
  ?leave:string ->
  ?leave_start:string -> ?leave_end:string -> unit -> transition_config
val effect_ : string -> t
val ref_ : string -> t
val cloak : t
val ignore : t
val id : string -> t
val teleport : string -> t
val modifier_of_string : string -> modifier option
val modifier_to_string : modifier -> string
val to_attribute : t -> string
val to_shorthand : t -> string
val modifier_to_json : modifier -> [> `String of string ]
val to_json :
  t ->
  [> `Assoc of
       (string *
        [> `Int of int
         | `List of [> `String of string ] list
         | `Null
         | `String of string ])
       list ]
