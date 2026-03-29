val data : (string * string) list -> string
val data_empty : string
val show : 'a -> string * 'a
val if_ : 'a -> string * 'a
val for_ : 'a -> string * 'a
val text : 'a -> string * 'a
val html : 'a -> string * 'a
val on : string -> 'a -> string * 'a
val on_click : 'a -> string * 'a
val on_submit : 'a -> string * 'a
val on_keyup : 'a -> string * 'a
val on_change : 'a -> string * 'a
val on_input : 'a -> string * 'a
val model : 'a -> string * 'a
val model_debounce : 'a -> int -> string * 'a
val model_lazy : 'a -> string * 'a
val model_number : 'a -> string * 'a
val bind : string -> 'a -> string * 'a
val bind_class : 'a -> string * 'a
val bind_style : 'a -> string * 'a
val bind_disabled : 'a -> string * 'a
val init : 'a -> string * 'a
val x_effect : 'a -> string * 'a
val ref_ : 'a -> string * 'a
val refs : string -> string
val transition : string * string
val transition_named : enter:'a -> leave:'a -> (string * 'a) list

val transition_full
  :  enter:'a
  -> enter_start:'a
  -> enter_end:'a
  -> leave:'a
  -> leave_start:'a
  -> leave_end:'a
  -> (string * 'a) list

val transition_fade : (string * string) list
val transition_scale : (string * string) list
val cloak : string * string
val ignore : string * string
val teleport : 'a -> string * 'a
val el : string
val all_refs : string
val store : string -> string
val watch : string -> string -> string
val next_tick : string -> string
val dispatch : string -> string
val dispatch_detail : string -> string -> string
val define_store : string -> string -> string
val attrs_to_string : (string * string) list -> string
val component : ?id:string -> ?class_:string -> string -> string -> string
