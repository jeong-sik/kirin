val script : string list -> string
val on : string -> string -> string
val on_click : string -> string
val on_submit : string -> string
val on_load : string -> string
val on_keyup : string -> string
val on_change : string -> string
val on_focus : string -> string
val on_blur : string -> string
val on_mouseenter : string -> string
val on_mouseleave : string -> string
val on_intersect : string -> string
val add_class : string -> string
val remove_class : string -> string
val toggle_class : string -> string
val add_class_to : string -> string -> string
val remove_class_from : string -> string -> string
val toggle_class_on : string -> string -> string
val set_attr : string -> string -> string
val remove_attr : string -> string
val set_prop : string -> string -> string
val set_html : string -> string
val set_text : string -> string
val transition : string -> string -> string -> string
val fade_out : ?duration:string -> unit -> string
val fade_in : ?duration:string -> unit -> string
val wait : string -> string
val wait_then : string -> string -> string
val if_ : string -> string -> string
val if_else : string -> string -> string -> string
val repeat : int -> string -> string
val repeat_until : string -> string -> string
val remove : string -> string
val remove_me : string
val append : string -> string -> string
val prepend : string -> string -> string
val put : string -> string -> string -> string
val trigger : string -> string
val trigger_on : string -> string -> string
val send : string -> string -> string
val htmx_trigger : string -> string
val settle : string
val me : string
val closest : string -> string
val next : string -> string
val previous : string -> string
val find : string -> string
val toggle_active_on_click : string
val remove_with_fade : ?duration:string -> unit -> string
val loading_indicator : string -> string
val dismissable : ?duration:string -> unit -> string
val autofocus_on_load : string
val copy_to_clipboard : string -> string
