type animation =
  | Fade
  | Slide
  | None_
  | Initial
  | Custom of string

type direction =
  | Forward
  | Backward

type element =
  { name : string
  ; animation : animation
  ; persist : bool
  }

type config =
  { enabled : bool
  ; fallback : bool
  ; animation : animation
  ; elements : element list
  }

val default_config : config
val enable : config -> config
val disable : config -> config
val with_fallback : config -> config
val with_animation : animation -> config -> config
val element : name:string -> ?animation:animation -> ?persist:bool -> unit -> element
val add_element : element -> config -> config
val persist : string -> element
val animation_to_string : animation -> string
val animation_of_string : string -> animation
val transition_name : string -> string
val transition_animate : animation -> string
val transition_persist : string
val transition_attrs : element -> string
val generate_script : config -> string
val generate_css : config -> string
val component_import : string
val component_usage : string
val head_content : config -> string
