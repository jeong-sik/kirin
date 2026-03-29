type config =
  { name : string
  ; renderer : string
  ; client_script : string
  ; server_script : string
  ; deps : string list
  ; dev_deps : string list
  }

type t =
  { config : config
  ; enabled : bool
  ; include_patterns : string list
  ; exclude_patterns : string list
  }

val react : t
val vue : t
val svelte : t
val solid : t
val preact : t
val lit : t
val alpine : t
val enable : t -> t
val disable : t -> t
val include_pattern : string -> t -> t
val exclude_pattern : string -> t -> t
val detect_framework : string -> string option
val matches_integration : t -> string -> bool
val get_client_script : t -> string
val get_server_script : t -> string
val generate_config : t list -> string
val generate_deps : t list -> string list * string list
