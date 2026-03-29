type preload_mode =
  | Intent
  | Visible
  | Always
  | Never

type prefetch_mode =
  | OnVisible
  | OnInteract
  | Disabled

type link_options =
  { preload : preload_mode
  ; prefetch : prefetch_mode
  ; no_prefetch : bool
  ; no_rel : bool
  ; external_ : bool
  ; active_class : string
  ; exact_active_class : string
  ; replace : bool
  ; target : string option
  }

val default_options : link_options
val no_preload : link_options

type hint_type =
  | Modulepreload
  | Preload
  | Prefetch
  | Preconnect
  | DnsPrefetch

type hint =
  { hint_type : hint_type
  ; href : string
  ; as_ : string option
  ; crossorigin : bool
  ; media : string option
  }

val modulepreload : string -> hint
val preload_css : string -> hint
val prefetch : string -> hint
val preconnect : string -> hint
val link_attrs : href:string -> ?options:link_options -> unit -> (string * string) list
val link : href:string -> ?options:link_options -> string -> string
val render_hint : hint -> string
val render_hints : hint list -> string
val hints_for_route : Manifest.route_entry -> hint list
val all_hints : Manifest.t -> hint list

type payload_options =
  { enabled : bool
  ; limit : int
  ; delay : int
  ; threshold : float
  }

val default_payload_options : payload_options
val payload_script : route_path:string -> options:payload_options -> string
val preload_mode_to_string : preload_mode -> string
val prefetch_mode_to_string : prefetch_mode -> string

val options_to_json
  :  link_options
  -> [> `Assoc of (string * [> `Bool of bool | `String of string ]) list ]

val hint_to_json
  :  hint
  -> [> `Assoc of (string * [> `Bool of bool | `Null | `String of string ]) list ]
