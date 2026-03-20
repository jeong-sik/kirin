type strategy = Intent | Viewport | Render | None
val strategy_to_string : strategy -> string
type state = Idle | Loading | Loaded | Error of string
val link_hint : href:string -> strategy:strategy -> string
val module_preload : src:string -> string
val preconnect : origin:string -> string
val dns_prefetch : hostname:string -> string
val preload_script : unit -> string
val link_attrs : strategy:strategy -> prefetch_intent:bool -> string
type priority = High | Medium | Low | Auto
val priority_to_string : priority -> string
val fetch_priority : priority -> string
val route_hints :
  loader_url:string ->
  js_modules:string list ->
  css_files:string list -> priority:priority -> string
val is_preload_request : (string * string) list -> bool
val preload_response : Yojson.Safe.t -> string
