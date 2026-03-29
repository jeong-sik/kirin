type preload_strategy =
  | Hover
  | Tap
  | Viewport
  | Eager
  | Off

type preload_type =
  | Code
  | Data
  | CodeAndData

type nav_behavior =
  | Navigate
  | NoScroll
  | KeepFocus
  | ReplaceState

type preload_options =
  { strategy : preload_strategy
  ; preload_type : preload_type
  ; behaviors : nav_behavior list
  ; reload : bool
  }

val default_options : preload_options
val no_preload : preload_options
val eager_preload : preload_options
val preload_data_attr : preload_strategy -> string
val behavior_attrs : nav_behavior list -> string list
val link_attrs : preload_options -> string
val link : href:string -> ?options:preload_options -> string -> string
val link_tap : href:string -> string -> string
val link_no_preload : href:string -> string -> string
val link_external : href:string -> string -> string
val modulepreload_hint : string -> string
val preload_css_hint : string -> string
val prefetch_hint : string -> string
val prerender_hint : string -> string

type route_hints =
  { js_modules : string list
  ; css_files : string list
  ; prefetch : string list
  }

val route_hints
  :  js:string list
  -> css:string list
  -> ?prefetch:string list
  -> unit
  -> route_hints

val render_hints : route_hints -> string
val viewport_observer_script : string
val strategy_to_json : preload_strategy -> [> `String of string ]

val options_to_json
  :  preload_options
  -> [> `Assoc of
          (string
          * [> `Bool of bool | `List of [> `String of string ] list | `String of string ])
            list
     ]
