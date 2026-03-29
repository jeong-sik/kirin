type mode =
  | SSR
  | CSR
  | Hybrid
  | Prerender

type config =
  { mode : mode
  ; dist_path : string
  ; index_html : string
  ; ssr_engine : Ssr.t option
  ; server_routes : Route_def.t list
  ; fallback_to_csr : bool
  ; cache_control : string option
  }

val default_config : config

type request_info =
  { path : string
  ; query : (string * string) list
  ; headers : (string * string) list
  ; method_ : string
  ; body : string option
  }

val request_info
  :  path:string
  -> ?query:(string * string) list
  -> ?headers:(string * string) list
  -> ?method_:string
  -> ?body:string
  -> unit
  -> request_info

type response =
  { status : int
  ; headers : (string * string) list
  ; body : string
  }

val html_response : ?status:int -> ?headers:(string * string) list -> string -> response
val redirect_response : ?status:int -> string -> response
val error_response : status:int -> message:string -> response
val route_matches : Route_def.t -> string -> bool
val find_route : Route_def.t list -> string -> Route_def.t option
val get_render_mode : config -> string -> Route_def.render_mode
val handle_ssr : config -> request_info -> response
val handle_csr : config -> response
val handle_prerender : config -> request_info -> response
val handle : config:config -> request:request_info -> unit -> response
val is_static_asset : string -> bool
val serve_static : config -> string -> response option
val catch_all_handler : config -> request_info -> response
val mode_to_string : mode -> string

val config_to_json
  :  config
  -> [> `Assoc of (string * [> `Bool of bool | `String of string ]) list ]
