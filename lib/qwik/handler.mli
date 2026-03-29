type mode =
  | SSR
  | Static
  | SPA

type config =
  { mode : mode
  ; dist_path : string
  ; manifest_path : string
  ; ssr_engine : Ssr.t option
  ; routes : Route_def.t list
  ; fallback_to_spa : bool
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
val json_response : ?status:int -> Yojson.Safe.t -> response
val error_response : status:int -> message:string -> response
val is_static_asset : string -> bool
val serve_static : config -> string -> response option
val handle_ssr : config -> request_info -> response
val handle_static : config -> request_info -> response
val handle_spa : config -> response
val handle : config:config -> request:request_info -> unit -> response
val catch_all_handler : config -> request_info -> response
val handle_loader : config:'a -> request:'b -> loader_name:string -> unit -> response
val handle_action : config:'a -> request:'b -> action_name:string -> unit -> response
val handle_q_data : config:'a -> request:request_info -> unit -> response
val mode_to_string : mode -> string

val config_to_json
  :  config
  -> [> `Assoc of (string * [> `Bool of bool | `String of string ]) list ]
