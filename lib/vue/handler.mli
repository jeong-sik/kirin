type mode =
  | SPA
  | Hydration
  | SSR
  | Streaming

type config =
  { mode : mode
  ; manifest_path : string
  ; entry_point : string
  ; ssr_config : Ssr.config option
  ; streaming_config : Streaming.config option
  ; dev_mode : bool
  ; dev_port : int
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
val redirect_response : location:string -> ?status:int -> unit -> response
val error_response : ?status:int -> string -> response
val handle_spa : config:config -> title:string -> unit -> response

val handle_hydration
  :  title:string
  -> payload:Data.payload
  -> entry_script:string
  -> unit
  -> response

val handle_ssr : engine:Ssr.t -> url:string -> unit -> response

val handle_ssr_with_fallback
  :  engine:Ssr.t
  -> url:string
  -> fallback:string
  -> unit
  -> response

type streaming_response =
  { headers : (string * string) list
  ; write_chunk : string -> unit
  ; finish : unit -> unit
  }

val handle_streaming
  :  ctx:Streaming.context
  -> shell:string
  -> chunks:Streaming.chunk list
  -> unit
  -> response

val handle_dev_proxy : port:int -> path:string -> unit -> response
val handle_api : handler:('a -> Action.action_result) -> request:'a -> unit -> response
val handle : config:config -> request:request_info -> unit -> response

type route_match =
  { params : (string * string) list
  ; route_name : string option
  ; handler_fn : request_info -> response
  }

val catch_all_handler : config -> request_info -> response

type middleware = request_info -> response option

val auth_middleware
  :  check_auth:((string * string) list -> bool)
  -> streaming_response
  -> response option

val logging_middleware : log:(string -> unit) -> request_info -> 'a option
val with_middleware : ('a -> 'b option) list -> ('a -> 'b) -> 'a -> 'b
val mode_to_string : mode -> string

val config_to_json
  :  config
  -> [> `Assoc of (string * [> `Bool of bool | `Int of int | `String of string ]) list ]

val response_to_json
  :  response
  -> [> `Assoc of
          (string * [> `Assoc of (string * [> `String of string ]) list | `Int of int ])
            list
     ]
