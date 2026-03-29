val generate_param_type : Route_def.t -> string
val generate_route_type : Route_def.t -> string
val generate_use_fetch_types : unit -> string
val generate_use_async_data_types : unit -> string
val generate_routes_file : Route_def.t list -> string
val generate_api_types : Action.api_route list -> string
val generate_props_type : name:string -> props:(string * string * bool) list -> string

val generate_page_type
  :  name:string
  -> props:(string * string * bool) list
  -> meta:(string * string) list
  -> string

val generate_nuxt_config_types : unit -> string

val generate_types_file
  :  routes:Route_def.t list
  -> api_routes:Action.api_route list
  -> string

val generate_server_handler_type : unit -> string
val escape_ts_string : string -> string
val generate_type_guard : name:string -> fields:(string * string) list -> string
