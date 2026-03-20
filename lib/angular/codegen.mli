val generate_param_type : Route_def.t -> string
val generate_route_type : Route_def.t -> string
val generate_server_routes : Route_def.t list -> string
val generate_component_type :
  name:string ->
  inputs:(string * string * bool) list ->
  outputs:(string * string) list -> string
val generate_guard_type : name:string -> string
val generate_resolver_type : name:string -> return_type:string -> string
val generate_types_file : routes:Route_def.t list -> string
val escape_ts_string : string -> string
val generate_type_guard :
  name:string -> fields:(string * string) list -> string
