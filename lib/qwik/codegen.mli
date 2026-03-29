val generate_param_type : Route_def.t -> string
val generate_route_type : Route_def.t -> string
val generate_loader_type : name:string -> return_type:string -> string

val generate_action_type
  :  name:string
  -> input_type:string
  -> return_type:string
  -> string

val generate_component : name:string -> props:(string * string) list -> string
val generate_routes_file : Route_def.t list -> string
val generate_server_entry : unit -> string
val generate_client_entry : unit -> string
val generate_types_file : routes:Route_def.t list -> string
val generate_qrl : chunk:string -> symbol:string -> capture:string list -> string
val escape_ts_string : string -> string
