val generate_param_type : (string * string) list -> string
val generate_route_type : Manifest.route_entry -> string
val generate_route_types : Manifest.t -> string
val generate_load_type : Manifest.route_entry -> string
val generate_load_types : Manifest.t -> string
val generate_action_type : Manifest.route_entry -> string
val generate_action_types : Manifest.t -> string
val generate_page_data_type : Manifest.route_entry -> string
val generate_router_file : Manifest.t -> string
val generate_types_file : Manifest.t -> string
val write_to_file : path:string -> string -> unit
val generate : manifest:Manifest.t -> output_dir:string -> unit -> unit
val generate_hooks_server : string
val generate_hooks_client : string
val generate_layout_type : Manifest.layout_entry -> string
val generate_layout_types : Manifest.t -> string
