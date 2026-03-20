val generate_params_type : Manifest.route_entry -> string
val generate_route_type : Manifest.route_entry -> string
val generate_route_types : Manifest.t -> string
val generate_route_id_type : Manifest.t -> string
val generate_route_tree_type : Manifest.t -> string
val generate_types_file : Manifest.t -> string
val generate_route_definition :
  Manifest.route_entry -> string
val generate_root_route : unit -> string
val generate_router_creation : Manifest.t -> string
val generate_use_params_hook : unit -> string
val generate_use_loader_data_hook : unit -> string
val generate_router_file : Manifest.t -> string
val generate_search_params_type : string -> (string * string) list -> string
val generate_link_props_type : Manifest.t -> string
