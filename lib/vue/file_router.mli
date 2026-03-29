type file_type =
  | Page
  | Layout
  | Middleware
  | ServerRoute
  | Plugin
  | Composable

type segment_type =
  | Static of string
  | Dynamic of string
  | CatchAll of string
  | Optional of string

type segment =
  { raw : string
  ; segment_type : segment_type
  ; param_name : string option
  }

val parse_segment : string -> segment
val parse_path : string -> segment list
val path_to_pattern : string -> string
val extract_params : string -> (string * Route_def.param_type) list

type discovered_route =
  { file_path : string
  ; route_path : string
  ; file_type : file_type
  ; is_index : bool
  ; has_server_handler : bool
  }

val is_page_file : string -> bool
val is_layout_file : string -> bool
val is_middleware_file : string -> bool
val is_server_route_file : string -> bool
val file_to_route_path : string -> string
val discover_route : base_dir:string -> string -> discovered_route

val discover_routes_aux
  :  base_dir:string
  -> prefix:string
  -> string
  -> discovered_route list

val discover_routes : string -> discovered_route list

type route_node =
  { segment : string
  ; route : discovered_route option
  ; children : route_node list
  }

val empty_node : string -> route_node
val insert_route : string list -> discovered_route -> route_node -> route_node
val build_tree : discovered_route list -> route_node

type router_config =
  { strict_trailing_slash : bool
  ; sensitive : bool
  ; hash_mode : bool
  ; scroll_behavior_type : string
  }

val default_config : router_config

val segment_to_json
  :  segment
  -> [> `Assoc of (string * [> `Null | `String of string ]) list ]

val discovered_to_json
  :  discovered_route
  -> [> `Assoc of (string * [> `Bool of bool | `String of string ]) list ]

val config_to_json
  :  router_config
  -> [> `Assoc of (string * [> `Bool of bool | `String of string ]) list ]
