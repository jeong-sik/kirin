type file_type =
  | Component
  | Module
  | Routes
  | ServerRoutes
  | Guard
  | Resolver
  | Service
  | Interceptor

type segment_type =
  | Static of string
  | Dynamic of string
  | Wildcard
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
  ; component_name : string option
  ; is_lazy : bool
  ; render_mode : Route_def.render_mode option
  }

val detect_file_type : string -> file_type option
val component_name_from_file : string -> string option
val dir_to_route_path : string -> string
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

val segment_to_json
  :  segment
  -> [> `Assoc of (string * [> `Null | `String of string ]) list ]

val discovered_to_json
  :  discovered_route
  -> [> `Assoc of (string * [> `Bool of bool | `Null | `String of string ]) list ]
