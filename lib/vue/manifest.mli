type route_entry =
  { id : string
  ; path : string
  ; pattern : string
  ; name : string option
  ; component : string
  ; params : (string * string) list
  ; layout : string option
  ; middleware : string list
  ; redirect : string option
  ; alias : string list
  ; is_page : bool
  ; is_layout : bool
  }

type layout_entry =
  { layout_id : string
  ; name : string
  ; component : string
  ; is_default : bool
  }

type server_route_entry =
  { route_id : string
  ; path : string
  ; method_ : string option
  ; handler : string
  }

type plugin_entry =
  { name : string
  ; src : string
  ; mode : string
  }

type t =
  { routes : route_entry list
  ; layouts : layout_entry list
  ; server_routes : server_route_entry list
  ; plugins : plugin_entry list
  ; middleware : string list
  }

val empty : t
val add_route : route_entry -> t -> t
val add_layout : layout_entry -> t -> t
val add_server_route : server_route_entry -> t -> t
val add_plugin : plugin_entry -> t -> t
val add_middleware : string -> t -> t

val create_route
  :  id:string
  -> path:string
  -> pattern:string
  -> component:string
  -> ?name:string option
  -> ?params:(string * string) list
  -> ?layout:string option
  -> ?middleware:string list
  -> ?redirect:string option
  -> ?alias:string list
  -> ?is_page:bool
  -> ?is_layout:bool
  -> unit
  -> route_entry

val create_layout
  :  name:string
  -> component:string
  -> ?is_default:bool
  -> unit
  -> layout_entry

val create_server_route
  :  path:string
  -> handler:string
  -> ?method_:string
  -> unit
  -> server_route_entry

val find_route : string -> t -> route_entry option
val find_route_by_name : string -> t -> route_entry option
val find_layout : string -> t -> layout_entry option
val default_layout : t -> layout_entry option

val route_to_json
  :  route_entry
  -> [> `Assoc of
          (string
          * [> `Assoc of (string * [> `String of string ]) list
            | `Bool of bool
            | `List of [> `String of string ] list
            | `String of string
            ])
            list
     ]

val layout_to_json
  :  layout_entry
  -> [> `Assoc of (string * [> `Bool of bool | `String of string ]) list ]

val server_route_to_json
  :  server_route_entry
  -> [> `Assoc of (string * [> `String of string ]) list ]

val plugin_to_json : plugin_entry -> [> `Assoc of (string * [> `String of string ]) list ]

val to_json
  :  t
  -> [> `Assoc of
          (string
          * [> `List of
                 [> `Assoc of
                      (string
                      * [> `Assoc of (string * [> `String of string ]) list
                        | `Bool of bool
                        | `List of [> `String of string ] list
                        | `String of string
                        ])
                        list
                 | `String of string
                 ]
                   list
            ])
            list
     ]

val from_json : Yojson__Safe.t -> t
val load_file : string -> t
val from_discovered_routes : File_router.discovered_route list -> t
