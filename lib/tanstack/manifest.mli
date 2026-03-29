type route_entry =
  { id : string
  ; path : string
  ; parent_id : string option
  ; index : bool
  ; has_loader : bool
  ; has_action : bool
  ; has_error_boundary : bool
  ; has_loading : bool
  }

type t =
  { version : int
  ; routes : route_entry list
  ; root_id : string option
  }

val empty : t
val add_route : route_entry -> t -> t
val set_root : string -> t -> t
val find_route : string -> t -> route_entry option
val find_route_by_path : string -> t -> route_entry option
val children_of : string -> t -> route_entry list
val root_routes : t -> route_entry list

type match_result =
  { route : route_entry
  ; params : (string * string) list
  ; search_params : (string * string) list
  }

val parse_path : string -> string list
val parse_search : string -> (string * string) list
val path_matches : string -> string -> (string * string) list option
val match_path : string -> t -> match_result option

val route_entry_to_json
  :  route_entry
  -> [> `Assoc of (string * [> `Bool of bool | `Null | `String of string ]) list ]

val to_json
  :  t
  -> [> `Assoc of
          (string
          * [> `Int of int
            | `List of
                [> `Assoc of
                     (string * [> `Bool of bool | `Null | `String of string ]) list
                ]
                  list
            | `Null
            | `String of string
            ])
            list
     ]

val route_entry_of_json
  :  [> `Assoc of (string * [> `Bool of bool | `String of string ]) list ]
  -> route_entry option

val of_json
  :  [> `Assoc of
          (string
          * [> `Int of int
            | `List of
                [> `Assoc of (string * [> `Bool of bool | `String of string ]) list ] list
            | `String of string
            ])
            list
     ]
  -> t

val from_discovered_routes : File_router.discovered_route list -> t
