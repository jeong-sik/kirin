type route_entry =
  { id : string
  ; pattern : string
  ; params : (string * string) list
  ; page : bool
  ; layout : string option
  ; error : bool
  ; endpoint : bool
  }

type layout_entry =
  { layout_id : string
  ; parent : string option
  ; reset : bool
  }

type t =
  { routes : route_entry list
  ; layouts : layout_entry list
  ; matchers : string list
  ; hooks : string list
  }

val empty : t
val add_route : route_entry -> t -> t
val add_layout : layout_entry -> t -> t
val add_matcher : string -> t -> t
val add_hook : string -> t -> t

val route
  :  id:string
  -> pattern:string
  -> ?params:(string * string) list
  -> ?page:bool
  -> ?layout:string
  -> ?error:bool
  -> ?endpoint:bool
  -> unit
  -> route_entry

val layout : id:string -> ?parent:string -> ?reset:bool -> unit -> layout_entry
val from_discovered_routes : File_router.discovered_route list -> t

val route_to_json
  :  route_entry
  -> [> `Assoc of
          (string
          * [> `Bool of bool
            | `List of [> `Assoc of (string * [> `String of string ]) list ] list
            | `String of string
            ])
            list
     ]

val layout_to_json
  :  layout_entry
  -> [> `Assoc of (string * [> `Bool of bool | `String of string ]) list ]

val to_json
  :  t
  -> [> `Assoc of
          (string
          * [> `List of
                 [> `Assoc of
                      (string
                      * [> `Bool of bool
                        | `List of
                            [> `Assoc of (string * [> `String of string ]) list ] list
                        | `String of string
                        ])
                        list
                 | `String of string
                 ]
                   list
            ])
            list
     ]

val to_json_string : ?pretty:bool -> t -> string
val of_json : Yojson__Safe.t -> t
val load_file : string -> t
