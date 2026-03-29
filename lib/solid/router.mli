type route =
  { path : string
  ; component : string
  ; children : route list
  ; data : string option
  ; preload : bool
  }

type match_result =
  { route : route
  ; params : (string * string) list
  ; search : (string * string) list
  }

val route
  :  path:string
  -> component:string
  -> ?data:string
  -> ?preload:bool
  -> ?children:route list
  -> unit
  -> route

val index : component:string -> ?data:string -> unit -> route
val catch_all : component:string -> ?data:string -> unit -> route
val layout : component:string -> ?children:route list -> unit -> route
val parse_path : string -> string list
val is_dynamic_segment : string -> bool
val param_name : string -> string
val match_segment : string -> string -> (string * string) list option
val match_path : string -> string -> (string * string) list option
val find_route : string -> route list -> match_result option
val route_to_config : route -> string
val routes_to_config : route list -> string
val discover_routes : base_path:string -> string -> route list
val preload_link : route -> string
val preload_links : route -> string list

type 'a loader_result =
  | Data of 'a
  | Redirect of string * int
  | NotFound
  | ServerError of string

val serialize_loader_result
  :  ('a -> ([> `Int of int | `String of string ] as 'b))
  -> 'a loader_result
  -> [> `Assoc of (string * 'b) list ]
