type segment =
  | Static of string
  | Dynamic of string
  | CatchAll of string
  | OptionalCatchAll of string

val parse_segment : string -> segment
val segment_to_pattern : segment -> string
val should_include : string -> bool
val remove_extension : string -> string
val file_to_route_path : string -> string
val discover_routes : 'a -> 'b list
val route_priority : Route_def.t -> int
val sort_routes : Route_def.t list -> Route_def.t list
val route_from_file : pages_dir:string -> string -> Route_def.t
val is_api_route : string -> bool

type api_method =
  | GET
  | POST
  | PUT
  | DELETE
  | PATCH
  | ALL

val parse_api_method : string -> api_method option

type endpoint =
  { path : string
  ; method_ : api_method
  ; handler : string
  }

val endpoint_from_file : pages_dir:'a -> string -> api_method -> endpoint
