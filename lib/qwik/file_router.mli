type file_type =
    Index
  | Layout
  | Plugin
  | Service
  | OnRequest
  | Loader
  | Action
  | Unknown
type segment_type =
    Static of string
  | Dynamic of string
  | CatchAll of string
  | Optional of string
  | Group of string
type segment = { raw : string; segment_type : segment_type; priority : int; }
val parse_segment : string -> segment
val parse_path : string -> segment list
val detect_file_type : string -> file_type
val is_route_file : string -> bool
val segment_to_path : segment -> string
val segments_to_pattern : segment list -> string
type discovered_route = {
  dir_path : string;
  url_pattern : string;
  has_index : bool;
  has_layout : bool;
  loaders : string list;
  actions : string list;
  plugins : string list;
  children : discovered_route list;
}
val discover_routes : 'a -> discovered_route
val to_route_def : discovered_route -> Route_def.t
val extract_params :
  pattern:string -> url:string -> (string * string) list option
val segment_to_json :
  segment ->
  [> `Assoc of (string * [> `Int of int | `Null | `String of string ]) list ]
val discovered_to_json :
  discovered_route ->
  ([> `Assoc of
        (string * [> `Bool of bool | `List of 'a list | `String of string ])
        list
    | `String of string ]
   as 'a)
