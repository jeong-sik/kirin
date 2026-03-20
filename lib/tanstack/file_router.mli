type file_type = Page | Layout | Loading | ErrorBoundary | NotFound
val file_type_of_name : string -> file_type option
type segment =
    Static of string
  | Dynamic of string
  | CatchAll of string
  | Optional of string
  | Group of string
val parse_segment : string -> segment
val segment_to_pattern : segment -> string
type discovered_route = {
  id : string;
  path : string;
  file_path : string;
  segments : segment list;
  has_layout : bool;
  has_loading : bool;
  has_error : bool;
  children : discovered_route list;
}
val is_directory : string -> bool
val is_file : string -> bool
val list_dir : string -> String.t list
val check_special_files : string -> bool * bool * bool * bool
val route_id_of_path : segment list -> string
val discover_routes :
  base_path:'a ->
  parent_segments:segment list -> string -> discovered_route list
val discover : string -> discovered_route list
type manifest_entry = {
  m_id : string;
  m_path : string;
  m_file : string;
  m_parent : string option;
  m_index : bool;
}
val flatten_routes :
  ?parent:string -> discovered_route list -> manifest_entry list
val manifest_to_json :
  discovered_route list ->
  [> `List of
       [> `Assoc of
            (string * [> `Bool of bool | `Null | `String of string ]) list ]
       list ]
val generate_route_tree_type : discovered_route list -> string
