type file_type =
    Page
  | PageServer
  | PageTs
  | Layout
  | LayoutServer
  | LayoutTs
  | Error
  | Server
type segment_type =
    Static of string
  | Param of string
  | OptionalParam of string
  | Rest of string
  | Matcher of string * string
  | Group of string
type segment = { raw : string; segment_type : segment_type; in_url : bool; }
val parse_segment : string -> segment
val parse_path : string -> segment list
val pattern_from_segments : segment list -> string
val regex_from_segments : segment list -> string
val classify_file : string -> file_type option
type discovered_route = {
  dir_path : string;
  segments : segment list;
  pattern : string;
  regex : string;
  files : file_type list;
  group : string option;
}
val find_group : segment list -> string option
val discover_route : string -> string list -> discovered_route option
val extract_params :
  discovered_route -> string -> (string * string) list option
val match_route :
  discovered_route list ->
  string -> (discovered_route * (string * string) list) option
val specificity : discovered_route -> int
val sort_routes : discovered_route list -> discovered_route list
val segment_to_json :
  segment ->
  [> `Assoc of
       (string *
        [> `Assoc of
             (string *
              [> `Assoc of (string * [> `String of string ]) list
               | `String of string ])
             list
         | `Bool of bool
         | `String of string ])
       list ]
val route_to_json :
  discovered_route ->
  [> `Assoc of
       (string *
        [> `List of
             [> `Assoc of
                  (string *
                   [> `Assoc of
                        (string *
                         [> `Assoc of (string * [> `String of string ]) list
                          | `String of string ])
                        list
                    | `Bool of bool
                    | `String of string ])
                  list
              | `String of string ]
             list
         | `String of string ])
       list ]
