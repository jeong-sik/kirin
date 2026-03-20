type http_method = GET | POST | PUT | PATCH | DELETE | HEAD | OPTIONS
val method_to_string : http_method -> string
type event_context = {
  url : string;
  method_ : http_method;
  params : (string * string) list;
  query : (string * string) list;
  headers : (string * string) list;
  body : Yojson.Safe.t option;
  raw_body : string option;
}
val create_context :
  url:string ->
  method_:http_method ->
  ?params:(string * string) list ->
  ?query:(string * string) list ->
  ?headers:(string * string) list ->
  ?body:Yojson.Safe.t -> ?raw_body:string -> unit -> event_context
val get_param : event_context -> string -> string option
val get_query : event_context -> string -> string option
val get_header : event_context -> string -> string option
val read_body : event_context -> Yojson.Safe.t option
val read_raw_body : event_context -> string option
type action_result =
    ActionData of Yojson.Safe.t
  | ActionRedirect of int * string
  | ActionError of int * string
  | ActionEmpty
  | ActionStream of string
val json : Yojson.Safe.t -> action_result
val redirect : ?status:int -> string -> action_result
val error : int -> string -> action_result
val empty : action_result
val stream : string -> action_result
type handler = event_context -> action_result
type server_route = {
  path : string;
  method_ : http_method option;
  handler : handler;
  middleware : string list;
}
type api_route = { path : string; method_ : http_method; }
val route : string -> handler -> server_route
val get : string -> handler -> server_route
val post : string -> handler -> server_route
val put : string -> handler -> server_route
val delete : string -> handler -> server_route
val with_middleware : string -> server_route -> server_route
type file_info = {
  file_name : string;
  content_type : string;
  file_size : int;
}
type form_value =
    Text of string
  | Number of float
  | Bool of bool
  | File of file_info
  | Multiple of form_value list
type form_data = (string * form_value) list
val get_text : ('a * form_value) list -> 'a -> string option
val get_text_exn : (string * form_value) list -> string -> string
val get_number : ('a * form_value) list -> 'a -> float option
val get_bool : ('a * form_value) list -> 'a -> bool option
val get_file : ('a * form_value) list -> 'a -> file_info option
type validation_error = { field : string; message : string; code : string; }
val validation_error :
  field:string -> message:string -> ?code:string -> unit -> validation_error
val required :
  (string * form_value) list -> string -> (string, validation_error) result
val email :
  (string * form_value) list -> string -> (string, validation_error) result
val min_length :
  int ->
  (string * form_value) list -> string -> (string, validation_error) result
val set_header : 'a -> 'b -> 'c -> 'c
val set_status : int -> action_result -> action_result
val result_to_json :
  action_result -> [> `Assoc of (string * Yojson.Safe.t) list ]
val validation_error_to_json :
  validation_error -> [> `Assoc of (string * [> `String of string ]) list ]
val route_to_json :
  server_route ->
  [> `Assoc of
       (string *
        [> `List of [> `String of string ] list | `Null | `String of string ])
       list ]
