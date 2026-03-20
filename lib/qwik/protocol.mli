type render_request = {
  url : string;
  base : string;
  headers : (string * string) list;
  container_attrs : (string * string) list;
  server_data : Yojson.Safe.t option;
}
type render_response = {
  html : string;
  timing : timing;
  head : Meta.t option;
  container_state : Container.t option;
}
and timing = { render_ms : float; serialize_ms : float; }
type response =
    Success of { id : int; result : render_response; }
  | Failure of { id : int; code : int; message : string; }
val render_request :
  url:string ->
  ?base:string ->
  ?headers:(string * string) list ->
  ?container_attrs:(string * string) list ->
  ?server_data:Yojson.Safe.t -> unit -> render_request
val encode_render : id:int -> render_request -> string
val encode_prefetch : id:int -> qrls:string list -> string
val decode_timing : Yojson__Safe.t -> timing
val decode_response : string -> response
val encode_health : id:int -> string
val encode_batch : (int * render_request) list -> string
val decode_batch : string -> response list
val response_to_json :
  response ->
  [> `Assoc of
       (string *
        [> `Assoc of
             (string *
              [> `Assoc of (string * [> `Float of float ]) list
               | `Int of int
               | `String of string ])
             list
         | `Int of int
         | `String of string ])
       list ]
