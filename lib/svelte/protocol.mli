val request_id : int Atomic.t
val next_id : unit -> int
type render_request = {
  url : string;
  props : Yojson.Safe.t;
  route_id : string option;
  cookies : (string * string) list;
  headers : (string * string) list;
}
type render_response = {
  html : string;
  head : string;
  css : string option;
  error : string option;
  status : int;
  redirect : string option;
  data : Yojson.Safe.t option;
}
type message =
    Request of int * string * Yojson.Safe.t
  | Response of int * Yojson.Safe.t
  | Error of int * int * string
  | Notification of string * Yojson.Safe.t
val encode_render_request : render_request -> int * string
val encode_request : string -> Yojson.Safe.t -> int * string
val encode_notification : string -> Yojson.Safe.t -> string
val decode_render_response :
  Yojson__Safe.t -> (render_response, [> `String of string ]) Result.t
val decode_response : Yojson__Safe.t -> message
val encode_health_request : unit -> int * string
val decode_health_response : Yojson__Safe.t -> bool
val encode_preload_request : string list -> int * string
val encode_invalidate_request : string list -> int * string
val encode_set_context : string -> Yojson.Safe.t -> string
module ErrorCode :
  sig
    val parse_error : int
    val invalid_request : int
    val method_not_found : int
    val invalid_params : int
    val internal_error : int
    val render_error : int
    val timeout_error : int
  end
val error_response :
  'a ->
  'b ->
  'c ->
  [> `Assoc of
       (string *
        [> `Assoc of (string * [> `Int of 'b | `String of 'c ]) list
         | `Int of 'a
         | `String of string ])
       list ]
val encode_batch : (string * Yojson.Safe.t) list -> string
val decode_batch : Yojson__Safe.t -> message list
