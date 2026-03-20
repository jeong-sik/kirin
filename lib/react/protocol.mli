type request = { id : int; method_ : string; params : Yojson.Safe.t; }
type response =
    Success of { id : int; result : Yojson.Safe.t; }
  | Error of { id : int; code : int; message : string;
      data : Yojson.Safe.t option;
    }
module Error_code :
  sig
    val parse_error : int
    val invalid_request : int
    val method_not_found : int
    val invalid_params : int
    val internal_error : int
    val render_error : int
    val timeout_error : int
    val memory_error : int
  end
val encode_request : request -> string
val decode_response : string -> (response, string) Result.t
val render_request :
  id:int -> url:string -> ?props:Yojson.Safe.t -> unit -> request
val stream_request :
  id:int -> url:string -> ?props:Yojson.Safe.t -> unit -> request
val health_request : id:int -> request
val shutdown_request : id:int -> request
val extract_html : response -> (string, string) Result.t
val extract_head : response -> string option
type render_result = {
  html : string;
  head : string option;
  status : int option;
  redirect : string option;
}
val extract_render_result : response -> (render_result, string) Result.t
val next_id : unit -> int
val encode_batch : request list -> string
val with_timeout : timeout_ms:'a -> (unit -> 'b) -> 'b
