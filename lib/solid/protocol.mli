type render_request = {
  url : string;
  props : Yojson.Safe.t;
  meta : Meta.builder option;
}
type render_response = {
  html : string;
  head : string;
  status : int;
  redirect : string option;
  resources : (string * Yojson.Safe.t) list;
}
val request_id : int ref
val next_id : unit -> int
val encode_render_request : render_request -> int * string
val encode_stream_request : url:string -> props:Yojson.Safe.t -> int * string
val encode_health_request : unit -> int * string
val decode_render_response : string -> (render_response, string) result
type stream_chunk =
    Html of string
  | Script of string
  | Complete
  | Error of string
val decode_stream_chunk : string -> stream_chunk
type health_status = {
  ok : bool;
  memory_mb : int;
  uptime_s : int;
  renders : int;
}
val decode_health_response : string -> (health_status, string) result
