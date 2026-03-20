type render_request = {
  url : string;
  headers : (string * string) list;
  payload : Yojson.Safe.t option;
}
type method_ = Render | RenderStream | Prerender | Health | Warmup
type render_result = {
  html : string;
  head : string;
  payload_json : string option;
  status : int;
  redirect : string option;
}
type stream_chunk =
    Shell of string
  | Content of string
  | Error of string
  | Done
type health_result = { ready : bool; requests : int; memory_mb : int; }
type response_result =
    RenderOk of render_result
  | StreamStart of string
  | HealthOk of health_result
  | WarmupOk
type request = {
  jsonrpc : string;
  id : int;
  method_name : string;
  params : Yojson.Safe.t;
}
type rpc_error = {
  code : int;
  message : string;
  data : Yojson.Safe.t option;
}
type response =
    Success of { id : int; result : Yojson.Safe.t; }
  | Failure of { id : int option; error : rpc_error; }
val parse_error : int
val invalid_request : int
val method_not_found : int
val invalid_params : int
val internal_error : int
val render_error : int
val timeout_error : int
val not_found_error : int
val method_to_string : method_ -> string
val method_of_string : string -> method_ option
val encode_render_request :
  render_request -> [> `Assoc of (string * Yojson.Safe.t) list ]
val encode_request :
  id:int -> method_:method_ -> params:Yojson.Safe.t -> string
val encode_render : id:int -> render_request -> string
val encode_health : id:int -> string
val encode_warmup : id:int -> string
val decode_response : string -> response
val decode_render_result : Yojson__Safe.t -> render_result
val decode_health_result : Yojson__Safe.t -> health_result
val encode_stream_chunk : stream_chunk -> string
val decode_stream_chunk : string -> stream_chunk
val encode_batch : (int * method_ * Yojson.Safe.t) list -> string
val render_request :
  url:string ->
  ?headers:(string * string) list ->
  ?payload:Yojson.Safe.t -> unit -> render_request
val is_success : response -> bool
val get_result : response -> Yojson.Safe.t option
val get_error : response -> rpc_error option
