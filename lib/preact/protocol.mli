type request_id = int

type request =
  { id : request_id
  ; method_ : string
  ; params : Yojson.Safe.t
  }

type success_response =
  { success_id : request_id
  ; result : Yojson.Safe.t
  }

type error_detail =
  { code : int
  ; message : string
  ; data : Yojson.Safe.t option
  }

type failure_response =
  { failure_id : request_id option
  ; error : error_detail
  }

type response =
  | RpcSuccess of success_response
  | RpcError of failure_response

val parse_error : int
val invalid_request : int
val method_not_found : int
val invalid_params : int
val internal_error : int
val render_error : int
val timeout_error : int
val component_error : int
val signals_error : int
val request_counter : int ref
val next_id : unit -> int

val render_request
  :  url:string
  -> ?props:Yojson.Safe.t
  -> ?signals:Yojson.Safe.t
  -> unit
  -> request

val health_request : unit -> request
val shutdown_request : unit -> request
val encode_request : request -> string
val encode_error_detail : error_detail -> [> `Assoc of (string * Yojson.Safe.t) list ]
val encode_response : response -> string
val decode_response : string -> (response, 'a) result
val extract_html : Yojson__Safe.t -> string option
val extract_head : Yojson__Safe.t -> string option
val extract_signals : Yojson__Safe.t -> Yojson__Safe.t option
val request_to_json : request -> [> `Assoc of (string * Yojson.Safe.t) list ]
val response_to_json : response -> [> `Assoc of (string * Yojson.Safe.t) list ]
