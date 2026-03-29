type request_id = int

type request =
  { id : request_id
  ; method_ : string
  ; params : Yojson.Safe.t
  }

type response =
  | Success of
      { id : request_id
      ; result : Yojson.Safe.t
      }
  | Error of
      { id : request_id
      ; code : int
      ; message : string
      ; data : Yojson.Safe.t option
      }

val parse_error : int
val invalid_request : int
val method_not_found : int
val invalid_params : int
val internal_error : int
val render_timeout : int
val component_error : int
val hydration_error : int
val next_id : int ref
val gen_id : unit -> int

val render_request
  :  url:string
  -> ?props:Yojson.Safe.t
  -> ?islands:Island.t list
  -> unit
  -> request

val render_island_request
  :  island_id:string
  -> component:string
  -> ?props:Yojson.Safe.t
  -> unit
  -> request

val content_request : collection:string -> slug:string -> unit -> request
val health_request : unit -> request
val encode_request : request -> string
val decode_response : string -> response
val success_response : id:request_id -> Yojson.Safe.t -> response

val error_response
  :  id:request_id
  -> code:int
  -> message:string
  -> ?data:Yojson.Safe.t
  -> unit
  -> response

val encode_response : response -> string

type render_result =
  { html : string
  ; head : string
  ; islands : (string * string) list
  ; scripts : string list
  }

val parse_render_result : Yojson__Safe.t -> render_result

type batch_request = request list
type batch_response = response list

val encode_batch_request : request list -> string
val decode_batch_response : string -> response list
