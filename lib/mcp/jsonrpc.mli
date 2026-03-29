type id =
  | Int of int
  | String of string
  | Null

type error_code =
  | Parse_error
  | Invalid_request
  | Method_not_found
  | Invalid_params
  | Internal_error
  | Custom of int

type error =
  { code : error_code
  ; message : string
  ; data : Yojson.Safe.t option
  }

type request =
  { id : id
  ; method_ : string
  ; params : Yojson.Safe.t option
  }

type response =
  { id : id
  ; result : Yojson.Safe.t option
  ; error : error option
  }

type notification =
  { method_ : string
  ; params : Yojson.Safe.t option
  }

type message =
  | Request of request
  | Response of response
  | Notification of notification

val error_code_to_int : error_code -> int
val error_code_of_int : int -> error_code
val id_to_json : id -> [> `Int of int | `Null | `String of string ]
val error_to_json : error -> [> `Assoc of (string * Yojson.Safe.t) list ]
val encode_request : request -> [> `Assoc of (string * Yojson.Safe.t) list ]
val encode_response : response -> [> `Assoc of (string * Yojson.Safe.t) list ]
val encode_notification : notification -> [> `Assoc of (string * Yojson.Safe.t) list ]
val encode : message -> [> `Assoc of (string * Yojson.Safe.t) list ]
val id_of_json : [> `Int of int | `Null | `String of string ] -> id
val error_of_json : Yojson__Safe.t -> error
val decode_request : Yojson__Safe.t -> request
val decode_response : Yojson__Safe.t -> response
val decode_notification : Yojson__Safe.t -> notification
val decode : Yojson__Safe.t -> message
val make_request : id:id -> method_:string -> ?params:Yojson.Safe.t -> unit -> request
val make_response : id:id -> ?result:Yojson.Safe.t -> ?error:error -> unit -> response
val make_notification : method_:string -> ?params:Yojson.Safe.t -> unit -> notification
val make_error : code:error_code -> message:string -> ?data:Yojson.Safe.t -> unit -> error
val success_response : id:id -> Yojson.Safe.t -> response
val error_response : id:id -> error -> response
val extract_meta : [> `Assoc of (string * 'a) list ] option -> 'a option
