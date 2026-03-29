type t =
  { status : Http.Status.t
  ; headers : (string * string) list
  ; body : string
  }

val status : t -> Http.Status.t
val status_code : t -> int
val header : string -> t -> string option
val headers : t -> (string * string) list
val body : t -> string
val json : t -> Yojson.Safe.t option
val is_success : t -> bool
val is_redirect : t -> bool
val is_client_error : t -> bool
val is_server_error : t -> bool
