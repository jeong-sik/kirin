exception Assertion_error of string
val fail : string -> 'a
val equal : ?msg:string -> Yojson.Safe.t -> Yojson.Safe.t -> unit
val is_true : ?msg:string -> bool -> unit
val is_false : ?msg:string -> bool -> unit
val status : Http.Status.t -> Testing_response.t -> unit
val status_code : int -> Testing_response.t -> unit
val success : Testing_response.t -> unit
val header_exists : string -> Testing_response.t -> unit
val header : string -> string -> Testing_response.t -> unit
val header_contains : string -> string -> Testing_response.t -> unit
val body : string -> Testing_response.t -> unit
val body_contains : string -> Testing_response.t -> unit
val json : Yojson.Safe.t -> Testing_response.t -> unit
val json_path : string -> Yojson.Safe.t -> Testing_response.t -> unit
val json_path_exists : string -> Testing_response.t -> unit
val json_path_string : string -> string -> Testing_response.t -> unit
val json_path_int : string -> int -> Testing_response.t -> unit
val json_path_bool : string -> bool -> Testing_response.t -> unit
val redirect_to : string -> Testing_response.t -> unit
val content_type : string -> Testing_response.t -> unit
val is_json : Testing_response.t -> unit
val is_html : Testing_response.t -> unit
