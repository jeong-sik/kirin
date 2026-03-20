type t = {
  meth : Http.Method.t;
  path : string;
  headers : (string * string) list;
  body : string;
  query : (string * string) list;
}
val empty : t
val with_method : Http.Method.t -> t -> t
val with_path : string -> t -> t
val with_header : string -> string -> t -> t
val with_headers : (string * string) list -> t -> t
val with_body : string -> t -> t
val with_json_body : Yojson.Safe.t -> t -> t
val with_query : string -> string -> t -> t
val with_queries : (string * string) list -> t -> t
val get :
  ?headers:(string * string) list ->
  ?query:(string * string) list -> string -> t
val post :
  ?headers:(string * string) list ->
  ?body:string -> ?content_type:string -> string -> t
val post_json :
  ?headers:(string * string) list -> string -> Yojson.Safe.t -> t
val put :
  ?headers:(string * string) list ->
  ?body:string -> ?content_type:string -> string -> t
val put_json :
  ?headers:(string * string) list -> string -> Yojson.Safe.t -> t
val delete : ?headers:(string * string) list -> string -> t
val patch : ?headers:(string * string) list -> ?body:string -> string -> t
val with_bearer_token : string -> t -> t
val with_basic_auth : string -> string -> t -> t
val with_accept : string -> t -> t
val with_cookie : string -> string -> t -> t
