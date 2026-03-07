(** Request module - Incoming HTTP request representation and accessors. *)

(** Path parameters extracted from route matching. *)
type params = (string * string) list

(** The request type with all HTTP request data. *)
type t = {
  meth : Http.Method.t;
  uri : Uri.t;
  headers : Http.Header.t;
  body_source : Eio.Buf_read.t;
  mutable cached_body : string option;
  params : params;
  ctx : Hmap.t;
  raw : Http.Request.t;
}

(** [make ~raw ~body_source] creates a new request from HTTP components. *)
val make : raw:Http.Request.t -> body_source:Eio.Buf_read.t -> t

(** [meth req] returns the HTTP method. *)
val meth : t -> Http.Method.t

(** [uri req] returns the full URI. *)
val uri : t -> Uri.t

(** [path req] returns the request path. *)
val path : t -> string

(** [headers req] returns all headers. *)
val headers : t -> Http.Header.t

(** [header name req] returns the value of header [name], if present. *)
val header : string -> t -> string option

(** [body req] reads the entire body as a string and caches it. *)
val body : t -> string

(** [body_source req] returns the raw body source for streaming reads. *)
val body_source : t -> Eio.Buf_read.t

(** [param name req] returns the path parameter [name], if present. *)
val param : string -> t -> string option

(** [param_exn name req] returns the path parameter [name].
    Raises [Failure] if [name] is not found. *)
val param_exn : string -> t -> string

(** [query name req] returns the query parameter [name], if present. *)
val query : string -> t -> string option

(** [query_all name req] returns all values for query parameter [name]. *)
val query_all : string -> t -> string list option

(** [with_params params req] returns a copy of [req] with new path parameters. *)
val with_params : params -> t -> t

(** [ctx req] returns the middleware context map. *)
val ctx : t -> Hmap.t

(** [with_ctx ctx req] returns a copy of [req] with a new context map. *)
val with_ctx : Hmap.t -> t -> t

(** [json_body req] parses the request body as JSON. *)
val json_body : t -> (Yojson.Safe.t, [> `Json_parse_error of string ]) result

(** [form_body req] parses the request body as URL-encoded form data. *)
val form_body : t -> (string * string list) list

(** [content_type req] returns the Content-Type header, if present. *)
val content_type : t -> string option

(** [is_json req] returns [true] if the Content-Type indicates JSON. *)
val is_json : t -> bool
