(** Response module - HTTP response construction. *)

(** Response body variants. *)
type body =
  | String of string
  | Stream of string option Eio.Stream.t
  | Producer of (string option Eio.Stream.t -> unit)

(** Response type. *)
type t = {
  status : Http.Status.t;
  headers : Http.Header.t;
  body : body;
}

(** [make ?status ?headers body] creates a response.
    [body] is a polymorphic variant: [`String s], [`Stream s], or [`Producer p]. *)
val make :
  ?status:Http.Status.t ->
  ?headers:Http.Header.t ->
  [< `String of string | `Stream of string option Eio.Stream.t | `Producer of (string option Eio.Stream.t -> unit) ] ->
  t

(** [status t] returns the HTTP status. *)
val status : t -> Http.Status.t

(** [headers t] returns the response headers. *)
val headers : t -> Http.Header.t

(** [body t] returns the response body. *)
val body : t -> body

(** [header name t] returns the value of header [name]. *)
val header : string -> t -> string option

(** [status_code t] returns the status as an integer. *)
val status_code : t -> int

(** [with_header name value t] adds a header to the response. *)
val with_header : string -> string -> t -> t

(** [with_headers hs t] adds multiple headers as [(name, value)] pairs. *)
val with_headers : (string * string) list -> t -> t

(** [with_status status t] sets the HTTP status. *)
val with_status : Http.Status.t -> t -> t

(** [text ?status body] creates a plain text response. *)
val text : ?status:Http.Status.t -> string -> t

(** [html ?status ?doctype body] creates an HTML response.
    If [doctype] is [true], prepends the DOCTYPE declaration. *)
val html : ?status:Http.Status.t -> ?doctype:bool -> string -> t

(** [json ?status body] creates a JSON response from a Yojson value. *)
val json : ?status:Http.Status.t -> Yojson.Safe.t -> t

(** [json_string ?status body] creates a JSON response from a pre-serialized string. *)
val json_string : ?status:Http.Status.t -> string -> t

(** [empty status] creates an empty response with the given status. *)
val empty : Http.Status.t -> t

(** [redirect ?status location] creates a redirect response (default 302). *)
val redirect : ?status:Http.Status.t -> string -> t

(** [redirect_permanent location] creates a 301 redirect. *)
val redirect_permanent : string -> t

(** [not_found ?body ()] creates a 404 response. *)
val not_found : ?body:string -> unit -> t

(** [bad_request ?body ()] creates a 400 response. *)
val bad_request : ?body:string -> unit -> t

(** [server_error ?body ()] creates a 500 response. *)
val server_error : ?body:string -> unit -> t

(** [stream ?status ?headers s] creates a streaming response. *)
val stream : ?status:Http.Status.t -> ?headers:(string * string) list -> string option Eio.Stream.t -> t

(** [htmx ?status ?target ?swap body] creates an HTMX response with optional swap headers. *)
val htmx : ?status:Http.Status.t -> ?target:string -> ?swap:string -> string -> t
