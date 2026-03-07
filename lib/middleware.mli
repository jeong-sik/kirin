(** Middleware module - Handler composition and built-in middlewares. *)

(** Middleware type: transforms a handler into another handler. *)
type t = Router.handler -> Router.handler

(** [id] is the identity middleware (passes through unchanged). *)
val id : t

(** [compose m1 m2] composes two middlewares (m1 applied first). *)
val compose : t -> t -> t

(** [pipeline middlewares] composes a list of middlewares left-to-right. *)
val pipeline : t list -> t

(** [apply mw handler] applies middleware [mw] to [handler]. *)
val apply : t -> Router.handler -> Router.handler

(** Infix composition operator. [m1 >> m2] is [compose m1 m2]. *)
val ( >> ) : t -> t -> t

(** [logger] logs request method, path, status, and timing to stderr. *)
val logger : t

(** [catch error_handler] catches exceptions and delegates to [error_handler]. *)
val catch : (exn -> Request.t -> Response.t) -> t

(** Default error handler that logs the exception and returns 500. *)
val default_error_handler : exn -> Request.t -> Response.t

(** [catch_default] catches exceptions with the default error handler. *)
val catch_default : t

(** CORS configuration. *)
type cors_config = {
  origins : string list;
  methods : string list;
  headers : string list;
  credentials : bool;
  max_age : int option;
}

(** Default CORS configuration (allows all origins). *)
val default_cors_config : cors_config

(** [cors ?config ()] creates a CORS middleware. *)
val cors : ?config:cors_config -> unit -> t

(** [with_headers hs] adds headers [(name, value)] to every response. *)
val with_headers : (string * string) list -> t

(** [timing] adds an [X-Response-Time] header to responses. *)
val timing : t
