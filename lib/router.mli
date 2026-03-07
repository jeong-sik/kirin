(** Router module - Route matching and dispatch. *)

(** Handler type - processes a request and returns a response. *)
type handler = Request.t -> Response.t

(** Segment in a route pattern. *)
type segment =
  | Static of string   (** Literal match *)
  | Param of string    (** [:name] capture *)
  | Wildcard           (** [*] match anything *)

(** Route definition. *)
type route = {
  meth : Http.Method.t;
  pattern : string;
  segments : segment list;
  handler : handler;
}

(** Router is a list of routes. *)
type t = route list

(** [parse_pattern pattern] splits a pattern string into segments.
    E.g., ["/users/:id"] becomes [[Static "users"; Param "id"]]. *)
val parse_pattern : string -> segment list

(** [route meth pattern handler] creates a route with the given method and pattern. *)
val route : Http.Method.t -> string -> handler -> route

(** [get pattern handler] creates a GET route. *)
val get : string -> handler -> route

(** [post pattern handler] creates a POST route. *)
val post : string -> handler -> route

(** [put pattern handler] creates a PUT route. *)
val put : string -> handler -> route

(** [patch pattern handler] creates a PATCH route. *)
val patch : string -> handler -> route

(** [delete pattern handler] creates a DELETE route. *)
val delete : string -> handler -> route

(** [head pattern handler] creates a HEAD route. *)
val head : string -> handler -> route

(** [options pattern handler] creates an OPTIONS route. *)
val options : string -> handler -> route

(** [match_segments segments path_parts] matches path parts against segments,
    returning extracted parameters on success. *)
val match_segments : segment list -> string list -> (string * string) list option

(** [find_route routes req] finds the first matching route for a request. *)
val find_route : t -> Request.t -> (route * (string * string) list) option

(** [dispatch routes req] dispatches a request to the matching handler.
    Returns a 404 response if no route matches. *)
val dispatch : t -> Request.t -> Response.t

(** [router routes] creates a handler from a list of routes. *)
val router : t -> handler

(** [scope prefix middlewares routes] creates scoped routes with a URL prefix
    and a list of middlewares applied to each route. *)
val scope : string -> (handler -> handler) list -> route list -> route list
