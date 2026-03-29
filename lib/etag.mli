(** ETag support for HTTP caching.

    Provides ETag generation, parsing, comparison, and a middleware that
    automatically adds ETags to responses and handles conditional requests
    (If-None-Match, If-Match).

    @since 1.0.0
    @status stable *)

(** {1 Types} *)

(** ETag type - strong (exact match) or weak (semantic equivalence). *)
type t =
  | Strong of string
  | Weak of string

(** {1 Parsing} *)

(** [parse s] parses an ETag from a header value string.
    Handles both strong (["xxx"]) and weak ([W/"xxx"]) formats. *)
val parse : string -> t

(** [parse_if_none_match header_value] parses a comma-separated
    If-None-Match header into a list of ETags. *)
val parse_if_none_match : string -> t list

(** {1 Serialization} *)

(** [to_string etag] converts an ETag to its header string representation. *)
val to_string : t -> string

(** {1 Generation} *)

(** [generate ?weak content] generates an ETag from content using MD5.
    @param weak If [true], produces a weak ETag (default: [false]). *)
val generate : ?weak:bool -> string -> t

(** [generate_from_stats ~mtime ~size] generates a weak ETag from file
    modification time and size. *)
val generate_from_stats : mtime:float -> size:int -> t

(** {1 Comparison} *)

(** [matches ?weak_comparison etag1 etag2] compares two ETags.
    With [weak_comparison] (default: [true]), values are compared
    regardless of strong/weak distinction. Strong comparison requires
    both ETags to be strong and identical. *)
val matches : ?weak_comparison:bool -> t -> t -> bool

(** [any_match etags target] returns [true] if any ETag in [etags]
    matches [target] (using weak comparison). *)
val any_match : t list -> t -> bool

(** {1 Middleware} *)

(** [middleware handler] adds ETag headers to 200 responses with string bodies
    and returns 304 Not Modified when the client's If-None-Match matches. *)
val middleware : (Request.t -> Response.t) -> Request.t -> Response.t

(** {1 Precondition Checks} *)

(** [check_if_match req current_etag] validates the If-Match precondition
    for PUT/DELETE requests. Returns [`Ok] if the precondition is met or
    absent, [`Precondition_failed] (412) if it fails. *)
val check_if_match : Request.t -> t -> [> `Ok | `Precondition_failed ]
