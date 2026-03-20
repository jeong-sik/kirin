(** Rate limiting middleware using the token bucket algorithm.

    Provides per-client rate limiting with configurable rates, burst sizes,
    and client identification. Supports custom storage backends for
    distributed deployments.

    @since 1.0.0
    @status stable *)

(** {1 Types} *)

(** Rate limit configuration. *)
type config = {
  requests_per_second : float;  (** Rate of token replenishment. *)
  burst_size : int;             (** Maximum tokens (bucket capacity). *)
}

(** Bucket state for a single client. *)
type bucket = {
  mutable tokens : float;
  mutable last_update : float;
}

(** Information returned when a request is allowed. *)
type allowed_info = {
  remaining : int;
  limit : int;
  reset_after : float;
}

(** Information returned when a request is rate-limited. *)
type limited_info = {
  retry_after : float;
  limit : int;
}

(** {1 Configuration} *)

(** Default configuration: 10 requests/second with burst of 20. *)
val default_config : config

(** {1 In-Memory Storage} *)

(** In-memory bucket storage. *)
module Store : sig
  (** [get key] returns the bucket for [key], if it exists. *)
  val get : string -> bucket option

  (** [set key bucket] stores or replaces the bucket for [key]. *)
  val set : string -> bucket -> unit

  (** [cleanup ~older_than] removes buckets not updated in the last
      [older_than] seconds. *)
  val cleanup : older_than:float -> unit
end

(** {1 Client Identification} *)

(** [get_client_id req] extracts a client identifier from the request.
    Checks X-Forwarded-For, then X-Real-IP, then falls back to ["unknown"]. *)
val get_client_id : Request.t -> string

(** {1 Rate Checking} *)

(** [check_rate_limit config client_id] consumes a token and returns
    [`Allowed info] or [`Limited info]. *)
val check_rate_limit :
  config -> string -> [ `Allowed of allowed_info | `Limited of limited_info ]

(** {1 Response Helpers} *)

(** [add_rate_limit_headers ~limit ~remaining ~reset_after resp] adds
    X-RateLimit-Limit, X-RateLimit-Remaining, and X-RateLimit-Reset headers. *)
val add_rate_limit_headers :
  limit:int -> remaining:int -> reset_after:float -> Response.t -> Response.t

(** [limit_exceeded_response ~retry_after ~limit] creates a 429 Too Many
    Requests response with Retry-After and rate limit headers. *)
val limit_exceeded_response : retry_after:float -> limit:int -> Response.t

(** {1 Middleware} *)

(** [middleware ?config ?get_key handler] applies rate limiting.
    @param config Rate limit configuration (default: [default_config]).
    @param get_key Function to extract client key from request
           (default: [get_client_id]). *)
val middleware :
  ?config:config ->
  ?get_key:(Request.t -> string) ->
  (Request.t -> Response.t) -> (Request.t -> Response.t)

(** {1 Custom Storage} *)

(** Module type for pluggable storage backends (e.g., Redis). *)
module type Storage = sig
  val get : string -> bucket option
  val set : string -> bucket -> unit
end

(** [middleware_with_storage (module S) ?config ?get_key handler] creates
    rate limiting middleware with a custom storage backend. *)
val middleware_with_storage :
  (module Storage) ->
  ?config:config ->
  ?get_key:(Request.t -> string) ->
  (Request.t -> Response.t) -> (Request.t -> Response.t)
