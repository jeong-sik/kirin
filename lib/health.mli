(** Kirin Health Check Module

    Kubernetes-style health checks (Liveness/Readiness probes).

    @since 1.0.0
    @status stable

    {b Example:}
    {[
      let health = Health.create () in
      Health.register health "db" db_check;
      Health.register health "redis" redis_check;

      let routes = Health.routes health
    ]} *)

(** {1 Types} *)

(** Health check status. *)
type status =
  | Healthy
  | Unhealthy of string
  | Degraded of string

(** Health check function. *)
type check = unit -> status

(** Health checker instance. *)
type t

(** {1 Creation} *)

(** [create ()] creates a new health checker with no registered checks. *)
val create : unit -> t

(** {1 Registration} *)

(** [register t name check] registers a named health check.
    Replaces any existing check with the same name. *)
val register : t -> string -> check -> unit

(** [set_ready t ready] sets the manual readiness flag. *)
val set_ready : t -> bool -> unit

(** [is_ready t] returns [true] if the manual readiness flag is set. *)
val is_ready : t -> bool

(** {1 Check Logic} *)

(** [check t] runs all registered checks and returns the aggregated status
    along with a JSON object containing status, uptime, and per-check details.
    Status is [Unhealthy] if any check is unhealthy, [Degraded] if any is
    degraded, [Healthy] otherwise. *)
val check : t -> status * Yojson.Safe.t

(** {1 HTTP Handlers} *)

(** [handler t] returns a handler for [/health] that runs all checks
    and returns JSON with 200 (healthy/degraded) or 503 (unhealthy). *)
val handler : t -> Router.handler

(** [live_handler t] returns a handler for [/live] that always returns 200.
    Used as a Kubernetes liveness probe. *)
val live_handler : t -> Router.handler

(** [ready_handler t] returns a handler for [/ready] that returns 200
    when healthy/degraded, 503 when unhealthy.
    Used as a Kubernetes readiness probe. *)
val ready_handler : t -> Router.handler

(** {1 Routes} *)

(** [routes t] creates standard health routes:
    [/health], [/live], [/ready], [/healthz], [/livez], [/readyz]. *)
val routes : t -> Router.route list
