(** Kirin Connection Pool

    Generic, high-performance connection pool for managing expensive resources
    like database connections, HTTP clients, or any reusable connections.

    @since 1.0.0
    @status stable *)

(** {1 Types} *)

(** Pool configuration. *)
type config = {
  min_size : int;
  max_size : int;
  idle_timeout : float;
  max_wait_time : float;
  health_check_interval : float;
}

(** Pool statistics. *)
type stats = {
  total_connections : int;
  active_connections : int;
  idle_connections : int;
  waiting_requests : int;
  total_acquisitions : int;
  total_timeouts : int;
  total_errors : int;
}

(** Pooled connection wrapper with metadata. *)
type 'a pooled = {
  conn : 'a;
  created_at : float;
  mutable last_used : float;
  mutable use_count : int;
}

(** Connection pool. *)
type 'a t

(** Pool errors. *)
type error =
  | Timeout
  | Pool_exhausted
  | Connection_failed of string
  | Validation_failed

(** Exception raised on pool errors. *)
exception Pool_error of error

(** {1 Configuration} *)

(** Default pool configuration. *)
val default_config : config

(** {1 Pool Creation} *)

(** [create ~create ~destroy ?validate ()] creates a new connection pool.
    @param min_size Minimum connections to maintain (default: 1)
    @param max_size Maximum connections allowed (default: 10)
    @param idle_timeout Seconds before closing idle connection (default: 300)
    @param max_wait_time Max seconds to wait when pool exhausted (default: 30)
    @param health_check_interval Seconds between health checks (default: 60)
    @param create Function to create a new connection
    @param destroy Function to close a connection
    @param validate Optional function to check connection health *)
val create :
  ?min_size:int ->
  ?max_size:int ->
  ?idle_timeout:float ->
  ?max_wait_time:float ->
  ?health_check_interval:float ->
  create:(unit -> 'a) ->
  destroy:('a -> unit) ->
  ?validate:('a -> bool) ->
  unit ->
  'a t

(** {1 Pool Operations} *)

(** [stats pool] returns current pool statistics. *)
val stats : 'a t -> stats

(** [size pool] returns [(active, idle, max_size)]. *)
val size : 'a t -> int * int * int

(** [acquire pool] gets a connection from the pool.
    Blocks if the pool is exhausted. Raises [Pool_error Timeout] on timeout. *)
val acquire : 'a t -> 'a pooled

(** [release pool pooled] returns a connection to the pool. *)
val release : 'a t -> 'a pooled -> unit

(** [use pool f] acquires a connection, applies [f], and releases it.
    The connection is released even if [f] raises an exception. *)
val use : 'a t -> ('a -> 'b) -> 'b

(** [use_pooled pool f] like [use] but passes the pooled wrapper with metadata. *)
val use_pooled : 'a t -> ('a pooled -> 'b) -> 'b

(** {1 Pool Maintenance} *)

(** [cleanup_idle pool] removes connections that have exceeded idle timeout.
    Returns the number of connections removed. *)
val cleanup_idle : 'a t -> int

(** [ensure_minimum pool] creates connections to meet the minimum pool size.
    Returns the number of connections created. *)
val ensure_minimum : 'a t -> int

(** [validate_all pool] validates all idle connections, removing invalid ones.
    Returns the number of connections removed. *)
val validate_all : 'a t -> int

(** {1 Pool Lifecycle} *)

(** [init pool] initializes the pool with minimum connections. *)
val init : 'a t -> unit

(** [shutdown pool] closes all connections and shuts down the pool. *)
val shutdown : 'a t -> unit

(** {1 Error Handling} *)

(** [error_to_string error] converts a pool error to a human-readable string. *)
val error_to_string : error -> string

(** {1 Utilities} *)

(** [connection_info pooled] returns [(age, idle_time, use_count)]. *)
val connection_info : 'a pooled -> float * float * int

(** [has_available pool] returns [true] if the pool can provide a connection. *)
val has_available : 'a t -> bool

(** [idle_count pool] returns the number of idle connections. *)
val idle_count : 'a t -> int

(** [active_count pool] returns the number of active (in-use) connections. *)
val active_count : 'a t -> int
