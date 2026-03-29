(** Kirin Database Adapter

    Provides database connectivity with Kirin-style APIs using Caqti-eio.

    {b Features:}
    - Connection pooling with automatic management
    - Transaction support with rollback
    - Direct-style async queries (Eio)
    - Support for PostgreSQL, SQLite, MariaDB

    @since 1.0.0
    @status needs-work
    Core functionality works but error handling and pool management need refinement. *)

(** {1 Types} *)

(** Database error type. *)
type error =
  | Connection_failed of string
  | Query_failed of string
  | Transaction_failed of string
  | Pool_exhausted
  | Timeout
  | Invalid_uri of string

(** [error_to_string error] converts a database error to a human-readable string. *)
val error_to_string : error -> string

(** Connection module type (Caqti compatible). *)
module type CONNECTION = Caqti_eio.CONNECTION

(** Pool type. *)
type pool = (Caqti_eio.connection, Caqti_error.t) Caqti_eio.Pool.t

(** Pool configuration. *)
type pool_config =
  { max_size : int
  ; idle_timeout : float option
  ; connect_timeout : float option
  }

(** Default pool configuration (max_size=10, idle=300s, connect=30s). *)
val default_config : pool_config

(** {1 Connection Pool} *)

(** [with_pool ?config ~sw ~env uri f] creates a connection pool and runs [f] with it.
    The pool is drained after [f] returns. *)
val with_pool
  :  ?config:pool_config
  -> sw:Eio.Switch.t
  -> env:Caqti_eio.stdenv
  -> string
  -> (pool -> 'a)
  -> ('a, error) result

(** [connect ~sw ~env uri] opens a single connection (without pooling). *)
val connect
  :  sw:Eio.Switch.t
  -> env:Caqti_eio.stdenv
  -> string
  -> (Caqti_eio.connection, error) result

(** {1 Query Execution} *)

(** [use pool f] acquires a connection from the pool, applies [f], and releases it. *)
val use
  :  pool
  -> ((module CONNECTION) -> ('a, Caqti_error.t) result)
  -> ('a, error) result

(** {1 Transaction Support} *)

(** [transaction pool f] executes [f] within a transaction.
    Commits on success, rolls back on error. *)
val transaction
  :  pool
  -> ((module CONNECTION) -> ('a, error) result)
  -> ('a, error) result

(** {1 Re-exports for Convenience} *)

(** Request helpers (re-export of Caqti_request). *)
module Request = Caqti_request

(** Type helpers (re-export of Caqti_type). *)
module Type = Caqti_type

(** Infix operators for query building. *)
module Infix = Caqti_request.Infix

(** {1 Query String Helpers} *)

(** [exec_req sql] makes a simple exec request (no results). *)
val exec_req : string -> (unit, unit, [ `Zero ]) Caqti_request.t

(** [find_req in_type out_type sql] makes a find request (exactly one result). *)
val find_req
  :  'a Caqti_type.t
  -> 'b Caqti_type.t
  -> string
  -> ('a, 'b, [ `One ]) Caqti_request.t

(** [find_opt_req in_type out_type sql] makes a find_opt request (zero or one result). *)
val find_opt_req
  :  'a Caqti_type.t
  -> 'b Caqti_type.t
  -> string
  -> ('a, 'b, [ `Zero | `One ]) Caqti_request.t

(** [collect_req in_type out_type sql] makes a collect request (multiple results). *)
val collect_req
  :  'a Caqti_type.t
  -> 'b Caqti_type.t
  -> string
  -> ('a, 'b, [ `Zero | `One | `Many ]) Caqti_request.t

(** {1 Health Check Integration} *)

(** Ping request (SELECT 1). *)
val ping_req : (unit, int, [ `One ]) Caqti_request.t

(** [health_check pool] creates a health check function for the database pool. *)
val health_check : pool -> Health.check

(** {1 URI Utilities} *)

(** [parse_uri uri_string] parses a database URI string. *)
val parse_uri : string -> (Uri.t, error) result

(** [scheme_of_uri uri] returns the URI scheme (postgresql, sqlite3, etc.). *)
val scheme_of_uri : Uri.t -> string option

(** [safe_uri uri] masks credentials in URI for logging. *)
val safe_uri : Uri.t -> Uri.t
