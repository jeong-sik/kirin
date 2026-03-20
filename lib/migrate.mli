(** Kirin Database Migration System

    Provides schema migration management for database evolution.

    {b Features:}
    - Version-tracked migrations
    - Up/Down migration support
    - Transaction-safe migrations
    - Migration status tracking *)

(** {1 Types} *)

(** Migration definition. *)
type migration = {
  version : int;
  name : string;
  up : string;
  down : string;
  checksum : string;
}

(** Migration status. *)
type migration_status = {
  version : int;
  name : string;
  applied : bool;
  applied_at : string option;
  checksum_match : bool;
}

(** Migration result. *)
type migration_result =
  | Success of { applied : int; total : int }
  | Partial of { applied : int; failed_at : int; error : string }
  | Error of string

(** {1 Migration Creation} *)

(** [compute_checksum sql] computes the MD5 checksum of an SQL string. *)
val compute_checksum : string -> string

(** [migration ~version ~name ~up ~down] creates a migration with auto-computed checksum. *)
val migration : version:int -> name:string -> up:string -> down:string -> migration

(** [migration_with_checksum ~version ~name ~up ~down ~checksum] creates a migration
    with an explicit checksum. *)
val migration_with_checksum :
  version:int -> name:string -> up:string -> down:string -> checksum:string -> migration

(** {1 Migration Table} *)

(** Migration table name. *)
val table_name : string

(** [get_create_table_sql scheme] returns the CREATE TABLE SQL for the given database scheme. *)
val get_create_table_sql : string option -> string

(** {1 Migration Helpers} *)

(** [sort_migrations migrations] sorts migrations by version (ascending). *)
val sort_migrations : migration list -> migration list

(** [current_version_from_list applied] returns the highest version from a list of applied versions. *)
val current_version_from_list : int list -> int option

(** {1 Pretty Printing} *)

(** [pp_status statuses] formats migration statuses for display. *)
val pp_status : migration_status list -> string

(** [pp_result result] formats a migration result for display. *)
val pp_result : migration_result -> string

(** {1 Database Operations} *)

(** [ensure_table conn scheme] creates the migration table if it does not exist. *)
val ensure_table : (module Db.CONNECTION) -> string option -> (unit, Caqti_error.t) result

(** [get_applied conn] returns all applied migration records. *)
val get_applied :
  (module Db.CONNECTION) ->
  ((int * string * string * string option) list, Caqti_error.t) result

(** [record_migration conn migration] records a migration as applied. *)
val record_migration : (module Db.CONNECTION) -> migration -> (unit, Caqti_error.t) result

(** [remove_migration_record conn version] removes a migration record. *)
val remove_migration_record : (module Db.CONNECTION) -> int -> (unit, Caqti_error.t) result

(** [apply_one conn migration] applies a single migration (up). *)
val apply_one : (module Db.CONNECTION) -> migration -> (unit, string) result

(** [rollback_one conn migration] rolls back a single migration (down). *)
val rollback_one : (module Db.CONNECTION) -> migration -> (unit, string) result

(** {1 Public API with Pool} *)

(** [up pool migrations ~scheme] runs all pending migrations. *)
val up : Db.pool -> migration list -> scheme:string option -> migration_result

(** [down pool migrations ~scheme ?steps ()] rolls back migrations.
    @param steps Number of migrations to roll back (default: 1) *)
val down :
  Db.pool ->
  migration list ->
  scheme:string option ->
  ?steps:int ->
  unit ->
  migration_result

(** [status pool migrations ~scheme] returns the status of all migrations. *)
val status :
  Db.pool ->
  migration list ->
  scheme:string option ->
  (migration_status list, string) result
