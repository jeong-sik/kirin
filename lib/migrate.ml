(** Kirin Database Migration System (Phase 12)

    Provides schema migration management for database evolution.

    {b Features:}
    - Version-tracked migrations
    - Up/Down migration support
    - Transaction-safe migrations
    - Migration status tracking

    {b Example - Define Migrations:}
    {[
      let migrations = Kirin.Migrate.[
        migration ~version:1 ~name:"create_users"
          ~up:"CREATE TABLE users (id SERIAL PRIMARY KEY, name TEXT)"
          ~down:"DROP TABLE users";

        migration ~version:2 ~name:"add_email"
          ~up:"ALTER TABLE users ADD COLUMN email TEXT"
          ~down:"ALTER TABLE users DROP COLUMN email";
      ]
    ]}

    {b Example - Run Migrations:}
    {[
      Kirin.Db.with_pool ~sw ~env "postgresql://localhost/mydb" (fun pool ->
        (* Run all pending migrations *)
        Kirin.Migrate.up pool migrations;

        (* Check migration status *)
        Kirin.Migrate.status pool migrations)
    ]}
*)

(** {1 Types} *)

(** Migration definition *)
type migration = {
  version : int;
  name : string;
  up : string;      (** SQL to apply migration *)
  down : string;    (** SQL to rollback migration *)
  checksum : string;  (** MD5 of up SQL for validation *)
}

(** Migration status *)
type migration_status = {
  version : int;
  name : string;
  applied : bool;
  applied_at : string option;  (** ISO 8601 timestamp *)
  checksum_match : bool;
}

(** Migration result *)
type migration_result =
  | Success of { applied : int; total : int }
  | Partial of { applied : int; failed_at : int; error : string }
  | Error of string

(** {1 Migration Creation} *)

(** Compute MD5 checksum of SQL *)
let compute_checksum sql =
  let digest = Digestif.MD5.digest_string sql in
  Digestif.MD5.to_hex digest

(** Create a migration *)
let migration ~version ~name ~up ~down = {
  version;
  name;
  up;
  down;
  checksum = compute_checksum up;
}

(** Create a migration with explicit checksum *)
let migration_with_checksum ~version ~name ~up ~down ~checksum = {
  version;
  name;
  up;
  down;
  checksum;
}

(** {1 Migration Table Management} *)

(** Migration table name *)
let table_name = "_kirin_migrations"

(** SQL to create migrations table (PostgreSQL) *)
let create_table_postgresql = Printf.sprintf {|
  CREATE TABLE IF NOT EXISTS %s (
    version INTEGER PRIMARY KEY,
    name TEXT NOT NULL,
    checksum TEXT NOT NULL,
    applied_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
  )
|} table_name

(** SQL to create migrations table (SQLite) *)
let create_table_sqlite = Printf.sprintf {|
  CREATE TABLE IF NOT EXISTS %s (
    version INTEGER PRIMARY KEY,
    name TEXT NOT NULL,
    checksum TEXT NOT NULL,
    applied_at TEXT DEFAULT (datetime('now'))
  )
|} table_name

(** SQL to create migrations table (generic) *)
let create_table_generic = Printf.sprintf {|
  CREATE TABLE IF NOT EXISTS %s (
    version INTEGER PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    checksum VARCHAR(32) NOT NULL,
    applied_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  )
|} table_name

(** Get appropriate CREATE TABLE SQL for database type *)
let get_create_table_sql scheme =
  match scheme with
  | Some "postgresql" -> create_table_postgresql
  | Some "sqlite3" -> create_table_sqlite
  | _ -> create_table_generic

(** {1 Migration Helpers} *)

(** Sort migrations by version *)
let sort_migrations (migrations : migration list) : migration list =
  List.sort (fun (a : migration) (b : migration) -> compare a.version b.version) migrations

(** Get current version (highest applied) - standalone function without pool *)
let current_version_from_list applied =
  match List.rev (List.sort compare applied) with
  | [] -> None
  | v :: _ -> Some v

(** {1 Pretty Printing} *)

(** Format migration status for display *)
let pp_status statuses =
  let lines = List.map (fun s ->
    let status_icon = if s.applied then "✓" else "○" in
    let checksum_warning = if not s.checksum_match then " ⚠ checksum mismatch" else "" in
    let applied_info = match s.applied_at with
      | Some ts -> Printf.sprintf " (applied: %s)" ts
      | None -> ""
    in
    Printf.sprintf "%s %03d: %s%s%s" status_icon s.version s.name applied_info checksum_warning
  ) statuses in
  String.concat "\n" lines

(** Format result for display *)
let pp_result = function
  | Success { applied; total } ->
    Printf.sprintf "✓ Success: %d migrations applied (%d total)" applied total
  | Partial { applied; failed_at; error } ->
    Printf.sprintf "✗ Partial: %d applied, failed at version %d: %s" applied failed_at error
  | Error msg ->
    Printf.sprintf "✗ Error: %s" msg

(** {1 Database Operations}

    These functions require a Db.pool and CONNECTION to execute SQL.
    They are designed to be used with Db.use or Db.transaction.
*)

(** Ensure migration table exists *)
let ensure_table (module C : Db.CONNECTION) scheme =
  let sql = get_create_table_sql scheme in
  let open Caqti_request.Infix in
  let req = (Caqti_type.unit ->. Caqti_type.unit) ~oneshot:true sql in
  C.exec req ()

(** Get applied migration versions *)
let get_applied (module C : Db.CONNECTION) =
  let sql = Printf.sprintf
    "SELECT version, name, checksum, CAST(applied_at AS TEXT) FROM %s ORDER BY version"
    table_name in
  let open Caqti_request.Infix in
  let req = (Caqti_type.unit ->* Caqti_type.(t4 int string string (option string))) sql in
  C.collect_list req ()

(** Record a migration as applied *)
let record_migration (module C : Db.CONNECTION) (migration : migration) =
  let sql = Printf.sprintf
    "INSERT INTO %s (version, name, checksum) VALUES (?, ?, ?)"
    table_name in
  let open Caqti_request.Infix in
  let req = (Caqti_type.(t3 int string string) ->. Caqti_type.unit) sql in
  C.exec req (migration.version, migration.name, migration.checksum)

(** Remove a migration record *)
let remove_migration_record (module C : Db.CONNECTION) version =
  let sql = Printf.sprintf "DELETE FROM %s WHERE version = ?" table_name in
  let open Caqti_request.Infix in
  let req = (Caqti_type.int ->. Caqti_type.unit) sql in
  C.exec req version

(** Apply a single migration (up) - returns string error for use in migration_result *)
let apply_one (module C : Db.CONNECTION) (migration : migration) : (unit, string) result =
  let open Caqti_request.Infix in
  let up_req = (Caqti_type.unit ->. Caqti_type.unit) ~oneshot:true migration.up in
  match C.exec up_req () with
  | Error err -> Error (Caqti_error.show err)
  | Ok () ->
    match record_migration (module C) migration with
    | Error err -> Error (Caqti_error.show err)
    | Ok () -> Ok ()

(** Rollback a single migration (down) - returns string error for use in migration_result *)
let rollback_one (module C : Db.CONNECTION) (migration : migration) : (unit, string) result =
  let open Caqti_request.Infix in
  let down_req = (Caqti_type.unit ->. Caqti_type.unit) ~oneshot:true migration.down in
  match C.exec down_req () with
  | Error err -> Error (Caqti_error.show err)
  | Ok () ->
    match remove_migration_record (module C) migration.version with
    | Error err -> Error (Caqti_error.show err)
    | Ok () -> Ok ()

(** {1 Public API with Pool} *)

(** Run pending migrations (up) *)
let up pool migrations ~scheme : migration_result =
  match Caqti_eio.Pool.use (fun (module C : Db.CONNECTION) ->
    match ensure_table (module C) scheme with
    | Stdlib.Error err -> Stdlib.Ok (Stdlib.Error (Printf.sprintf "Failed to create migrations table: %s" (Caqti_error.show err)))
    | Stdlib.Ok () ->
      match get_applied (module C) with
      | Stdlib.Error err -> Stdlib.Ok (Stdlib.Error (Printf.sprintf "Failed to get applied migrations: %s" (Caqti_error.show err)))
      | Stdlib.Ok applied ->
        let applied_versions = List.map (fun (v, _, _, _) -> v) applied in
        let sorted = sort_migrations migrations in
        let pending = List.filter (fun (m : migration) ->
          not (List.mem m.version applied_versions)) sorted in

        let rec apply_all count = function
          | [] -> Success { applied = count; total = List.length migrations }
          | (m : migration) :: rest ->
            match apply_one (module C) m with
            | Stdlib.Error e -> Partial { applied = count; failed_at = m.version; error = e }
            | Stdlib.Ok () -> apply_all (count + 1) rest
        in
        Stdlib.Ok (Stdlib.Ok (apply_all 0 pending))
  ) pool with
  | Stdlib.Ok (Stdlib.Ok result) -> result
  | Stdlib.Ok (Stdlib.Error msg) -> Error msg
  | Stdlib.Error err -> Error (Printf.sprintf "Pool error: %s" (Caqti_error.show err))

(** Rollback migrations (down) *)
let down pool migrations ~scheme ?(steps = 1) () : migration_result =
  match Caqti_eio.Pool.use (fun (module C : Db.CONNECTION) ->
    match ensure_table (module C) scheme with
    | Stdlib.Error err -> Stdlib.Ok (Stdlib.Error (Printf.sprintf "Failed to create migrations table: %s" (Caqti_error.show err)))
    | Stdlib.Ok () ->
      match get_applied (module C) with
      | Stdlib.Error err -> Stdlib.Ok (Stdlib.Error (Printf.sprintf "Failed to get applied migrations: %s" (Caqti_error.show err)))
      | Stdlib.Ok applied ->
        let applied_versions = List.map (fun (v, _, _, _) -> v) applied in
        let sorted = sort_migrations migrations |> List.rev in
        let to_rollback = List.filter (fun (m : migration) ->
          List.mem m.version applied_versions) sorted in
        let limited = match steps with
          | n when n > 0 ->
            let rec take n lst = match n, lst with
              | 0, _ | _, [] -> []
              | n, x :: xs -> x :: take (n - 1) xs
            in
            take n to_rollback
          | _ -> to_rollback
        in

        let rec rollback_all count = function
          | [] -> Success { applied = count; total = List.length migrations }
          | (m : migration) :: rest ->
            match rollback_one (module C) m with
            | Stdlib.Error e -> Partial { applied = count; failed_at = m.version; error = e }
            | Stdlib.Ok () -> rollback_all (count + 1) rest
        in
        Stdlib.Ok (Stdlib.Ok (rollback_all 0 limited))
  ) pool with
  | Stdlib.Ok (Stdlib.Ok result) -> result
  | Stdlib.Ok (Stdlib.Error msg) -> Error msg
  | Stdlib.Error err -> Error (Printf.sprintf "Pool error: %s" (Caqti_error.show err))

(** Get migration status *)
let status pool migrations ~scheme : (migration_status list, string) Stdlib.result =
  match Caqti_eio.Pool.use (fun (module C : Db.CONNECTION) ->
    match ensure_table (module C) scheme with
    | Stdlib.Error err -> Stdlib.Ok (Stdlib.Error (Printf.sprintf "Failed: %s" (Caqti_error.show err)))
    | Stdlib.Ok () ->
      match get_applied (module C) with
      | Stdlib.Error err -> Stdlib.Ok (Stdlib.Error (Printf.sprintf "Failed: %s" (Caqti_error.show err)))
      | Stdlib.Ok applied ->
        let applied_map = List.fold_left (fun acc (v, _, checksum, applied_at) ->
          (v, (checksum, applied_at)) :: acc) [] applied in

        let sorted = sort_migrations migrations in
        let statuses = List.map (fun (m : migration) ->
          match List.assoc_opt m.version applied_map with
          | None ->
            { version = m.version;
              name = m.name;
              applied = false;
              applied_at = None;
              checksum_match = true }
          | Some (checksum, applied_at) ->
            { version = m.version;
              name = m.name;
              applied = true;
              applied_at;
              checksum_match = checksum = m.checksum }
        ) sorted in
        Stdlib.Ok (Stdlib.Ok statuses)
  ) pool with
  | Stdlib.Ok (Stdlib.Ok result) -> Stdlib.Ok result
  | Stdlib.Ok (Stdlib.Error msg) -> Stdlib.Error msg
  | Stdlib.Error err -> Stdlib.Error (Printf.sprintf "Pool error: %s" (Caqti_error.show err))
