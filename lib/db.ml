(** Kirin Database Adapter (Phase 12)

    Provides database connectivity with Kirin-style APIs using Caqti-eio.

    {b Features:}
    - Connection pooling with automatic management
    - Transaction support with rollback
    - Direct-style async queries (Eio)
    - Support for PostgreSQL, SQLite, MariaDB

    {b Example - Basic Query:}
    {[
      Kirin.Db.with_pool ~sw ~env "postgresql://localhost/mydb" (fun pool ->
        Kirin.Db.use pool (fun (module C : Kirin.Db.CONNECTION) ->
          let req = Caqti_request.find Caqti_type.int Caqti_type.string
            "SELECT name FROM users WHERE id = ?" in
          C.find req 1))
    ]}

    {b Example - Transaction:}
    {[
      Kirin.Db.transaction pool (fun (module C : Kirin.Db.CONNECTION) ->
        let insert = Caqti_request.exec
          Caqti_type.(t2 string int)
          "INSERT INTO users (name, age) VALUES (?, ?)" in
        let* () = C.exec insert ("Alice", 30) in
        C.exec insert ("Bob", 25))
    ]}
*)

(** {1 Types} *)

(** Database error type *)
type error =
  | Connection_failed of string
  | Query_failed of string
  | Transaction_failed of string
  | Pool_exhausted
  | Timeout
  | Invalid_uri of string

(** Convert error to string *)
let error_to_string = function
  | Connection_failed msg -> Printf.sprintf "Connection failed: %s" msg
  | Query_failed msg -> Printf.sprintf "Query failed: %s" msg
  | Transaction_failed msg -> Printf.sprintf "Transaction failed: %s" msg
  | Pool_exhausted -> "Connection pool exhausted"
  | Timeout -> "Database operation timed out"
  | Invalid_uri msg -> Printf.sprintf "Invalid database URI: %s" msg

(** Connection module type (Caqti compatible) *)
module type CONNECTION = Caqti_eio.CONNECTION

(** Pool type *)
type pool = (Caqti_eio.connection, Caqti_error.t) Caqti_eio.Pool.t

(** Pool configuration *)
type pool_config = {
  max_size : int;
  idle_timeout : float option;  (** seconds *)
  connect_timeout : float option;  (** seconds *)
}

(** Default pool configuration *)
let default_config = {
  max_size = 10;
  idle_timeout = Some 300.0;  (* 5 minutes *)
  connect_timeout = Some 30.0;
}

(** {1 Connection Pool} *)

(** Create a connection pool and run function with it *)
let with_pool ?(config = default_config) ~sw ~env uri_string f =
  let uri = Uri.of_string uri_string in
  let pool_config = Caqti_pool_config.create ~max_size:config.max_size () in
  match Caqti_eio_unix.connect_pool ~sw ~stdenv:env ~pool_config uri with
  | Ok pool ->
    let result = f pool in
    Caqti_eio.Pool.drain pool;
    Ok result
  | Error err ->
    Error (Connection_failed (Caqti_error.show err))

(** Connect without creating a pool (single connection) *)
let connect ~sw ~env uri_string =
  let uri = Uri.of_string uri_string in
  match Caqti_eio_unix.connect ~sw ~stdenv:env uri with
  | Ok conn -> Ok conn
  | Error err -> Error (Connection_failed (Caqti_error.show err))

(** {1 Query Execution} *)

(** Use a connection from the pool *)
let use pool f =
  match Caqti_eio.Pool.use f pool with
  | Ok x -> Ok x
  | Error err -> Error (Query_failed (Caqti_error.show err))

(** {1 Transaction Support} *)

(** Execute a function within a transaction *)
let transaction pool f =
  match Caqti_eio.Pool.use (fun (module C : CONNECTION) ->
    match C.start () with
    | Error err -> Error err
    | Ok () ->
      match f (module C : CONNECTION) with
      | Ok result ->
        (match C.commit () with
         | Ok () -> Ok (Ok result)
         | Error err ->
           let _ = C.rollback () in
           Error err)
      | Error e ->
        let _ = C.rollback () in
        Ok (Error e)
  ) pool with
  | Ok (Ok result) -> Ok result
  | Ok (Error e) -> Error e
  | Error err -> Error (Transaction_failed (Caqti_error.show err))

(** {1 Re-exports for Convenience} *)

(** Request helpers - re-export Caqti_request *)
module Request = Caqti_request

(** Type helpers - re-export Caqti_type *)
module Type = Caqti_type

(** Infix operators for query building *)
module Infix = Caqti_request.Infix

(** {1 Query String Helpers} *)

(** Make a simple exec request (no results) - Caqti 2.x style *)
let exec_req sql =
  let open Caqti_request.Infix in
  (Caqti_type.unit ->. Caqti_type.unit) sql

(** Make a find request (exactly one result) - Caqti 2.x style *)
let find_req in_type out_type sql =
  let open Caqti_request.Infix in
  (in_type ->! out_type) sql

(** Make a find_opt request (zero or one result) - Caqti 2.x style *)
let find_opt_req in_type out_type sql =
  let open Caqti_request.Infix in
  (in_type ->? out_type) sql

(** Make a collect request (multiple results) - Caqti 2.x style *)
let collect_req in_type out_type sql =
  let open Caqti_request.Infix in
  (in_type ->* out_type) sql

(** {1 Health Check Integration} *)

(** Health check query - simple SELECT 1 *)
let ping_req =
  let open Caqti_request.Infix in
  (Caqti_type.unit ->! Caqti_type.int) "SELECT 1"

(** Create a health check for the database pool *)
let health_check pool =
  fun () ->
    match use pool (fun (module C : CONNECTION) -> C.find ping_req ()) with
    | Ok 1 -> Health.Healthy
    | _ -> Health.Unhealthy "Database connection failed"

(** {1 URI Utilities} *)

(** Parse database URI *)
let parse_uri uri_string =
  try Ok (Uri.of_string uri_string)
  with _ -> Error (Invalid_uri uri_string)

(** Get scheme from URI (postgresql, sqlite3, etc.) *)
let scheme_of_uri uri = Uri.scheme uri

(** Mask credentials in URI for logging *)
let safe_uri uri =
  Uri.with_userinfo uri None
