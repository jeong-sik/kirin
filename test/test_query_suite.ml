(** Database tests (Phase 12) - Query Builder, Migration, and Db module *)

open Alcotest

module Q = Kirin.Query

(* Query Builder Tests *)

let test_query_select () =
  let q = Q.(
    select ["id"; "name"; "email"]
    |> from "users"
    |> build
  ) in
  check string "select sql" "SELECT id, name, email FROM users" q.sql;
  check int "no params" 0 (List.length q.params)

let test_query_select_distinct () =
  let q = Q.(
    select_distinct ["name"]
    |> from "users"
    |> build
  ) in
  check string "distinct sql" "SELECT DISTINCT name FROM users" q.sql

let test_query_where () =
  let q = Q.(
    select ["*"]
    |> from "users"
    |> where "age > ?" [Int 18]
    |> build
  ) in
  check string "where sql" "SELECT * FROM users WHERE age > ?" q.sql;
  check int "1 param" 1 (List.length q.params)

let test_query_where_multiple () =
  let q = Q.(
    select ["*"]
    |> from "users"
    |> where "age > ?" [Int 18]
    |> where "status = ?" [String "active"]
    |> build
  ) in
  check string "multiple where sql"
    "SELECT * FROM users WHERE age > ? AND status = ?" q.sql;
  check int "2 params" 2 (List.length q.params)

let test_query_order_by () =
  let q = Q.(
    select ["*"]
    |> from "users"
    |> order_by "name" Asc
    |> order_by "age" Desc
    |> build
  ) in
  check string "order by sql"
    "SELECT * FROM users ORDER BY name ASC, age DESC" q.sql

let test_query_limit_offset () =
  let q = Q.(
    select ["*"]
    |> from "users"
    |> limit 10
    |> offset 20
    |> build
  ) in
  check string "limit offset sql"
    "SELECT * FROM users LIMIT 10 OFFSET 20" q.sql

let test_query_insert () =
  let q = Q.(
    insert_into "users"
    |> columns ["name"; "email"; "age"]
    |> values [String "Alice"; String "alice@example.com"; Int 30]
    |> build
  ) in
  check string "insert sql"
    "INSERT INTO users (name, email, age) VALUES (?, ?, ?)" q.sql;
  check int "3 params" 3 (List.length q.params)

let test_query_update () =
  let q = Q.(
    update "users"
    |> set "name" (String "Bob")
    |> set "age" (Int 25)
    |> where "id = ?" [Int 1]
    |> build
  ) in
  check string "update sql"
    "UPDATE users SET name = ?, age = ? WHERE id = ?" q.sql;
  check int "3 params" 3 (List.length q.params)

let test_query_delete () =
  let q = Q.(
    delete_from "users"
    |> where "id = ?" [Int 1]
    |> build
  ) in
  check string "delete sql" "DELETE FROM users WHERE id = ?" q.sql;
  check int "1 param" 1 (List.length q.params)

let test_query_join () =
  let q = Q.(
    select ["u.name"; "o.total"]
    |> from "users u"
    |> inner_join "orders o" "o.user_id = u.id"
    |> build
  ) in
  check string "join sql"
    "SELECT u.name, o.total FROM users u INNER JOIN orders o ON o.user_id = u.id" q.sql

let test_query_left_join () =
  let q = Q.(
    select ["u.name"; "COUNT(o.id)"]
    |> from "users u"
    |> left_join "orders o" "o.user_id = u.id"
    |> group_by ["u.id"; "u.name"]
    |> build
  ) in
  check string "left join sql"
    "SELECT u.name, COUNT(o.id) FROM users u LEFT JOIN orders o ON o.user_id = u.id GROUP BY u.id, u.name" q.sql

let test_query_returning () =
  let q = Q.(
    insert_into "users"
    |> columns ["name"]
    |> values [String "Alice"]
    |> returning ["id"]
    |> build
  ) in
  check string "returning sql"
    "INSERT INTO users (name) VALUES (?) RETURNING id" q.sql

let test_query_raw () =
  let q = Q.raw "SELECT custom_func(?)" [Q.Int 42] in
  check string "raw sql" "SELECT custom_func(?)" q.sql;
  check int "1 param" 1 (List.length q.params)

let test_query_paginate () =
  let q = Q.(
    select ["*"]
    |> from "users"
    |> paginate ~page:3 ~per_page:10
    |> build
  ) in
  check string "paginate sql"
    "SELECT * FROM users LIMIT 10 OFFSET 20" q.sql

let test_query_where_id () =
  let q = Q.(
    select ["*"]
    |> from "users"
    |> where_id 42
    |> build
  ) in
  check string "where_id sql" "SELECT * FROM users WHERE id = ?" q.sql

let test_query_where_in () =
  let q = Q.(
    select ["*"]
    |> from "users"
    |> where_in "id" [Int 1; Int 2; Int 3]
    |> build
  ) in
  check string "where_in sql"
    "SELECT * FROM users WHERE id IN (?, ?, ?)" q.sql;
  check int "3 params" 3 (List.length q.params)

let test_query_where_null () =
  let q = Q.(
    select ["*"]
    |> from "users"
    |> where_null "deleted_at"
    |> build
  ) in
  check string "where_null sql"
    "SELECT * FROM users WHERE deleted_at IS NULL" q.sql

let test_query_where_like () =
  let q = Q.(
    select ["*"]
    |> from "users"
    |> where_like "name" "%john%"
    |> build
  ) in
  check string "where_like sql"
    "SELECT * FROM users WHERE name LIKE ?" q.sql

(* Issue #43: or_where as first clause should produce valid SQL *)
let test_query_or_where_first_clause () =
  let q = Q.(
    select ["*"]
    |> from "users"
    |> or_where "age > ?" [Int 18]
    |> build
  ) in
  (* Must produce "WHERE age > ?", not "WHERE OR age > ?" *)
  check string "or_where first clause sql"
    "SELECT * FROM users WHERE age > ?" q.sql

let test_query_or_where_after_where () =
  let q = Q.(
    select ["*"]
    |> from "users"
    |> where "name = ?" [String "alice"]
    |> or_where "name = ?" [String "bob"]
    |> build
  ) in
  check string "or_where after where sql"
    "SELECT * FROM users WHERE name = ? OR name = ?" q.sql

let test_query_or_where_first_clause_update () =
  let q = Q.(
    update "users"
    |> set "active" (Bool true)
    |> or_where "role = ?" [String "admin"]
    |> build
  ) in
  check bool "update or_where first clause valid"
    true (not (String.sub q.sql 0 (String.length q.sql) |> fun s ->
      try ignore (Str.search_forward (Str.regexp "WHERE OR") s 0); true
      with Not_found -> false))

let test_query_or_where_first_clause_delete () =
  let q = Q.(
    delete_from "users"
    |> or_where "status = ?" [String "banned"]
    |> build
  ) in
  check string "delete or_where first clause sql"
    "DELETE FROM users WHERE status = ?" q.sql

let query_tests = [
  test_case "select" `Quick test_query_select;
  test_case "select distinct" `Quick test_query_select_distinct;
  test_case "where" `Quick test_query_where;
  test_case "where multiple" `Quick test_query_where_multiple;
  test_case "order by" `Quick test_query_order_by;
  test_case "limit offset" `Quick test_query_limit_offset;
  test_case "insert" `Quick test_query_insert;
  test_case "update" `Quick test_query_update;
  test_case "delete" `Quick test_query_delete;
  test_case "join" `Quick test_query_join;
  test_case "left join" `Quick test_query_left_join;
  test_case "returning" `Quick test_query_returning;
  test_case "raw" `Quick test_query_raw;
  test_case "paginate" `Quick test_query_paginate;
  test_case "where_id" `Quick test_query_where_id;
  test_case "where_in" `Quick test_query_where_in;
  test_case "where_null" `Quick test_query_where_null;
  test_case "where_like" `Quick test_query_where_like;
  test_case "or_where first clause" `Quick test_query_or_where_first_clause;
  test_case "or_where after where" `Quick test_query_or_where_after_where;
  test_case "or_where first clause update" `Quick test_query_or_where_first_clause_update;
  test_case "or_where first clause delete" `Quick test_query_or_where_first_clause_delete;
]

(* Migration Tests (Phase 12) *)

module Mig = Kirin.Migrate

let test_migrate_create () =
  let m = Mig.migration ~version:1 ~name:"create_users"
    ~up:"CREATE TABLE users (id INT PRIMARY KEY)"
    ~down:"DROP TABLE users" in
  check int "version" 1 m.version;
  check string "name" "create_users" m.name;
  check string "up" "CREATE TABLE users (id INT PRIMARY KEY)" m.up;
  check string "down" "DROP TABLE users" m.down;
  (* checksum should be computed from up SQL *)
  check bool "checksum not empty" true (String.length m.checksum > 0)

let test_migrate_checksum () =
  let m1 = Mig.migration ~version:1 ~name:"test" ~up:"SELECT 1" ~down:"SELECT 2" in
  let m2 = Mig.migration ~version:2 ~name:"test" ~up:"SELECT 1" ~down:"SELECT 3" in
  let m3 = Mig.migration ~version:3 ~name:"test" ~up:"SELECT 2" ~down:"SELECT 1" in
  (* Same up SQL should produce same checksum *)
  check string "same checksum" m1.checksum m2.checksum;
  (* Different up SQL should produce different checksum *)
  check bool "different checksum" true (m1.checksum <> m3.checksum)

let test_migrate_sort () =
  let migrations = Mig.[
    migration ~version:3 ~name:"c" ~up:"" ~down:"";
    migration ~version:1 ~name:"a" ~up:"" ~down:"";
    migration ~version:2 ~name:"b" ~up:"" ~down:"";
  ] in
  let sorted = Mig.sort_migrations migrations in
  check int "first version" 1 (List.nth sorted 0).version;
  check int "second version" 2 (List.nth sorted 1).version;
  check int "third version" 3 (List.nth sorted 2).version

let test_migrate_current_version () =
  check (option int) "empty list" None (Mig.current_version_from_list []);
  check (option int) "single" (Some 5) (Mig.current_version_from_list [5]);
  check (option int) "multiple" (Some 10) (Mig.current_version_from_list [5; 10; 3; 7])

let test_migrate_pp_result_success () =
  let r = Mig.Success { applied = 3; total = 5 } in
  let s = Mig.pp_result r in
  check bool "contains success" true (String.length s > 0);
  check bool "has checkmark" true (String.sub s 0 3 = "\xe2\x9c\x93")

let test_migrate_pp_result_partial () =
  let r = Mig.Partial { applied = 2; failed_at = 3; error = "syntax error" } in
  let s = Mig.pp_result r in
  check bool "contains partial" true (String.length s > 0)

let test_migrate_pp_result_error () =
  let r = Mig.Error "connection failed" in
  let s = Mig.pp_result r in
  check bool "contains error" true (String.length s > 0)

let migrate_tests = [
  test_case "create migration" `Quick test_migrate_create;
  test_case "checksum" `Quick test_migrate_checksum;
  test_case "sort migrations" `Quick test_migrate_sort;
  test_case "current version" `Quick test_migrate_current_version;
  test_case "pp_result success" `Quick test_migrate_pp_result_success;
  test_case "pp_result partial" `Quick test_migrate_pp_result_partial;
  test_case "pp_result error" `Quick test_migrate_pp_result_error;
]

(* Db Module Type Tests *)

module D = Kirin.Db

let test_db_error_to_string () =
  check string "connection failed"
    "Connection failed: timeout"
    (D.error_to_string (D.Connection_failed "timeout"));
  check string "query failed"
    "Query failed: syntax error"
    (D.error_to_string (D.Query_failed "syntax error"));
  check string "pool exhausted"
    "Connection pool exhausted"
    (D.error_to_string D.Pool_exhausted);
  check string "timeout"
    "Database operation timed out"
    (D.error_to_string D.Timeout)

let test_db_default_config () =
  check int "max_size" 10 D.default_config.max_size;
  check (option (float 0.01)) "idle_timeout" (Some 300.0) D.default_config.idle_timeout;
  check (option (float 0.01)) "connect_timeout" (Some 30.0) D.default_config.connect_timeout

let test_db_parse_uri () =
  match D.parse_uri "postgresql://localhost/test" with
  | Ok uri -> check string "scheme" "postgresql" (Option.get (Uri.scheme uri))
  | Error _ -> fail "parse_uri should succeed"

let test_db_safe_uri () =
  let uri = Uri.of_string "postgresql://user:pass@localhost/db" in
  let safe = D.safe_uri uri in
  check (option string) "no userinfo" None (Uri.userinfo safe)

let db_tests = [
  test_case "error_to_string" `Quick test_db_error_to_string;
  test_case "default config" `Quick test_db_default_config;
  test_case "parse uri" `Quick test_db_parse_uri;
  test_case "safe uri" `Quick test_db_safe_uri;
]
