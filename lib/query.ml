(** Kirin Query Builder (Phase 12)

    Type-safe SQL query builder with a fluent API.

    {b Features:}
    - Fluent API for SELECT, INSERT, UPDATE, DELETE
    - Type-safe parameter binding
    - SQL injection prevention
    - Support for JOINs, WHERE, ORDER BY, LIMIT

    {b Example - SELECT:}
    {[
      let q = Kirin.Query.(
        select ["id"; "name"; "email"]
        |> from "users"
        |> where "age > ?" [Int 18]
        |> order_by "name" Asc
        |> limit 10
        |> build
      ) in
      (* q.sql = "SELECT id, name, email FROM users WHERE age > ? ORDER BY name ASC LIMIT 10" *)
    ]}

    {b Example - INSERT:}
    {[
      let q = Kirin.Query.(
        insert_into "users"
        |> columns ["name"; "email"; "age"]
        |> values [String "Alice"; String "alice@example.com"; Int 30]
        |> build
      ) in
      (* q.sql = "INSERT INTO users (name, email, age) VALUES (?, ?, ?)" *)
    ]}

    {b Example - UPDATE:}
    {[
      let q = Kirin.Query.(
        update "users"
        |> set "name" (String "Bob")
        |> set "age" (Int 25)
        |> where "id = ?" [Int 1]
        |> build
      ) in
      (* q.sql = "UPDATE users SET name = ?, age = ? WHERE id = ?" *)
    ]}
*)

(** {1 Types} *)

(** SQL parameter value *)
type param =
  | Null
  | Int of int
  | Int64 of int64
  | Float of float
  | String of string
  | Bool of bool
  | Blob of string

(** Sort order *)
type order = Asc | Desc

(** Join type *)
type join_type =
  | Inner
  | Left
  | Right
  | Full
  | Cross

(** Query type *)
type query_type =
  | Select
  | Insert
  | Update
  | Delete

(** Built query *)
type query = {
  sql : string;
  params : param list;
}

(** Query builder state *)
type t = {
  query_type : query_type;
  table : string option;
  columns : string list;
  values_list : param list list;  (** For INSERT with multiple rows *)
  sets : (string * param) list;   (** For UPDATE *)
  joins : (join_type * string * string) list;  (** (type, table, condition) *)
  where_clauses : (string * param list) list;
  group_by : string list;
  having : (string * param list) option;
  order_by_clauses : (string * order) list;
  limit_val : int option;
  offset_val : int option;
  distinct : bool;
  returning : string list;
}

(** {1 Builder Creation} *)

(** Create empty builder *)
let empty query_type = {
  query_type;
  table = None;
  columns = [];
  values_list = [];
  sets = [];
  joins = [];
  where_clauses = [];
  group_by = [];
  having = None;
  order_by_clauses = [];
  limit_val = None;
  offset_val = None;
  distinct = false;
  returning = [];
}

(** Start a SELECT query *)
let select columns = { (empty Select) with columns }

(** Start a SELECT DISTINCT query *)
let select_distinct columns =
  { (empty Select) with columns; distinct = true }

(** Start an INSERT query *)
let insert_into table = { (empty Insert) with table = Some table }

(** Start an UPDATE query *)
let update table = { (empty Update) with table = Some table }

(** Start a DELETE query *)
let delete_from table = { (empty Delete) with table = Some table }

(** {1 Builder Methods} *)

(** Set FROM table *)
let from table t = { t with table = Some table }

(** Add columns for INSERT *)
let columns cols t = { t with columns = cols }

(** Add values for INSERT *)
let values vals t = { t with values_list = [vals] }

(** Add multiple value rows for INSERT *)
let values_many rows t = { t with values_list = rows }

(** Add SET clause for UPDATE *)
let set column value t =
  { t with sets = t.sets @ [(column, value)] }

(** Add multiple SET clauses *)
let set_many pairs t =
  { t with sets = t.sets @ pairs }

(** Add WHERE clause *)
let where condition params t =
  { t with where_clauses = t.where_clauses @ [(condition, params)] }

(** Add AND WHERE clause (alias for where) *)
let and_where = where

(** Add OR WHERE clause *)
let or_where condition params t =
  let clause = "OR " ^ condition in
  { t with where_clauses = t.where_clauses @ [(clause, params)] }

(** Add JOIN *)
let join join_type table condition t =
  { t with joins = t.joins @ [(join_type, table, condition)] }

(** Add INNER JOIN *)
let inner_join table condition = join Inner table condition

(** Add LEFT JOIN *)
let left_join table condition = join Left table condition

(** Add RIGHT JOIN *)
let right_join table condition = join Right table condition

(** Add GROUP BY *)
let group_by columns t = { t with group_by = columns }

(** Add HAVING *)
let having condition params t =
  { t with having = Some (condition, params) }

(** Add ORDER BY *)
let order_by column order t =
  { t with order_by_clauses = t.order_by_clauses @ [(column, order)] }

(** Add multiple ORDER BY clauses *)
let order_by_many orders t =
  { t with order_by_clauses = t.order_by_clauses @ orders }

(** Set LIMIT *)
let limit n t = { t with limit_val = Some n }

(** Set OFFSET *)
let offset n t = { t with offset_val = Some n }

(** Add RETURNING clause (PostgreSQL) *)
let returning columns t = { t with returning = columns }

(** {1 SQL Generation} *)

(** Convert join type to SQL *)
let join_type_to_sql = function
  | Inner -> "INNER JOIN"
  | Left -> "LEFT JOIN"
  | Right -> "RIGHT JOIN"
  | Full -> "FULL OUTER JOIN"
  | Cross -> "CROSS JOIN"

(** Convert order to SQL *)
let order_to_sql = function
  | Asc -> "ASC"
  | Desc -> "DESC"

(** Build SELECT query *)
let build_select t =
  let table = Option.value t.table ~default:"" in
  let cols = if t.columns = [] then "*" else String.concat ", " t.columns in
  let distinct_str = if t.distinct then "DISTINCT " else "" in

  let parts = [Printf.sprintf "SELECT %s%s FROM %s" distinct_str cols table] in

  (* JOINs *)
  let parts = parts @ List.map (fun (jt, tbl, cond) ->
    Printf.sprintf "%s %s ON %s" (join_type_to_sql jt) tbl cond
  ) t.joins in

  (* WHERE *)
  let where_parts, where_params =
    if t.where_clauses = [] then [], []
    else
      let conditions = List.mapi (fun i (cond, _) ->
        if i = 0 then cond
        else if String.length cond > 3 && String.sub cond 0 3 = "OR " then cond
        else "AND " ^ cond
      ) t.where_clauses in
      let params = List.concat (List.map snd t.where_clauses) in
      ["WHERE " ^ String.concat " " conditions], params
  in
  let parts = parts @ where_parts in

  (* GROUP BY *)
  let parts = if t.group_by = [] then parts
    else parts @ ["GROUP BY " ^ String.concat ", " t.group_by] in

  (* HAVING *)
  let parts, having_params = match t.having with
    | None -> parts, []
    | Some (cond, params) -> parts @ ["HAVING " ^ cond], params
  in

  (* ORDER BY *)
  let parts = if t.order_by_clauses = [] then parts
    else
      let orders = List.map (fun (col, ord) ->
        Printf.sprintf "%s %s" col (order_to_sql ord)
      ) t.order_by_clauses in
      parts @ ["ORDER BY " ^ String.concat ", " orders]
  in

  (* LIMIT *)
  let parts = match t.limit_val with
    | None -> parts
    | Some n -> parts @ [Printf.sprintf "LIMIT %d" n]
  in

  (* OFFSET *)
  let parts = match t.offset_val with
    | None -> parts
    | Some n -> parts @ [Printf.sprintf "OFFSET %d" n]
  in

  { sql = String.concat " " parts;
    params = where_params @ having_params }

(** Build INSERT query *)
let build_insert t =
  let table = Option.value t.table ~default:"" in
  let cols = String.concat ", " t.columns in
  let placeholders = String.concat ", " (List.map (fun _ -> "?") t.columns) in

  let values_str = if List.length t.values_list <= 1 then
    Printf.sprintf "(%s)" placeholders
  else
    let rows = List.map (fun _ ->
      Printf.sprintf "(%s)" placeholders
    ) t.values_list in
    String.concat ", " rows
  in

  let sql = Printf.sprintf "INSERT INTO %s (%s) VALUES %s" table cols values_str in

  (* RETURNING *)
  let sql = if t.returning = [] then sql
    else sql ^ " RETURNING " ^ String.concat ", " t.returning in

  let params = List.concat t.values_list in
  { sql; params }

(** Build UPDATE query *)
let build_update t =
  let table = Option.value t.table ~default:"" in

  let set_clauses = List.map (fun (col, _) ->
    Printf.sprintf "%s = ?" col
  ) t.sets in
  let set_str = String.concat ", " set_clauses in
  let set_params = List.map snd t.sets in

  let parts = [Printf.sprintf "UPDATE %s SET %s" table set_str] in

  (* WHERE *)
  let where_parts, where_params =
    if t.where_clauses = [] then [], []
    else
      let conditions = List.mapi (fun i (cond, _) ->
        if i = 0 then cond
        else if String.length cond > 3 && String.sub cond 0 3 = "OR " then cond
        else "AND " ^ cond
      ) t.where_clauses in
      let params = List.concat (List.map snd t.where_clauses) in
      ["WHERE " ^ String.concat " " conditions], params
  in
  let parts = parts @ where_parts in

  (* RETURNING *)
  let parts = if t.returning = [] then parts
    else parts @ ["RETURNING " ^ String.concat ", " t.returning] in

  { sql = String.concat " " parts;
    params = set_params @ where_params }

(** Build DELETE query *)
let build_delete t =
  let table = Option.value t.table ~default:"" in
  let parts = [Printf.sprintf "DELETE FROM %s" table] in

  (* WHERE *)
  let where_parts, where_params =
    if t.where_clauses = [] then [], []
    else
      let conditions = List.mapi (fun i (cond, _) ->
        if i = 0 then cond
        else if String.length cond > 3 && String.sub cond 0 3 = "OR " then cond
        else "AND " ^ cond
      ) t.where_clauses in
      let params = List.concat (List.map snd t.where_clauses) in
      ["WHERE " ^ String.concat " " conditions], params
  in
  let parts = parts @ where_parts in

  (* RETURNING *)
  let parts = if t.returning = [] then parts
    else parts @ ["RETURNING " ^ String.concat ", " t.returning] in

  { sql = String.concat " " parts;
    params = where_params }

(** Build the query *)
let build t =
  match t.query_type with
  | Select -> build_select t
  | Insert -> build_insert t
  | Update -> build_update t
  | Delete -> build_delete t

(** {1 Utility Functions} *)

(** Convert param to string (for debugging) *)
let param_to_string = function
  | Null -> "NULL"
  | Int n -> string_of_int n
  | Int64 n -> Int64.to_string n
  | Float f -> string_of_float f
  | String s -> Printf.sprintf "'%s'" (String.escaped s)
  | Bool b -> if b then "TRUE" else "FALSE"
  | Blob _ -> "<blob>"

(** Pretty print query with params *)
let pp_query q =
  Printf.sprintf "%s\n-- Params: [%s]"
    q.sql
    (String.concat "; " (List.map param_to_string q.params))

(** {1 Raw SQL} *)

(** Create query from raw SQL *)
let raw sql params = { sql; params }

(** Create query from raw SQL without params *)
let raw_sql sql = { sql; params = [] }

(** {1 Helpers for Common Patterns} *)

(** WHERE id = ? *)
let where_id id = where "id = ?" [Int id]

(** WHERE column IN (?, ?, ...) *)
let where_in column values t =
  let placeholders = String.concat ", " (List.map (fun _ -> "?") values) in
  let condition = Printf.sprintf "%s IN (%s)" column placeholders in
  where condition values t

(** WHERE column IS NULL *)
let where_null column t =
  where (Printf.sprintf "%s IS NULL" column) [] t

(** WHERE column IS NOT NULL *)
let where_not_null column t =
  where (Printf.sprintf "%s IS NOT NULL" column) [] t

(** WHERE column LIKE ? *)
let where_like column pattern t =
  where (Printf.sprintf "%s LIKE ?" column) [String pattern] t

(** WHERE column BETWEEN ? AND ? *)
let where_between column low high t =
  where (Printf.sprintf "%s BETWEEN ? AND ?" column) [low; high] t

(** ORDER BY column DESC, common for recent items *)
let order_by_recent column = order_by column Desc

(** Pagination helper *)
let paginate ~page ~per_page t =
  t |> limit per_page |> offset ((page - 1) * per_page)
