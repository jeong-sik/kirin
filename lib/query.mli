(** Kirin Query Builder

    Type-safe SQL query builder with a fluent API.

    {b Features:}
    - Fluent API for SELECT, INSERT, UPDATE, DELETE
    - Type-safe parameter binding
    - SQL injection prevention
    - Support for JOINs, WHERE, ORDER BY, LIMIT *)

(** {1 Types} *)

(** SQL parameter value. *)
type param =
  | Null
  | Int of int
  | Int64 of int64
  | Float of float
  | String of string
  | Bool of bool
  | Blob of string

(** Sort order. *)
type order = Asc | Desc

(** Join type. *)
type join_type =
  | Inner
  | Left
  | Right
  | Full
  | Cross

(** Query type. *)
type query_type =
  | Select
  | Insert
  | Update
  | Delete

(** Built query. *)
type query = {
  sql : string;
  params : param list;
}

(** Query builder state. *)
type t

(** {1 Builder Creation} *)

(** [select columns] starts a SELECT query. *)
val select : string list -> t

(** [select_distinct columns] starts a SELECT DISTINCT query. *)
val select_distinct : string list -> t

(** [insert_into table] starts an INSERT query. *)
val insert_into : string -> t

(** [update table] starts an UPDATE query. *)
val update : string -> t

(** [delete_from table] starts a DELETE query. *)
val delete_from : string -> t

(** {1 Builder Methods} *)

(** [from table t] sets the FROM table. *)
val from : string -> t -> t

(** [columns cols t] adds columns for INSERT. *)
val columns : string list -> t -> t

(** [values vals t] adds values for INSERT. *)
val values : param list -> t -> t

(** [values_many rows t] adds multiple value rows for INSERT. *)
val values_many : param list list -> t -> t

(** [set column value t] adds a SET clause for UPDATE. *)
val set : string -> param -> t -> t

(** [set_many pairs t] adds multiple SET clauses. *)
val set_many : (string * param) list -> t -> t

(** [where condition params t] adds a WHERE clause. *)
val where : string -> param list -> t -> t

(** [and_where condition params t] adds an AND WHERE clause. *)
val and_where : string -> param list -> t -> t

(** [or_where condition params t] adds an OR WHERE clause. *)
val or_where : string -> param list -> t -> t

(** [join join_type table condition t] adds a JOIN clause. *)
val join : join_type -> string -> string -> t -> t

(** [inner_join table condition t] adds an INNER JOIN. *)
val inner_join : string -> string -> t -> t

(** [left_join table condition t] adds a LEFT JOIN. *)
val left_join : string -> string -> t -> t

(** [right_join table condition t] adds a RIGHT JOIN. *)
val right_join : string -> string -> t -> t

(** [group_by columns t] adds a GROUP BY clause. *)
val group_by : string list -> t -> t

(** [having condition params t] adds a HAVING clause. *)
val having : string -> param list -> t -> t

(** [order_by column order t] adds an ORDER BY clause. *)
val order_by : string -> order -> t -> t

(** [order_by_many orders t] adds multiple ORDER BY clauses. *)
val order_by_many : (string * order) list -> t -> t

(** [limit n t] sets the LIMIT. *)
val limit : int -> t -> t

(** [offset n t] sets the OFFSET. *)
val offset : int -> t -> t

(** [returning columns t] adds a RETURNING clause (PostgreSQL). *)
val returning : string list -> t -> t

(** {1 SQL Generation} *)

(** [build t] builds the query into SQL and params. *)
val build : t -> query

(** {1 Utility Functions} *)

(** [param_to_string param] converts a param to string (for debugging). *)
val param_to_string : param -> string

(** [pp_query q] pretty prints a query with params. *)
val pp_query : query -> string

(** {1 Raw SQL} *)

(** [raw sql params] creates a query from raw SQL with params. *)
val raw : string -> param list -> query

(** [raw_sql sql] creates a query from raw SQL without params. *)
val raw_sql : string -> query

(** {1 Helpers for Common Patterns} *)

(** [where_id id] adds [WHERE id = ?] clause. *)
val where_id : int -> t -> t

(** [where_in column values t] adds [WHERE column IN (...)] clause. *)
val where_in : string -> param list -> t -> t

(** [where_null column t] adds [WHERE column IS NULL] clause. *)
val where_null : string -> t -> t

(** [where_not_null column t] adds [WHERE column IS NOT NULL] clause. *)
val where_not_null : string -> t -> t

(** [where_like column pattern t] adds [WHERE column LIKE ?] clause. *)
val where_like : string -> string -> t -> t

(** [where_between column low high t] adds [WHERE column BETWEEN ? AND ?] clause. *)
val where_between : string -> param -> param -> t -> t

(** [order_by_recent column] adds ORDER BY column DESC. *)
val order_by_recent : string -> t -> t

(** [paginate ~page ~per_page t] adds LIMIT and OFFSET for pagination. *)
val paginate : page:int -> per_page:int -> t -> t
