(** Relay Support for Kirin GraphQL.

    Provides helpers for building Relay-compliant GraphQL APIs, including
    Global Object Identification, Connections (Pagination), and Node interface. *)

module Schema = Graphql.Schema

(** {1 Global Object Identification} *)

(** [to_global_id type_name id] encodes a type name and ID into a
    base64 global ID. *)
val to_global_id : string -> string -> string

(** [from_global_id global_id] decodes a global ID into
    [(type_name, id)], or [None] if decoding fails. *)
val from_global_id : string -> (string * string) option

(** {1 Pagination Types} *)

(** Relay PageInfo. *)
type page_info = {
  has_next_page : bool;
  has_previous_page : bool;
  start_cursor : string option;
  end_cursor : string option;
}

(** [page_info_type ()] returns the GraphQL PageInfo object type. *)
val page_info_type : unit -> (unit, page_info option) Schema.typ

(** Relay Edge. *)
type 'node edge = {
  cursor : string;
  node : 'node;
}

(** Relay Connection. *)
type 'node connection = {
  edges : 'node edge list;
  page_info : page_info;
  total_count : int option;
}

(** [connection_definitions name node_type] creates Relay connection and
    edge GraphQL types for the given node type.
    Returns [(connection_type, edge_type)]. *)
val connection_definitions :
  string ->
  (unit, 'node option) Schema.typ ->
  (unit, 'node connection option) Schema.typ *
  (unit, 'node edge option) Schema.typ

(** {1 Pagination Helpers} *)

(** [args ()] returns the standard Relay connection arguments
    (first, after, last, before). *)
val args :
  unit ->
  ('a, int option -> string option -> int option -> string option -> 'a)
  Schema.Arg.arg_list

(** Parsed connection arguments. *)
type connection_args = {
  first : int option;
  after : string option;
  last : int option;
  before : string option;
}

(** [get_args first after last before] constructs [connection_args]
    from individual optional values. *)
val get_args :
  int option -> string option -> int option -> string option ->
  connection_args

(** [connection_from_list ?total_count list args] creates a connection
    from an in-memory list with cursor-based pagination.
    For large datasets, use database-level pagination instead. *)
val connection_from_list :
  ?total_count:int ->
  'node list ->
  connection_args ->
  'node connection

(** {1 Node Interface} *)

(** [node_definitions ()] creates the Relay Node interface and a
    [node] field constructor. Returns [(node_interface, node_field_fn)].

    The node interface is a Relay Node with a global [id] field.
    The node field function takes a resolver
    [ctx -> string -> abstract_value option] and returns a query field. *)
val node_definitions :
  unit ->
  ('ctx, unit) Schema.abstract_typ *
  (('ctx Schema.resolve_info -> string -> ('ctx, unit) Schema.abstract_value option) ->
   ('ctx, unit) Schema.field)
