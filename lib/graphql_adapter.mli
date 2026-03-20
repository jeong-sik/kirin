(** Kirin GraphQL Integration

    Provides GraphQL API support for Kirin applications using
    ocaml-graphql-server with synchronous execution.

    {b Features:}
    - Schema-first development
    - HTTP endpoint (POST /graphql, GET for introspection)
    - Built-in error handling and batched queries
    - Context passing for auth/request data *)

(** {1 Re-exports from graphql library} *)

(** Schema module for building GraphQL schemas. *)
module Schema = Graphql.Schema

(** Argument definitions. *)
module Arg = Schema.Arg

(** {1 Kirin-style Schema Builders} *)

(** Create a non-null type. *)
val non_null : ('ctx, 'src option) Schema.typ -> ('ctx, 'src) Schema.typ

(** Create a list type. *)
val list : ('ctx, 'src) Schema.typ -> ('ctx, 'src list option) Schema.typ

(** String scalar type. *)
val string : ('ctx, string option) Schema.typ

(** Int scalar type. *)
val int : ('ctx, int option) Schema.typ

(** Float scalar type. *)
val float : ('ctx, float option) Schema.typ

(** Boolean scalar type. *)
val bool : ('ctx, bool option) Schema.typ

(** ID/GUID scalar type (serialized as string). *)
val id : ('ctx, string option) Schema.typ

(** [obj name ~fields] creates an object type. *)
val obj :
  ?doc:string ->
  string ->
  fields:('ctx, 'src) Schema.field list ->
  ('ctx, 'src option) Schema.typ

(** Create a field in an object type. *)
val field :
  ?doc:string ->
  ?deprecated:Schema.deprecated ->
  string ->
  typ:('ctx, 'a) Schema.typ ->
  args:('a, 'b) Arg.arg_list ->
  resolve:('ctx Schema.resolve_info -> 'src -> 'b) ->
  ('ctx, 'src) Schema.field

(** Create an async field (for IO operations). *)
val io_field :
  ?doc:string ->
  ?deprecated:Schema.deprecated ->
  string ->
  typ:('ctx, 'a) Schema.typ ->
  args:(('a, string) result, 'b) Arg.arg_list ->
  resolve:('ctx Schema.resolve_info -> 'src -> 'b) ->
  ('ctx, 'src) Schema.field

(** Create an interface type. *)
val interface :
  ?doc:string ->
  string ->
  fields:(('ctx, 'a) Schema.abstract_typ -> Schema.abstract_field list) ->
  ('ctx, 'a) Schema.abstract_typ

(** Create a union type. *)
val union : ?doc:string -> string -> ('ctx, 'a) Schema.abstract_typ

(** Create an enum type. *)
val enum :
  ?doc:string ->
  string ->
  values:'a Schema.enum_value list ->
  ('ctx, 'a option) Schema.typ

(** Create an enum value. *)
val enum_value :
  ?doc:string ->
  ?deprecated:Schema.deprecated ->
  string ->
  value:'a ->
  'a Schema.enum_value

(** Create a scalar type. *)
val scalar :
  ?doc:string ->
  string ->
  coerce:('a -> Yojson.Basic.t) ->
  ('ctx, 'a option) Schema.typ

(** {1 Schema Creation} *)

(** [schema ?mutations ?subscriptions query] creates a GraphQL schema. *)
val schema :
  ?mutations:('ctx, unit) Schema.field list ->
  ?subscriptions:'ctx Schema.subscription_field list ->
  ('ctx, unit) Schema.field list ->
  'ctx Schema.schema

(** Fix combinator for recursive types. *)
val fix :
  (('ctx, 'a option) Schema.typ Schema.fixpoint -> ('ctx, 'a option) Schema.typ) ->
  ('ctx, 'a option) Schema.typ

(** {1 HTTP Handlers} *)

(** GraphQL request body. *)
type graphql_request = {
  query : string;
  operation_name : string option;
  variables : Yojson.Safe.t option;
}

(** [parse_request body] parses a GraphQL request from a JSON body string.
    Returns [None] if the body is invalid or missing a query field. *)
val parse_request : string -> graphql_request option

(** [yojson_to_const_value json] converts Yojson.Safe.t to Graphql_parser.const_value. *)
val yojson_to_const_value : Yojson.Safe.t -> Graphql_parser.const_value

(** [convert_variables vars] converts variables to Graphql format. *)
val convert_variables : Yojson.Safe.t option -> (string * Graphql_parser.const_value) list

(** [basic_to_safe json] converts Yojson.Basic.t to Yojson.Safe.t. *)
val basic_to_safe : Yojson.Basic.t -> Yojson.Safe.t

(** [execute schema ~ctx ~query ?variables ?operation_name ()] executes a GraphQL query. *)
val execute :
  'ctx Schema.schema ->
  ctx:'ctx ->
  query:string ->
  ?variables:Yojson.Safe.t ->
  ?operation_name:string ->
  unit ->
  [ `Response of Yojson.Basic.t
  | `Stream of Yojson.Basic.t Schema.response Seq.t ] Schema.response

(** [handler ?make_ctx schema req] creates a POST handler for /graphql endpoint.
    Default [make_ctx] returns [unit], so the schema context type is [unit]. *)
val handler :
  ?make_ctx:(Request.t -> unit) ->
  unit Schema.schema ->
  Request.t ->
  Response.t

(** [playground_handler ?endpoint req] serves GraphQL Playground IDE HTML. *)
val playground_handler : ?endpoint:string -> Request.t -> Response.t

(** [introspection_handler schema req] handles introspection queries via GET. *)
val introspection_handler : unit Schema.schema -> Request.t -> Response.t

(** {1 Middleware} *)

(** [middleware ?path ?make_ctx schema] creates GraphQL middleware.
    Adds both POST and GET handlers at the specified path (default: "/graphql"). *)
val middleware :
  ?path:string ->
  ?make_ctx:(Request.t -> unit) ->
  unit Schema.schema ->
  (Request.t -> Response.t) ->
  Request.t ->
  Response.t

(** {1 Subscription Support} *)

(** Subscription field constructor. *)
val subscription_field :
  ?doc:string ->
  ?deprecated:Schema.deprecated ->
  string ->
  typ:('ctx, 'out) Schema.typ ->
  args:(('out Seq.t, string) result, 'args) Arg.arg_list ->
  resolve:('ctx Schema.resolve_info -> 'args) ->
  'ctx Schema.subscription_field

(** {1 Error Helpers} *)

(** GraphQL error type. *)
type graphql_error = {
  message : string;
  locations : (int * int) list option;
  path : string list option;
}

(** [error_response ?status errors] creates a GraphQL error response. *)
val error_response : ?status:Http.Status.t -> graphql_error list -> Response.t

(** [error message] creates a simple error. *)
val error : string -> graphql_error

(** [error_with_location message ~line ~column] creates error with location. *)
val error_with_location : string -> line:int -> column:int -> graphql_error

(** [error_with_path message path] creates error with path. *)
val error_with_path : string -> string list -> graphql_error

(** {1 Batched Queries} *)

(** [batched_handler ?make_ctx schema req] handles batched GraphQL queries.
    Accepts both single requests and arrays of requests. *)
val batched_handler :
  ?make_ctx:(Request.t -> unit) ->
  unit Schema.schema ->
  Request.t ->
  Response.t
