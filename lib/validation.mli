(** Request/Response Validation

    Schema-based validation with clear error messages.

    @since 1.0.0
    @status stable

    {b Quick Start:}
    {[
      open Kirin

      let user_schema = Validation.(
        object_ [
          field "name" (string ~min_length:1 ~max_length:100 ());
          field "email" (string ~pattern:"^[^@]+@[^@]+$" ());
          field "age" (int ~minimum:0 ~maximum:150 ());
        ] ~required:["name"; "email"]
      )

      let create_user req =
        match Validation.validate_json user_schema (Request.json req) with
        | Ok data -> Response.json data
        | Error errors -> Response.json ~status:`Bad_request
            (Validation.errors_to_json errors)
    ]}
*)

(** {1 Types} *)

(** Validation error. *)
type error = {
  path : string list;
  message : string;
  code : string;
}

(** Validation result. *)
type 'a result = ('a, error list) Result.t

(** {1 Schema Types} *)

(** String constraints. *)
type string_constraints = {
  min_length : int option;
  max_length : int option;
  pattern : string option;
  format : string option;
}

(** Number constraints (shared by int and float). *)
type number_constraints = {
  minimum : float option;
  maximum : float option;
  exclusive_min : bool;
  exclusive_max : bool;
  multiple_of : float option;
}

(** Array constraints. *)
type array_constraints = {
  min_items : int option;
  max_items : int option;
  unique_items : bool;
}

(** Validation schema. *)
type schema =
  | String of string_constraints
  | Int of number_constraints
  | Float of number_constraints
  | Bool
  | Null
  | Array of schema * array_constraints
  | Object of object_schema
  | OneOf of schema list
  | AnyOf of schema list
  | AllOf of schema list
  | Enum of Yojson.Safe.t list
  | Const of Yojson.Safe.t
  | Any
  | Custom of (Yojson.Safe.t -> (Yojson.Safe.t, string) Result.t)

(** Object schema. *)
and object_schema = {
  properties : (string * schema) list;
  required : string list;
  additional_properties : bool;
}

(** {1 Default Constraints} *)

val empty_string_constraints : string_constraints
val empty_number_constraints : number_constraints
val empty_array_constraints : array_constraints

(** {1 Schema Builders} *)

val string : ?min_length:int -> ?max_length:int -> ?pattern:string -> ?format:string -> unit -> schema
val int : ?minimum:int -> ?maximum:int -> ?exclusive_min:bool -> ?exclusive_max:bool -> ?multiple_of:int -> unit -> schema
val float : ?minimum:float -> ?maximum:float -> ?exclusive_min:bool -> ?exclusive_max:bool -> ?multiple_of:float -> unit -> schema
val bool : unit -> schema
val null : unit -> schema
val array : ?min_items:int -> ?max_items:int -> ?unique_items:bool -> schema -> schema
val object_ : ?required:string list -> ?additional:bool -> (string * schema) list -> schema
val field : string -> schema -> string * schema
val optional : schema -> schema
val one_of : schema list -> schema
val any_of : schema list -> schema
val all_of : schema list -> schema
val enum : Yojson.Safe.t list -> schema
val const : Yojson.Safe.t -> schema
val any : unit -> schema
val custom : (Yojson.Safe.t -> (Yojson.Safe.t, string) Result.t) -> schema

(** {1 Format Validation} *)

val validate_format : string -> string -> (unit, string) Result.t

(** {1 Error Formatting} *)

val format_path : string list -> string
val error_to_json : error -> Yojson.Safe.t
val errors_to_json : error list -> Yojson.Safe.t
val error_to_string : error -> string
val errors_to_string : error list -> string

(** {1 Validation Logic} *)

val make_error : ?path:string list -> code:string -> string -> error

(** {1 Public API} *)

(** Validate JSON against schema. *)
val validate : schema -> Yojson.Safe.t -> Yojson.Safe.t result

(** Validate and return typed result. *)
val validate_json : schema -> Yojson.Safe.t -> Yojson.Safe.t result

(** Validate string representation. *)
val validate_string_json : schema -> string -> Yojson.Safe.t result

(** {1 Common Schemas} *)

val email : unit -> schema
val uuid : unit -> schema
val uri : unit -> schema
val date : unit -> schema
val datetime : unit -> schema
val positive_int : unit -> schema
val non_negative_int : unit -> schema
val percentage : unit -> schema
val non_empty_string : unit -> schema

(** {1 Type-safe Validation} *)

(** Validate and map to OCaml type via [of_yojson]. *)
val validate_type :
  (Yojson.Safe.t -> ('a, string) Result.t) ->
  schema -> Yojson.Safe.t -> ('a, error list) Result.t

(** Handler wrapper for automatic body validation. *)
val validated_body :
  (Yojson.Safe.t -> ('a, string) Result.t) ->
  schema -> ('a -> Middleware.t) -> Middleware.t

(** {1 Kirin Type-safe DSL} *)

(** Typed schema combining a schema with a deserializer. *)
type 'a typed_schema = {
  schema : schema;
  of_json : Yojson.Safe.t -> ('a, string) Result.t;
}

(** Builder module for typed schemas. *)
module Type : sig
  val make : schema -> (Yojson.Safe.t -> ('a, string) Result.t) -> 'a typed_schema
  val string : ?min:int -> ?max:int -> ?pattern:string -> ?format:string -> unit -> string typed_schema
  val int : ?min:int -> ?max:int -> unit -> int typed_schema
  val bool : unit -> bool typed_schema
  val obj : (Yojson.Safe.t -> ('a, string) Result.t) -> (string * schema) list -> 'a typed_schema
  val ( %> ) : string -> 'a typed_schema -> string * schema
end

(** Handler with typed schema validation. *)
val validated : 'a typed_schema -> ('a -> Request.t -> Response.t) -> Request.t -> Response.t

(** {1 Request Helpers} *)

(** Validate body string with schema. *)
val validate_body : schema -> string -> Yojson.Safe.t result

(** Coerce string to JSON based on schema type (for query params). *)
val coerce_string_to_json : schema -> string -> Yojson.Safe.t

(** Validate query parameters. *)
val validate_query_params :
  (string * schema) list -> (string -> string option) -> (unit, error list) Result.t
