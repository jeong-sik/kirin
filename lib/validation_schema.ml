(** Validation Schema Types and Builders

    Type definitions, constraints, and schema builder functions
    extracted from Validation. *)

(** {1 Validation Types} *)

(** Validation error *)
type error =
  { path : string list
  ; message : string
  ; code : string
  }

(** Validation result *)
type 'a result = ('a, error list) Result.t

(** {1 Schema Types} *)

(** String constraints *)
type string_constraints =
  { min_length : int option
  ; max_length : int option
  ; pattern : string option
  ; format : string option (* email, uri, uuid, date, datetime, etc. *)
  }

(** Number constraints *)
type number_constraints =
  { minimum : float option
  ; maximum : float option
  ; exclusive_min : bool
  ; exclusive_max : bool
  ; multiple_of : float option
  }

(** Array constraints *)
type array_constraints =
  { min_items : int option
  ; max_items : int option
  ; unique_items : bool
  }

(** Validation schema *)
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

and object_schema =
  { properties : (string * schema) list
  ; required : string list
  ; additional_properties : bool
  }

(** {1 Default Constraints} *)

let empty_string_constraints =
  { min_length = None; max_length = None; pattern = None; format = None }
;;

let empty_number_constraints =
  { minimum = None
  ; maximum = None
  ; exclusive_min = false
  ; exclusive_max = false
  ; multiple_of = None
  }
;;

let empty_array_constraints = { min_items = None; max_items = None; unique_items = false }

(** {1 Schema Builders} *)

(** String schema *)
let string ?min_length ?max_length ?pattern ?format () =
  String { min_length; max_length; pattern; format }
;;

(** Integer schema *)
let int
      ?minimum
      ?maximum
      ?(exclusive_min = false)
      ?(exclusive_max = false)
      ?multiple_of
      ()
  =
  Int
    { minimum = Option.map float_of_int minimum
    ; maximum = Option.map float_of_int maximum
    ; exclusive_min
    ; exclusive_max
    ; multiple_of = Option.map float_of_int multiple_of
    }
;;

(** Float schema *)
let float
      ?minimum
      ?maximum
      ?(exclusive_min = false)
      ?(exclusive_max = false)
      ?multiple_of
      ()
  =
  Float { minimum; maximum; exclusive_min; exclusive_max; multiple_of }
;;

(** Boolean schema *)
let bool () = Bool

(** Null schema *)
let null () = Null

(** Array schema *)
let array ?min_items ?max_items ?(unique_items = false) items =
  Array (items, { min_items; max_items; unique_items })
;;

(** Object schema *)
let object_ ?(required = []) ?(additional = true) properties =
  Object { properties; required; additional_properties = additional }
;;

(** Field helper *)
let field name schema = name, schema

(** Optional field (wraps in nullable) *)
let optional schema = OneOf [ schema; Null ]

(** OneOf (exactly one must match) *)
let one_of schemas = OneOf schemas

(** AnyOf (at least one must match) *)
let any_of schemas = AnyOf schemas

(** AllOf (all must match) *)
let all_of schemas = AllOf schemas

(** Enum schema *)
let enum values = Enum values

(** Const schema *)
let const value = Const value

(** Any (no validation) *)
let any () = Any

(** Custom validator *)
let custom f = Custom f
