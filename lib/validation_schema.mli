type error =
  { path : string list
  ; message : string
  ; code : string
  }

type 'a result = ('a, error list) Result.t

type string_constraints =
  { min_length : int option
  ; max_length : int option
  ; pattern : string option
  ; format : string option
  }

type number_constraints =
  { minimum : float option
  ; maximum : float option
  ; exclusive_min : bool
  ; exclusive_max : bool
  ; multiple_of : float option
  }

type array_constraints =
  { min_items : int option
  ; max_items : int option
  ; unique_items : bool
  }

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

val empty_string_constraints : string_constraints
val empty_number_constraints : number_constraints
val empty_array_constraints : array_constraints

val string
  :  ?min_length:int
  -> ?max_length:int
  -> ?pattern:string
  -> ?format:string
  -> unit
  -> schema

val int
  :  ?minimum:int
  -> ?maximum:int
  -> ?exclusive_min:bool
  -> ?exclusive_max:bool
  -> ?multiple_of:int
  -> unit
  -> schema

val float
  :  ?minimum:float
  -> ?maximum:float
  -> ?exclusive_min:bool
  -> ?exclusive_max:bool
  -> ?multiple_of:float
  -> unit
  -> schema

val bool : unit -> schema
val null : unit -> schema
val array : ?min_items:int -> ?max_items:int -> ?unique_items:bool -> schema -> schema

val object_
  :  ?required:string list
  -> ?additional:bool
  -> (string * schema) list
  -> schema

val field : 'a -> 'b -> 'a * 'b
val optional : schema -> schema
val one_of : schema list -> schema
val any_of : schema list -> schema
val all_of : schema list -> schema
val enum : Yojson.Safe.t list -> schema
val const : Yojson.Safe.t -> schema
val any : unit -> schema
val custom : (Yojson.Safe.t -> (Yojson.Safe.t, string) Result.t) -> schema
