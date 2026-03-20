type obj_ref = int
type sobj =
    SNull
  | SBool of bool
  | SInt of int
  | SFloat of float
  | SString of string
  | SArray of sobj list
  | SObject of (string * sobj) list
  | SRef of obj_ref
  | SQrl of string
  | SSignal of obj_ref
  | SResource of obj_ref * string
  | SStore of obj_ref
type t = {
  mutable objects : (obj_ref * sobj) list;
  mutable qrls : (string * Qrl.t) list;
  mutable next_ref : obj_ref;
  mutable paused : bool;
}
val create : unit -> t
val alloc_ref : t -> obj_ref
val add_object : t -> sobj -> obj_ref
val get_object : t -> obj_ref -> sobj option
val add_qrl : t -> string -> Qrl.t -> unit
val get_qrl : t -> string -> Qrl.t option
val pause : t -> unit
val is_paused : t -> bool
val resume : t -> unit
val sobj_to_json :
  sobj ->
  ([> `Assoc of (string * 'a) list
    | `Bool of bool
    | `Float of float
    | `Int of obj_ref
    | `List of 'a list
    | `Null
    | `String of string ]
   as 'a)
val sobj_of_json : Yojson.Safe.t -> sobj
val to_json :
  t ->
  [> `Assoc of
       (string *
        [> `Assoc of
             (string *
              ([> `Assoc of (string * 'a) list
                | `Bool of bool
                | `Float of float
                | `Int of obj_ref
                | `List of 'a list
                | `Null
                | `String of string ]
               as 'a))
             list ])
       list ]
val of_json : Yojson__Safe.t -> t
val serialize : t -> string
val script_tag : t -> string
val prefetch_script : t -> string
type ctx_id = string
type ctx_value = Yojson.Safe.t
type ctx_store = { mutable contexts : (ctx_id * ctx_value) list; }
val create_ctx_store : unit -> ctx_store
val set_ctx : ctx_store -> ctx_id -> ctx_value -> unit
val get_ctx : ctx_store -> ctx_id -> ctx_value option
val ctx_store_to_json : ctx_store -> [> `Assoc of (ctx_id * ctx_value) list ]
