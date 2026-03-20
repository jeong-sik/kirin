type signal_id = string
type 'a signal_state = {
  id : signal_id;
  mutable value : 'a;
  mutable subscribers : (unit -> unit) list;
  mutable version : int;
}
type signal = {
  name : string;
  initial_value : Yojson.Safe.t;
  computed : bool;
  readonly : bool;
}
type computed = {
  signal : signal;
  dependencies : string list;
  expression : string;
}
type effect_def = {
  effect_name : string;
  effect_dependencies : string list;
  effect_cleanup : bool;
}
type batch = { operations : (string * Yojson.Safe.t) list; }
val signal :
  ?computed:bool -> ?readonly:bool -> string -> Yojson.Safe.t -> signal
val computed : name:string -> dependencies:string list -> string -> computed
val make_effect : ?cleanup:bool -> name:string -> string list -> effect_def
val batch : (string * Yojson.Safe.t) list -> batch
val is_computed : signal -> bool
val is_readonly : signal -> bool
val name : signal -> string
val initial_value : signal -> Yojson.Safe.t
val to_declaration : signal -> string
val computed_to_declaration : computed -> string
val effect_to_code : effect_def -> string
val batch_to_code : batch -> string
type import_type =
    Signal
  | Computed
  | Effect
  | Batch
  | UseSignal
  | UseComputed
  | UseSignalEffect
val import_to_string : import_type -> string
val generate_import : import_type list -> string
val generate_react_import : import_type list -> string
val signal_to_ts : signal -> string
val to_json : signal -> [> `Assoc of (string * Yojson.Safe.t) list ]
val computed_to_json :
  computed ->
  [> `Assoc of
       (string *
        [> `Assoc of (string * Yojson.Safe.t) list
         | `List of [> `String of string ] list
         | `String of string ])
       list ]
val effect_to_json :
  effect_def ->
  [> `Assoc of
       (string *
        [> `Bool of bool
         | `List of [> `String of string ] list
         | `String of string ])
       list ]
val batch_to_json :
  batch ->
  [> `Assoc of
       (string *
        [> `List of [> `Assoc of (string * Yojson.Safe.t) list ] list ])
       list ]
