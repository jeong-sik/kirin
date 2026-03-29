type signal_id = string

type 'a signal =
  { id : signal_id
  ; mutable value : 'a
  ; mutable subscribers : signal_id list
  }

type 'a computed =
  { id : signal_id
  ; compute : unit -> 'a
  ; mutable cached_value : 'a option
  ; mutable dependencies : signal_id list
  }

type 'a resource =
  { id : signal_id
  ; mutable state : 'a resource_state
  ; track_fn : unit -> unit
  }

and 'a resource_state =
  | ResourcePending
  | ResourceResolved of 'a
  | ResourceRejected of string

val next_id : int ref
val gen_id : unit -> string
val create : 'a -> 'a signal
val computed : (unit -> 'a) -> 'a computed
val resource : (unit -> unit) -> 'a resource
val get : 'a signal -> 'a
val set : 'a signal -> 'a -> unit
val update : 'a signal -> ('a -> 'a) -> unit
val get_computed : 'a computed -> 'a
val invalidate : 'a computed -> unit
val get_resource : 'a resource -> 'a resource_state
val is_loading : 'a resource -> bool
val has_value : 'a resource -> bool
val get_value : 'a resource -> 'a
val get_value_opt : 'a resource -> 'a option
val resolve : 'a resource -> 'a -> unit
val reject : 'a resource -> string -> unit

type store =
  { id : signal_id
  ; mutable props : (string * Yojson.Safe.t) list
  ; mutable nested_stores : (string * store) list
  }

val create_store : unit -> store
val get_prop : store -> string -> Yojson.Safe.t option
val set_prop : store -> string -> Yojson.Safe.t -> unit
val get_nested : store -> string -> store option
val set_nested : store -> string -> store -> unit

type serialized_signal =
  { sig_id : signal_id
  ; sig_value : Yojson.Safe.t
  ; sig_subs : signal_id list
  }

val serialize_signal : 'a signal -> ('a -> Yojson.Safe.t) -> serialized_signal

val signal_to_json
  :  'a signal
  -> ('a -> ([> `List of [> `String of signal_id ] list | `String of signal_id ] as 'b))
  -> [> `Assoc of (string * 'b) list ]

val resource_to_json
  :  'a resource
  -> ('a -> ([> `String of string ] as 'b))
  -> [> `Assoc of (string * [> `Assoc of (string * 'b) list | `String of signal_id ]) list
     ]

val store_to_json : store -> Yojson.Safe.t

type context =
  { mutable signals : (signal_id * Yojson.Safe.t) list
  ; mutable resources : (signal_id * Yojson.Safe.t) list
  ; mutable stores : (signal_id * store) list
  }

val create_context : unit -> context
val register_signal : context -> 'a signal -> ('a -> Yojson.Safe.t) -> unit
val register_resource : context -> 'a resource -> ('a -> Yojson.Safe.t) -> unit
val register_store : context -> store -> unit

val context_to_json
  :  context
  -> [> `Assoc of (string * [> `Assoc of (signal_id * Yojson.Safe.t) list ]) list ]
