val escape_for_js : string -> string
val serialize : Yojson.Safe.t -> string
val script_tag : ?var:string -> Yojson.Safe.t -> string
val route_data_tag : route_id:string -> Yojson.Safe.t -> string

type 'a resource_state =
  | Pending
  | Ready of 'a
  | Errored of string
  | Refreshing of 'a

val serialize_resource
  :  ('a -> ([> `String of string ] as 'b))
  -> 'a resource_state
  -> [> `Assoc of (string * 'b) list ]

type store_value = Yojson.Safe.t

val store_data_tag : store_id:string -> Yojson.Safe.t -> string
val signal_data_tag : signal_id:string -> Yojson.Safe.t -> string
val hydration_start : component_id:string -> string
val hydration_end : component_id:string -> string
val with_hydration_markers : component_id:string -> string -> string
val suspense_fallback_marker : suspense_id:string -> string
val suspense_resolved_marker : suspense_id:string -> string

type initial_data =
  { route_id : string option
  ; route_data : Yojson.Safe.t option
  ; stores : (string * Yojson.Safe.t) list
  ; signals : (string * Yojson.Safe.t) list
  ; resources : (string * Yojson.Safe.t) list
  }

val empty_initial_data : initial_data
val all_scripts : initial_data -> string
