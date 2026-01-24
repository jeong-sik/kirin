(** Qwik Signals

    Fine-grained reactive state primitives.

    Signals are the reactive primitive in Qwik - they track dependencies
    and trigger updates only where needed. *)

(** {1 Signal Types} *)

(** Signal identifier *)
type signal_id = string

(** Signal value *)
type 'a signal = {
  id: signal_id;
  mutable value: 'a;
  mutable subscribers: signal_id list;
}

(** Computed signal (derived from other signals) *)
type 'a computed = {
  id: signal_id;
  compute: unit -> 'a;
  mutable cached_value: 'a option;
  mutable dependencies: signal_id list;
}

(** Resource signal (async data) *)
type 'a resource = {
  id: signal_id;
  mutable state: 'a resource_state;
  track_fn: unit -> unit;
}

and 'a resource_state =
  | ResourcePending
  | ResourceResolved of 'a
  | ResourceRejected of string

(** {1 Signal Creation} *)

let next_id = ref 0

let gen_id () =
  incr next_id;
  Printf.sprintf "sig_%d" !next_id

(** Create a signal *)
let create value = {
  id = gen_id ();
  value;
  subscribers = [];
}

(** Create a computed signal *)
let computed compute = {
  id = gen_id ();
  compute;
  cached_value = None;
  dependencies = [];
}

(** Create a resource signal *)
let resource track_fn = {
  id = gen_id ();
  state = ResourcePending;
  track_fn;
}

(** {1 Signal Operations} *)

(** Get signal value *)
let get signal = signal.value

(** Set signal value *)
let set signal value =
  signal.value <- value

(** Update signal value with function *)
let update signal f =
  signal.value <- f signal.value

(** {1 Computed Operations} *)

(** Get computed value *)
let get_computed comp =
  match comp.cached_value with
  | Some v -> v
  | None ->
    let v = comp.compute () in
    comp.cached_value <- Some v;
    v

(** Invalidate computed cache *)
let invalidate comp =
  comp.cached_value <- None

(** {1 Resource Operations} *)

(** Get resource state *)
let get_resource res = res.state

(** Check if resource is loading *)
let is_loading res =
  match res.state with
  | ResourcePending -> true
  | _ -> false

(** Check if resource has value *)
let has_value res =
  match res.state with
  | ResourceResolved _ -> true
  | _ -> false

(** Get resource value (may raise) *)
let get_value res =
  match res.state with
  | ResourceResolved v -> v
  | ResourcePending -> failwith "Resource is still loading"
  | ResourceRejected msg -> failwith ("Resource failed: " ^ msg)

(** Get resource value option *)
let get_value_opt res =
  match res.state with
  | ResourceResolved v -> Some v
  | _ -> None

(** Resolve resource *)
let resolve res value =
  res.state <- ResourceResolved value

(** Reject resource *)
let reject res error =
  res.state <- ResourceRejected error

(** {1 Store (Object Signals)} *)

(** Store is a proxy-like object with signal properties *)
type store = {
  id: signal_id;
  mutable props: (string * Yojson.Safe.t) list;
  mutable nested_stores: (string * store) list;
}

(** Create a store *)
let create_store () = {
  id = gen_id ();
  props = [];
  nested_stores = [];
}

(** Get store property *)
let get_prop store key =
  List.assoc_opt key store.props

(** Set store property *)
let set_prop store key value =
  store.props <- (key, value) :: List.filter (fun (k, _) -> k <> key) store.props

(** Get nested store *)
let get_nested store key =
  List.assoc_opt key store.nested_stores

(** Set nested store *)
let set_nested store key nested =
  store.nested_stores <- (key, nested) :: List.filter (fun (k, _) -> k <> key) store.nested_stores

(** {1 Serialization for SSR} *)

(** Signal state for serialization *)
type serialized_signal = {
  sig_id: signal_id;
  sig_value: Yojson.Safe.t;
  sig_subs: signal_id list;
}

(** Serialize signal *)
let serialize_signal (signal : 'a signal) value_to_json = {
  sig_id = signal.id;
  sig_value = value_to_json signal.value;
  sig_subs = signal.subscribers;
}

(** Signal to JSON *)
let signal_to_json (signal : 'a signal) value_to_json =
  `Assoc [
    ("id", `String signal.id);
    ("value", value_to_json signal.value);
    ("subs", `List (List.map (fun s -> `String s) signal.subscribers));
  ]

(** Resource to JSON *)
let resource_to_json (res : 'a resource) value_to_json =
  let state_json = match res.state with
    | ResourcePending -> `Assoc [("state", `String "pending")]
    | ResourceResolved v -> `Assoc [
        ("state", `String "resolved");
        ("value", value_to_json v);
      ]
    | ResourceRejected msg -> `Assoc [
        ("state", `String "rejected");
        ("error", `String msg);
      ]
  in
  `Assoc [
    ("id", `String res.id);
    ("resource", state_json);
  ]

(** Store to JSON *)
let rec store_to_json store =
  `Assoc [
    ("id", `String store.id);
    ("props", `Assoc store.props);
    ("nested", `Assoc (List.map (fun (k, v) -> (k, store_to_json v)) store.nested_stores));
  ]

(** {1 Context} *)

(** Signal context for tracking subscriptions *)
type context = {
  mutable signals: (signal_id * Yojson.Safe.t) list;
  mutable resources: (signal_id * Yojson.Safe.t) list;
  mutable stores: (signal_id * store) list;
}

(** Create context *)
let create_context () = {
  signals = [];
  resources = [];
  stores = [];
}

(** Register signal in context *)
let register_signal ctx (signal : 'a signal) value_to_json =
  ctx.signals <- (signal.id, value_to_json signal.value) :: ctx.signals

(** Register resource in context *)
let register_resource ctx (res : 'a resource) value_to_json =
  ctx.resources <- (res.id, resource_to_json res value_to_json) :: ctx.resources

(** Register store in context *)
let register_store ctx store =
  ctx.stores <- (store.id, store) :: ctx.stores

(** Context to JSON *)
let context_to_json ctx =
  `Assoc [
    ("signals", `Assoc ctx.signals);
    ("resources", `Assoc (List.map (fun (id, v) -> (id, v)) ctx.resources));
    ("stores", `Assoc (List.map (fun (id, s) -> (id, store_to_json s)) ctx.stores));
  ]
