(** Preact Signals

    Fine-grained reactivity system for Preact.
    Signals provide automatic dependency tracking and efficient updates. *)

(** {1 Signal Types} *)

(** Signal identifier for tracking *)
type signal_id = string

(** Signal state *)
type 'a signal_state = {
  id: signal_id;
  mutable value: 'a;
  mutable subscribers: (unit -> unit) list;
  mutable version: int;
}

(** Signal type - wrapper for reactive values *)
type signal = {
  name: string;
  initial_value: Yojson.Safe.t;
  computed: bool;
  readonly: bool;
}

(** Computed signal *)
type computed = {
  signal: signal;
  dependencies: string list;
  expression: string;
}

(** Effect definition *)
type effect_def = {
  effect_name: string;
  effect_dependencies: string list;
  effect_cleanup: bool;
}

(** Batch operation *)
type batch = {
  operations: (string * Yojson.Safe.t) list;
}

(** {1 Signal Construction} *)

(** Create a new signal *)
let signal ?(computed = false) ?(readonly = false) name initial_value = {
  name;
  initial_value;
  computed;
  readonly;
}

(** Create a computed signal *)
let computed ~name ~dependencies expression = {
  signal = { name; initial_value = `Null; computed = true; readonly = true };
  dependencies;
  expression;
}

(** Create an effect *)
let make_effect ?(cleanup = false) ~name dependencies = {
  effect_name = name;
  effect_dependencies = dependencies;
  effect_cleanup = cleanup;
}

(** Create a batch operation *)
let batch operations = { operations }

(** {1 Signal Utilities} *)

(** Check if signal is computed *)
let is_computed s = s.computed

(** Check if signal is readonly *)
let is_readonly s = s.readonly

(** Get signal name *)
let name s = s.name

(** Get initial value *)
let initial_value s = s.initial_value

(** {1 Code Generation} *)

(** Generate signal declaration *)
let to_declaration s =
  let value_str = Yojson.Safe.to_string s.initial_value in
  Printf.sprintf "const %s = signal(%s);" s.name value_str

(** Generate computed declaration *)
let computed_to_declaration c =
  Printf.sprintf "const %s = computed(() => %s);" c.signal.name c.expression

(** Generate effect code *)
let effect_to_code e =
  let deps_str = String.concat ", " e.effect_dependencies in
  if e.effect_cleanup then
    Printf.sprintf {|effect(() => {
  // Effect using: %s
  return () => {
    // Cleanup
  };
});|} deps_str
  else
    Printf.sprintf {|effect(() => {
  // Effect using: %s
});|} deps_str

(** Generate batch operation code *)
let batch_to_code b =
  let ops = List.map (fun (name, value) ->
    Printf.sprintf "  %s.value = %s;" name (Yojson.Safe.to_string value)
  ) b.operations |> String.concat "\n" in
  Printf.sprintf "batch(() => {\n%s\n});" ops

(** {1 Import Generation} *)

(** Import type *)
type import_type =
  | Signal
  | Computed
  | Effect
  | Batch
  | UseSignal
  | UseComputed
  | UseSignalEffect

(** Import to string *)
let import_to_string = function
  | Signal -> "signal"
  | Computed -> "computed"
  | Effect -> "effect"
  | Batch -> "batch"
  | UseSignal -> "useSignal"
  | UseComputed -> "useComputed"
  | UseSignalEffect -> "useSignalEffect"

(** Generate import statement *)
let generate_import imports =
  let import_names = List.map import_to_string imports |> String.concat ", " in
  Printf.sprintf "import { %s } from '@preact/signals';" import_names

(** Generate React compat import *)
let generate_react_import imports =
  let import_names = List.map import_to_string imports |> String.concat ", " in
  Printf.sprintf "import { %s } from '@preact/signals-react';" import_names

(** {1 TypeScript Generation} *)

(** Generate TypeScript type for signal *)
let signal_to_ts s =
  let value_type = match s.initial_value with
    | `String _ -> "string"
    | `Int _ | `Float _ -> "number"
    | `Bool _ -> "boolean"
    | `Null -> "null"
    | `List _ -> "unknown[]"
    | `Assoc _ -> "Record<string, unknown>"
    | `Intlit _ -> "number"
  in
  if s.readonly then
    Printf.sprintf "const %s: ReadonlySignal<%s>;" s.name value_type
  else
    Printf.sprintf "const %s: Signal<%s>;" s.name value_type

(** {1 Serialization} *)

(** Signal to JSON *)
let to_json s =
  `Assoc [
    ("name", `String s.name);
    ("initialValue", s.initial_value);
    ("computed", `Bool s.computed);
    ("readonly", `Bool s.readonly);
  ]

(** Computed to JSON *)
let computed_to_json c =
  `Assoc [
    ("signal", to_json c.signal);
    ("dependencies", `List (List.map (fun d -> `String d) c.dependencies));
    ("expression", `String c.expression);
  ]

(** Effect to JSON *)
let effect_to_json e =
  `Assoc [
    ("name", `String e.effect_name);
    ("dependencies", `List (List.map (fun d -> `String d) e.effect_dependencies));
    ("cleanup", `Bool e.effect_cleanup);
  ]

(** Batch to JSON *)
let batch_to_json b =
  let ops = List.map (fun (name, value) ->
    `Assoc [("name", `String name); ("value", value)]
  ) b.operations in
  `Assoc [("operations", `List ops)]
