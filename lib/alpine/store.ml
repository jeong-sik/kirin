(** Alpine.js Global Stores

    Alpine.store() for shared state across components.
    Supports SSR with server-side state injection. *)

(** {1 Store Types} *)

(** Store property *)
type property = {
  store_prop_name: string;
  store_prop_value: Yojson.Safe.t;
}

(** Store method *)
type method_ = {
  store_method_name: string;
  store_method_body: string;
}

(** Store definition *)
type t = {
  name: string;
  properties: property list;
  methods: method_ list;
}

(** {1 Store Creation} *)

(** Create store *)
let create name = {
  name;
  properties = [];
  methods = [];
}

(** Add property *)
let with_property name value store =
  let prop = { store_prop_name = name; store_prop_value = value } in
  { store with properties = store.properties @ [prop] }

(** Add method *)
let with_method name body store =
  let m = { store_method_name = name; store_method_body = body } in
  { store with methods = store.methods @ [m] }

(** {1 Convenience Builders} *)

(** Create store from object *)
let from_object name pairs =
  let props = pairs |> List.map (fun (k, v) ->
    { store_prop_name = k; store_prop_value = v }
  ) in
  { name; properties = props; methods = [] }

(** {1 Code Generation} *)

(** Generate store object *)
let to_object store =
  let props = store.properties |> List.map (fun p ->
    Printf.sprintf "%s: %s" p.store_prop_name (Yojson.Safe.to_string p.store_prop_value)
  ) in
  let methods = store.methods |> List.map (fun m ->
    Printf.sprintf "%s() { %s }" m.store_method_name m.store_method_body
  ) in
  "{ " ^ String.concat ", " (props @ methods) ^ " }"

(** Generate Alpine.store() registration *)
let to_alpine_store store =
  Printf.sprintf "Alpine.store('%s', %s)" store.name (to_object store)

(** Generate $store accessor expression *)
let accessor store prop =
  Printf.sprintf "$store.%s.%s" store.name prop

(** {1 SSR State Injection} *)

(** Generate script to register stores *)
let registration_script stores =
  let registrations = stores |> List.map to_alpine_store |> String.concat ";\n  " in
  Printf.sprintf {|<script>
document.addEventListener('alpine:init', () => {
  %s;
});
</script>|} registrations

(** Generate hydration script with server state *)
let hydration_script ~store_name ~data =
  let json = Yojson.Safe.to_string (`Assoc data) in
  Printf.sprintf {|<script>
document.addEventListener('alpine:init', () => {
  Alpine.store('%s', %s);
});
</script>|} store_name json

(** Merge server data into store *)
let with_server_data data store =
  let merged_props = store.properties |> List.map (fun p ->
    match List.assoc_opt p.store_prop_name data with
    | Some v -> { p with store_prop_value = v }
    | None -> p
  ) in
  { store with properties = merged_props }

(** {1 Magic Properties} *)

(** $store magic accessor *)
let magic_store store_name =
  Printf.sprintf "$store.%s" store_name

(** x-text with store binding *)
let x_text_store store_name prop =
  Printf.sprintf "x-text=\"$store.%s.%s\"" store_name prop

(** x-bind with store binding *)
let x_bind_store store_name prop attr =
  Printf.sprintf "x-bind:%s=\"$store.%s.%s\"" attr store_name prop

(** {1 Persist Plugin Support} *)

(** Mark store for persistence *)
type persist_config = {
  persist_name: string;
  persist_storage: [`Local | `Session];
}

(** Generate persisted store *)
let to_persisted_store ?storage:(persist_storage = `Local) store =
  let storage_str = match persist_storage with
    | `Local -> "localStorage"
    | `Session -> "sessionStorage"
  in
  Printf.sprintf {|Alpine.store('%s', Alpine.$persist(%s).using(%s))|}
    store.name (to_object store) storage_str

(** {1 Serialization} *)

(** Property to JSON *)
let property_to_json p =
  `Assoc [
    ("name", `String p.store_prop_name);
    ("value", p.store_prop_value);
  ]

(** Store to JSON *)
let to_json store =
  `Assoc [
    ("name", `String store.name);
    ("properties", `List (List.map property_to_json store.properties));
    ("methods", `List (List.map (fun m ->
      `Assoc [
        ("name", `String m.store_method_name);
        ("body", `String m.store_method_body);
      ]
    ) store.methods));
  ]
