(** Alpine.js Components

    Reusable Alpine components using x-data and Alpine.data().
    Supports component registration and SSR data injection. *)

(** {1 Component Types} *)

(** Component property *)
type property = {
  prop_name: string;
  prop_default: Yojson.Safe.t;
  prop_type: [`String | `Number | `Boolean | `Array | `Object];
}

(** Component method *)
type method_ = {
  method_name: string;
  method_params: string list;
  method_body: string;
}

(** Component getter *)
type getter = {
  getter_name: string;
  getter_body: string;
}

(** Component watcher *)
type watcher = {
  watch_property: string;
  watch_handler: string;
}

(** Component definition *)
type t = {
  name: string;
  properties: property list;
  methods: method_ list;
  getters: getter list;
  watchers: watcher list;
  init: string option;
  destroy: string option;
}

(** {1 Component Creation} *)

(** Create empty component *)
let create name = {
  name;
  properties = [];
  methods = [];
  getters = [];
  watchers = [];
  init = None;
  destroy = None;
}

(** Add property *)
let with_property ?default:(prop_default = `Null) prop_type prop_name component =
  let prop = { prop_name; prop_default; prop_type } in
  { component with properties = component.properties @ [prop] }

(** Add method *)
let with_method method_name method_params method_body component =
  let m = { method_name; method_params; method_body } in
  { component with methods = component.methods @ [m] }

(** Add getter *)
let with_getter getter_name getter_body component =
  let g = { getter_name; getter_body } in
  { component with getters = component.getters @ [g] }

(** Add watcher *)
let with_watcher watch_property watch_handler component =
  let w = { watch_property; watch_handler } in
  { component with watchers = component.watchers @ [w] }

(** Set init function *)
let with_init init component =
  { component with init = Some init }

(** Set destroy function *)
let with_destroy destroy component =
  { component with destroy = Some destroy }

(** {1 Property Helpers} *)

(** Create string property *)
let string_prop ?(default = "") name =
  { prop_name = name; prop_default = `String default; prop_type = `String }

(** Create number property *)
let number_prop ?(default = 0.0) name =
  { prop_name = name; prop_default = `Float default; prop_type = `Number }

(** Create boolean property *)
let boolean_prop ?(default = false) name =
  { prop_name = name; prop_default = `Bool default; prop_type = `Boolean }

(** Create array property *)
let array_prop ?(default = []) name =
  { prop_name = name; prop_default = `List default; prop_type = `Array }

(** Create object property *)
let object_prop ?(default = []) name =
  { prop_name = name; prop_default = `Assoc default; prop_type = `Object }

(** {1 Code Generation} *)

(** Generate x-data object literal *)
let to_x_data component =
  let props = component.properties |> List.map (fun p ->
    Printf.sprintf "%s: %s" p.prop_name (Yojson.Safe.to_string p.prop_default)
  ) in
  let methods = component.methods |> List.map (fun m ->
    let params = String.concat ", " m.method_params in
    Printf.sprintf "%s(%s) { %s }" m.method_name params m.method_body
  ) in
  let getters = component.getters |> List.map (fun g ->
    Printf.sprintf "get %s() { %s }" g.getter_name g.getter_body
  ) in
  let init_fn = match component.init with
    | Some body -> [Printf.sprintf "init() { %s }" body]
    | None -> []
  in
  let destroy_fn = match component.destroy with
    | Some body -> [Printf.sprintf "destroy() { %s }" body]
    | None -> []
  in
  let all = props @ methods @ getters @ init_fn @ destroy_fn in
  "{ " ^ String.concat ", " all ^ " }"

(** Generate Alpine.data() registration *)
let to_alpine_data component =
  Printf.sprintf "Alpine.data('%s', () => (%s))" component.name (to_x_data component)

(** Generate x-data with component name *)
let to_x_data_ref component =
  Printf.sprintf "x-data=\"%s\"" component.name

(** Generate x-data with initial props override *)
let to_x_data_with_props component props =
  let overrides = props |> List.map (fun (k, v) ->
    Printf.sprintf "%s: %s" k (Yojson.Safe.to_string v)
  ) |> String.concat ", " in
  Printf.sprintf "x-data=\"%s({ %s })\"" component.name overrides

(** {1 SSR Data Injection} *)

(** Generate script for component registration *)
let registration_script components =
  let registrations = components |> List.map to_alpine_data |> String.concat ";\n" in
  Printf.sprintf {|<script>
document.addEventListener('alpine:init', () => {
  %s;
});
</script>|} registrations

(** Generate inline x-data with server values *)
let inline_data ~data component =
  let merged = component.properties |> List.map (fun p ->
    let value = match List.assoc_opt p.prop_name data with
      | Some v -> v
      | None -> p.prop_default
    in
    Printf.sprintf "%s: %s" p.prop_name (Yojson.Safe.to_string value)
  ) in
  let methods = component.methods |> List.map (fun m ->
    let params = String.concat ", " m.method_params in
    Printf.sprintf "%s(%s) { %s }" m.method_name params m.method_body
  ) in
  let all = merged @ methods in
  "{ " ^ String.concat ", " all ^ " }"

(** {1 Serialization} *)

(** Property to JSON *)
let property_to_json p =
  `Assoc [
    ("name", `String p.prop_name);
    ("default", p.prop_default);
    ("type", `String (match p.prop_type with
      | `String -> "string"
      | `Number -> "number"
      | `Boolean -> "boolean"
      | `Array -> "array"
      | `Object -> "object"));
  ]

(** Method to JSON *)
let method_to_json m =
  `Assoc [
    ("name", `String m.method_name);
    ("params", `List (List.map (fun p -> `String p) m.method_params));
    ("body", `String m.method_body);
  ]

(** Component to JSON *)
let to_json component =
  `Assoc [
    ("name", `String component.name);
    ("properties", `List (List.map property_to_json component.properties));
    ("methods", `List (List.map method_to_json component.methods));
    ("init", match component.init with Some i -> `String i | None -> `Null);
    ("destroy", match component.destroy with Some d -> `String d | None -> `Null);
  ]
