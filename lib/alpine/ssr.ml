(** Alpine.js SSR Engine

    Server-side rendering support for Alpine.js.
    Injects initial state for client-side hydration. *)

(** {1 SSR Configuration} *)

(** SSR config *)
type ssr_config = {
  cdn_url: string;
  version: string;
  include_cloak_style: bool;
  defer_init: bool;
  plugins: string list;
}

(** Default configuration *)
let default_config = {
  cdn_url = "https://cdn.jsdelivr.net/npm/alpinejs";
  version = "3.x.x";
  include_cloak_style = true;
  defer_init = false;
  plugins = [];
}

(** {1 Script Generation} *)

(** Generate Alpine script tag *)
let script_tag config =
  let defer = if config.defer_init then " defer" else "" in
  Printf.sprintf {|<script%s src="%s@%s/dist/cdn.min.js"></script>|}
    defer config.cdn_url config.version

(** Generate plugin script tags *)
let plugin_scripts config =
  config.plugins |> List.map (fun plugin ->
    Printf.sprintf {|<script src="%s@%s/dist/cdn.min.js"></script>|}
      (config.cdn_url ^ "-" ^ plugin) config.version
  ) |> String.concat "\n"

(** Generate x-cloak style *)
let cloak_style =
  {|<style>[x-cloak] { display: none !important; }</style>|}

(** Generate full head content *)
let head_scripts config =
  let cloak = if config.include_cloak_style then cloak_style ^ "\n" else "" in
  let plugins = match config.plugins with
    | [] -> ""
    | _ -> plugin_scripts config ^ "\n"
  in
  cloak ^ plugins ^ script_tag config

(** {1 SSR State Injection} *)

(** SSR state *)
type ssr_state = {
  components: Component.t list;
  stores: Store.t list;
  initial_data: (string * Yojson.Safe.t) list;
}

(** Empty state *)
let empty_state = {
  components = [];
  stores = [];
  initial_data = [];
}

(** Add component *)
let with_component component state =
  { state with components = state.components @ [component] }

(** Add store *)
let with_store store state =
  { state with stores = state.stores @ [store] }

(** Add initial data *)
let with_data key value state =
  { state with initial_data = state.initial_data @ [(key, value)] }

(** Generate initialization script *)
let init_script state =
  let component_regs = state.components |> List.map Component.to_alpine_data in
  let store_regs = state.stores |> List.map Store.to_alpine_store in
  let data_assignments = state.initial_data |> List.map (fun (key, value) ->
    Printf.sprintf "window.__ALPINE_DATA__ = window.__ALPINE_DATA__ || {}; window.__ALPINE_DATA__['%s'] = %s"
      key (Yojson.Safe.to_string value)
  ) in
  let all = data_assignments @ component_regs @ store_regs in
  match all with
  | [] -> ""
  | _ ->
    Printf.sprintf {|<script>
document.addEventListener('alpine:init', () => {
  %s;
});
</script>|} (String.concat ";\n  " all)

(** {1 HTML Rendering} *)

(** Render element with Alpine directives *)
let render_element ~tag ~directives ?(attrs = []) ?(children = "") () =
  let directive_attrs = directives |> List.map Directive.to_attribute |> String.concat " " in
  let other_attrs = attrs |> List.map (fun (k, v) -> Printf.sprintf "%s=\"%s\"" k v) |> String.concat " " in
  let all_attrs = String.concat " " (List.filter (fun s -> s <> "") [directive_attrs; other_attrs]) in
  Printf.sprintf "<%s %s>%s</%s>" tag all_attrs children tag

(** Render with x-cloak for FOUC prevention *)
let render_cloaked ~tag ~directives ?attrs ?children () =
  let all_directives = Directive.cloak :: directives in
  render_element ~tag ~directives:all_directives ?attrs ?children ()

(** {1 Static Rendering} *)

(** Evaluate simple expression with data *)
let eval_expression ~data expr =
  (* Simple property access: "count" -> data["count"] *)
  match List.assoc_opt expr data with
  | Some (`String s) -> s
  | Some (`Int n) -> string_of_int n
  | Some (`Float f) -> Printf.sprintf "%.2f" f
  | Some (`Bool b) -> string_of_bool b
  | Some json -> Yojson.Safe.to_string json
  | None -> ""

(** Render x-text statically *)
let render_text_static ~data expr =
  eval_expression ~data expr

(** Render x-for statically *)
let render_for_static ~data ~item ~items ~template =
  match List.assoc_opt items data with
  | Some (`List arr) ->
    arr |> List.map (fun item_value ->
      let item_data = match item_value with
        | `Assoc pairs -> pairs
        | _ -> [(item, item_value)]
      in
      template item_data
    ) |> String.concat ""
  | _ -> ""

(** {1 Full Page Rendering} *)

(** Render full HTML page with Alpine *)
let render_page ~config ~state ~body =
  Printf.sprintf {|<!DOCTYPE html>
<html>
<head>
%s
%s
</head>
<body>
%s
</body>
</html>|} (head_scripts config) (init_script state) body

(** {1 Kirin Integration} *)

(** Create Kirin response with Alpine page *)
let response ~config ~state ~body =
  let html = render_page ~config ~state ~body in
  Kirin.Response.html html

(** Create handler with state from request *)
let handler ~config ~get_state ~render_body =
  fun req ->
    let state = get_state req in
    let body = render_body req state in
    response ~config ~state ~body

(** {1 Serialization} *)

(** Config to JSON *)
let config_to_json config =
  `Assoc [
    ("cdnUrl", `String config.cdn_url);
    ("version", `String config.version);
    ("includeCloakStyle", `Bool config.include_cloak_style);
    ("deferInit", `Bool config.defer_init);
    ("plugins", `List (List.map (fun p -> `String p) config.plugins));
  ]

(** State to JSON *)
let state_to_json state =
  `Assoc [
    ("components", `List (List.map Component.to_json state.components));
    ("stores", `List (List.map Store.to_json state.stores));
    ("initialData", `Assoc state.initial_data);
  ]
