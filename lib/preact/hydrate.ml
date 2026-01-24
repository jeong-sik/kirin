(** Preact Hydration

    HTML shell generation for Preact hydration.
    Supports both preact/compat (React-style) and native Preact hydration. *)

(** {1 Hydration Types} *)

(** Hydration mode *)
type mode =
  | Native     (* Preact native hydrate *)
  | Compat     (* React-compatible hydrate via preact/compat *)

(** HTML shell options *)
type options = {
  title: string;
  meta: (string * string) list;
  scripts: string list;
  styles: string list;
  initial_data: Yojson.Safe.t option;
  root_id: string;
  body_class: string option;
  lang: string;
  mode: mode;
  signals_data: (string * Yojson.Safe.t) list;
}

(** Default options *)
let default_options = {
  title = "";
  meta = [];
  scripts = [];
  styles = [];
  initial_data = None;
  root_id = "app";
  body_class = None;
  lang = "en";
  mode = Native;
  signals_data = [];
}

(** {1 Data Serialization} *)

(** XSS-safe JSON serialization *)
let escape_json str =
  str
  |> Str.global_replace (Str.regexp_string "<") "\\u003c"
  |> Str.global_replace (Str.regexp_string ">") "\\u003e"
  |> Str.global_replace (Str.regexp_string "&") "\\u0026"
  |> Str.global_replace (Str.regexp_string "'") "\\u0027"

(** Serialize initial data *)
let serialize_data data =
  Yojson.Safe.to_string data |> escape_json

(** Generate initial data script *)
let data_script data =
  match data with
  | Some d ->
    "<script>window.__INITIAL_DATA__=" ^ (serialize_data d) ^ ";</script>"
  | None -> ""

(** Generate signals data script *)
let signals_script signals =
  if signals = [] then ""
  else
    let signals_json = `Assoc signals in
    "<script>window.__PREACT_SIGNALS__=" ^ (serialize_data signals_json) ^ ";</script>"

(** {1 Meta Tags} *)

(** Generate meta tag *)
let meta_tag (name, content) =
  if String.length name >= 3 && String.sub name 0 3 = "og:" then
    "<meta property=\"" ^ name ^ "\" content=\"" ^ content ^ "\">"
  else
    "<meta name=\"" ^ name ^ "\" content=\"" ^ content ^ "\">"

(** Generate all meta tags *)
let meta_tags metas =
  List.map meta_tag metas |> String.concat "\n    "

(** {1 Shell Generation} *)

(** Generate HTML shell *)
let render options =
  let meta_str = meta_tags options.meta in
  let styles_str = List.map (fun s ->
    "<link rel=\"stylesheet\" href=\"" ^ s ^ "\">"
  ) options.styles |> String.concat "\n    " in
  let scripts_str = List.map (fun s ->
    "<script type=\"module\" src=\"" ^ s ^ "\"></script>"
  ) options.scripts |> String.concat "\n    " in
  let data_str = data_script options.initial_data in
  let signals_str = signals_script options.signals_data in
  let body_class_attr = match options.body_class with
    | Some c -> " class=\"" ^ c ^ "\""
    | None -> ""
  in

  "<!DOCTYPE html>\n" ^
  "<html lang=\"" ^ options.lang ^ "\">\n" ^
  "<head>\n" ^
  "    <meta charset=\"UTF-8\">\n" ^
  "    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n" ^
  "    <title>" ^ options.title ^ "</title>\n" ^
  "    " ^ meta_str ^ "\n" ^
  "    " ^ styles_str ^ "\n" ^
  "</head>\n" ^
  "<body" ^ body_class_attr ^ ">\n" ^
  "    <div id=\"" ^ options.root_id ^ "\"></div>\n" ^
  "    " ^ data_str ^ "\n" ^
  "    " ^ signals_str ^ "\n" ^
  "    " ^ scripts_str ^ "\n" ^
  "</body>\n" ^
  "</html>"

(** Generate shell with manifest *)
let shell
    ~title
    ?(meta = [])
    ?(initial_data = None)
    ?(signals_data = [])
    ~manifest
    ~entry
    ?(mode = Native)
    () =
  let file = Manifest.resolve_file manifest entry |> Option.value ~default:"" in
  let css = Manifest.css_for manifest entry in
  let options = {
    default_options with
    title;
    meta;
    initial_data;
    signals_data;
    scripts = [file];
    styles = css;
    mode;
  } in
  render options

(** {1 Client Entry Generation} *)

(** Generate Preact client entry code *)
let client_entry_native ~component ~root_id =
  "import { h, hydrate } from 'preact';\n" ^
  "import App from '" ^ component ^ "';\n\n" ^
  "const initialData = window.__INITIAL_DATA__;\n" ^
  "const signalsData = window.__PREACT_SIGNALS__;\n\n" ^
  "hydrate(\n" ^
  "  h(App, { initialData, signalsData }),\n" ^
  "  document.getElementById('" ^ root_id ^ "')\n" ^
  ");\n"

(** Generate Preact compat client entry code *)
let client_entry_compat ~component ~root_id =
  "import { hydrateRoot } from 'preact/compat/client';\n" ^
  "import App from '" ^ component ^ "';\n\n" ^
  "const initialData = window.__INITIAL_DATA__;\n" ^
  "const signalsData = window.__PREACT_SIGNALS__;\n\n" ^
  "hydrateRoot(\n" ^
  "  document.getElementById('" ^ root_id ^ "'),\n" ^
  "  <App initialData={initialData} signalsData={signalsData} />\n" ^
  ");\n"

(** Generate client entry based on mode *)
let client_entry ~component ~root_id mode =
  match mode with
  | Native -> client_entry_native ~component ~root_id
  | Compat -> client_entry_compat ~component ~root_id

(** {1 Serialization} *)

(** Mode to string *)
let mode_to_string = function
  | Native -> "native"
  | Compat -> "compat"

(** Options to JSON *)
let options_to_json o =
  `Assoc [
    ("title", `String o.title);
    ("meta", `List (List.map (fun (n, c) ->
      `Assoc [("name", `String n); ("content", `String c)]
    ) o.meta));
    ("scripts", `List (List.map (fun s -> `String s) o.scripts));
    ("styles", `List (List.map (fun s -> `String s) o.styles));
    ("initialData", Option.value ~default:`Null o.initial_data);
    ("rootId", `String o.root_id);
    ("bodyClass", match o.body_class with Some c -> `String c | None -> `Null);
    ("lang", `String o.lang);
    ("mode", `String (mode_to_string o.mode));
    ("signalsData", `Assoc o.signals_data);
  ]
