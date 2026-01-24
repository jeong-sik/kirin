(** Preact Vite Manifest

    Parse and resolve Vite build manifest for Preact projects.
    Handles content-addressed asset URLs and dependency tracking. *)

(** {1 Manifest Types} *)

(** Manifest entry *)
type entry = {
  file: string;
  src: string;
  is_entry: bool;
  css: string list;
  assets: string list;
  dynamic_imports: string list;
  imports: string list;
}

(** Manifest *)
type t = {
  entries: (string * entry) list;
  base_url: string;
}

(** {1 Parsing} *)

(** Empty entry *)
let empty_entry = {
  file = "";
  src = "";
  is_entry = false;
  css = [];
  assets = [];
  dynamic_imports = [];
  imports = [];
}

(** Safe list extraction from JSON *)
let to_list_safe json =
  match json with
  | `List l -> l
  | _ -> []

(** Parse entry from JSON *)
let parse_entry json =
  let open Yojson.Safe.Util in
  let file = json |> member "file" |> to_string_option |> Option.value ~default:"" in
  let src = json |> member "src" |> to_string_option |> Option.value ~default:"" in
  let is_entry = json |> member "isEntry" |> to_bool_option |> Option.value ~default:false in
  let css = json |> member "css" |> to_list_safe
    |> List.filter_map to_string_option in
  let assets = json |> member "assets" |> to_list_safe
    |> List.filter_map to_string_option in
  let dynamic_imports = json |> member "dynamicImports" |> to_list_safe
    |> List.filter_map to_string_option in
  let imports = json |> member "imports" |> to_list_safe
    |> List.filter_map to_string_option in
  { file; src; is_entry; css; assets; dynamic_imports; imports }

(** Parse manifest from JSON *)
let parse ?(base_url = "/") json =
  match json with
  | `Assoc entries ->
    let parsed = List.map (fun (key, value) ->
      (key, parse_entry value)
    ) entries in
    { entries = parsed; base_url }
  | _ -> { entries = []; base_url }

(** Load manifest from file *)
let load ?(base_url = "/") path =
  try
    let content = In_channel.with_open_text path In_channel.input_all in
    let json = Yojson.Safe.from_string content in
    Ok (parse ~base_url json)
  with
  | Sys_error msg -> Error ("Failed to read manifest: " ^ msg)
  | Yojson.Json_error msg -> Error ("Failed to parse manifest: " ^ msg)

(** {1 Resolution} *)

(** Resolve entry by key *)
let resolve manifest key =
  List.assoc_opt key manifest.entries

(** Resolve file path *)
let resolve_file manifest key =
  match resolve manifest key with
  | Some entry -> Some (manifest.base_url ^ entry.file)
  | None -> None

(** Get CSS files for entry *)
let css_for manifest key =
  match resolve manifest key with
  | Some entry -> List.map (fun css -> manifest.base_url ^ css) entry.css
  | None -> []

(** Get all CSS files *)
let all_css manifest =
  List.concat_map (fun (_, entry) ->
    List.map (fun css -> manifest.base_url ^ css) entry.css
  ) manifest.entries

(** Get all entry points *)
let entry_points manifest =
  List.filter_map (fun (key, entry) ->
    if entry.is_entry then Some key else None
  ) manifest.entries

(** {1 Preload Hints} *)

(** Generate modulepreload link for entry *)
let preload_hint manifest key =
  match resolve manifest key with
  | Some entry ->
    Printf.sprintf {|<link rel="modulepreload" href="%s%s">|} manifest.base_url entry.file
  | None -> ""

(** Generate all preload hints for entry and its imports *)
let preload_hints manifest key =
  match resolve manifest key with
  | Some entry ->
    let main_hint = Printf.sprintf {|<link rel="modulepreload" href="%s%s">|}
      manifest.base_url entry.file in
    let import_hints = List.filter_map (fun import_key ->
      match resolve manifest import_key with
      | Some import_entry ->
        Some (Printf.sprintf {|<link rel="modulepreload" href="%s%s">|}
          manifest.base_url import_entry.file)
      | None -> None
    ) entry.imports in
    String.concat "\n" (main_hint :: import_hints)
  | None -> ""

(** {1 Script/Link Tags} *)

(** Generate script tag for entry *)
let script_tag manifest key =
  match resolve_file manifest key with
  | Some file -> Printf.sprintf {|<script type="module" src="%s"></script>|} file
  | None -> ""

(** Generate link tag for CSS *)
let css_link_tag url =
  Printf.sprintf {|<link rel="stylesheet" href="%s">|} url

(** Generate all CSS link tags for entry *)
let css_tags manifest key =
  css_for manifest key
  |> List.map css_link_tag
  |> String.concat "\n"

(** {1 Serialization} *)

(** Entry to JSON *)
let entry_to_json e =
  `Assoc [
    ("file", `String e.file);
    ("src", `String e.src);
    ("isEntry", `Bool e.is_entry);
    ("css", `List (List.map (fun c -> `String c) e.css));
    ("assets", `List (List.map (fun a -> `String a) e.assets));
    ("dynamicImports", `List (List.map (fun d -> `String d) e.dynamic_imports));
    ("imports", `List (List.map (fun i -> `String i) e.imports));
  ]

(** Manifest to JSON *)
let to_json manifest =
  `Assoc [
    ("baseUrl", `String manifest.base_url);
    ("entries", `Assoc (List.map (fun (k, v) -> (k, entry_to_json v)) manifest.entries));
  ]
