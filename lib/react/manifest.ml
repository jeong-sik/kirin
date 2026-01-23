(** Vite Manifest Parsing

    Parses Vite's manifest.json for content-addressed asset URLs.
    Supports CSS/JS dependency tracking and dynamic import preloading.

    Manifest format (Vite 4+):
    {
      "index.html": {
        "file": "assets/index-abc123.js",
        "isEntry": true,
        "css": ["assets/index-def456.css"],
        "assets": ["assets/logo-xyz789.png"],
        "dynamicImports": ["src/pages/lazy.ts"]
      }
    }
*)

type entry = {
  file: string;
  src: string;
  is_entry: bool;
  css: string list;
  assets: string list;
  dynamic_imports: string list;
  imports: string list;
}

type t = (string * entry) list

let empty_entry src = {
  file = "";
  src;
  is_entry = false;
  css = [];
  assets = [];
  dynamic_imports = [];
  imports = [];
}

let parse_entry ~src json =
  match json with
  | `Assoc fields ->
    let get_string key =
      match List.assoc_opt key fields with
      | Some (`String s) -> s
      | _ -> ""
    in
    let get_bool key =
      match List.assoc_opt key fields with
      | Some (`Bool b) -> b
      | _ -> false
    in
    let get_string_list key =
      match List.assoc_opt key fields with
      | Some (`List items) ->
        List.filter_map (function `String s -> Some s | _ -> None) items
      | _ -> []
    in
    {
      file = get_string "file";
      src;
      is_entry = get_bool "isEntry";
      css = get_string_list "css";
      assets = get_string_list "assets";
      dynamic_imports = get_string_list "dynamicImports";
      imports = get_string_list "imports";
    }
  | _ -> empty_entry src

let parse json : t =
  match json with
  | `Assoc entries ->
    List.map (fun (src, entry_json) ->
      (src, parse_entry ~src entry_json)
    ) entries
  | _ -> []

let parse_string s =
  try
    let json = Yojson.Safe.from_string s in
    Ok (parse json)
  with
  | Yojson.Json_error msg -> Error ("JSON parse error: " ^ msg)

let load path =
  try
    let content = In_channel.(with_open_text path input_all) in
    parse_string content
  with
  | Sys_error msg -> Error ("File error: " ^ msg)

let find (manifest : t) src =
  List.assoc_opt src manifest

let resolve manifest src =
  match find manifest src with
  | Some entry -> Some entry.file
  | None -> None

let css_for manifest src =
  match find manifest src with
  | Some entry -> entry.css
  | None -> []

let assets_for manifest src =
  match find manifest src with
  | Some entry -> entry.assets
  | None -> []

let imports_for manifest src =
  match find manifest src with
  | Some entry -> entry.imports
  | None -> []

let dynamic_imports_for manifest src =
  match find manifest src with
  | Some entry -> entry.dynamic_imports
  | None -> []

let entries manifest =
  List.filter (fun (_, entry) -> entry.is_entry) manifest

let all_css manifest =
  manifest
  |> List.concat_map (fun (_, entry) -> entry.css)
  |> List.sort_uniq String.compare

let all_assets manifest =
  manifest
  |> List.concat_map (fun (_, entry) -> entry.assets)
  |> List.sort_uniq String.compare

(** Generate preload hints for an entry and its dependencies *)
let preload_hints ?(base_url = "") manifest src =
  let rec collect_imports visited src_list =
    List.fold_left (fun (visited, files) s ->
      if List.mem s visited then (visited, files)
      else
        match find manifest s with
        | Some entry ->
          let visited' = s :: visited in
          let files' = entry.file :: files in
          collect_imports visited' entry.imports
          |> fun (v, f) -> (v, files' @ f)
        | None -> (visited, files)
    ) (visited, []) src_list
  in
  let _, files = collect_imports [] [src] in
  files
  |> List.sort_uniq String.compare
  |> List.map (fun file ->
    let url = if base_url = "" then "/" ^ file else base_url ^ "/" ^ file in
    let ext = Filename.extension file in
    let as_type = match ext with
      | ".js" | ".mjs" -> "script"
      | ".css" -> "style"
      | ".woff" | ".woff2" | ".ttf" | ".otf" -> "font"
      | ".png" | ".jpg" | ".jpeg" | ".gif" | ".svg" | ".webp" -> "image"
      | _ -> "fetch"
    in
    Printf.sprintf {|<link rel="modulepreload" href="%s" as="%s">|} url as_type
  )
  |> String.concat "\n"

(** Get the hashed filename for an asset, stripping any leading paths *)
let hashed_filename manifest src =
  match resolve manifest src with
  | Some file -> Some (Filename.basename file)
  | None -> None

(** Check if manifest contains an entry *)
let mem manifest src =
  List.mem_assoc src manifest

(** Get all source files in manifest *)
let sources manifest =
  List.map fst manifest

(** Pretty print manifest for debugging *)
let pp fmt manifest =
  Format.fprintf fmt "Manifest {\n";
  List.iter (fun (src, entry) ->
    Format.fprintf fmt "  %s -> %s%s\n"
      src entry.file
      (if entry.is_entry then " [entry]" else "");
    if entry.css <> [] then
      Format.fprintf fmt "    css: [%s]\n" (String.concat ", " entry.css);
    if entry.imports <> [] then
      Format.fprintf fmt "    imports: [%s]\n" (String.concat ", " entry.imports);
  ) manifest;
  Format.fprintf fmt "}"

let to_string manifest =
  Format.asprintf "%a" pp manifest
