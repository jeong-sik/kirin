(** Asset URL Resolution

    Resolves asset URLs using Vite manifest for content-addressed
    filenames with cache busting support.
*)

(** Generate URL for an asset from manifest *)
let url ?(base_url = "") ~manifest src =
  match Manifest.resolve manifest src with
  | Some file ->
    if base_url = "" then "/" ^ file
    else base_url ^ "/" ^ file
  | None ->
    (* Fallback to original path if not in manifest *)
    if base_url = "" then "/" ^ src
    else base_url ^ "/" ^ src

(** Generate script tag for a JS entry *)
let script_tag ?(base_url = "") ?(module_ = true) ~manifest src =
  let href = url ~base_url ~manifest src in
  if module_ then
    Printf.sprintf {|<script type="module" src="%s"></script>|} href
  else
    Printf.sprintf {|<script src="%s"></script>|} href

(** Generate link tag for a CSS file *)
let link_tag ?(base_url = "") ~manifest src =
  let href = url ~base_url ~manifest src in
  Printf.sprintf {|<link rel="stylesheet" href="%s">|} href

(** Generate all CSS link tags for an entry *)
let css_tags ?(base_url = "") ~manifest src =
  Manifest.css_for manifest src
  |> List.map (fun css_file ->
    let href = if base_url = "" then "/" ^ css_file else base_url ^ "/" ^ css_file in
    Printf.sprintf {|<link rel="stylesheet" href="%s">|} href
  )
  |> String.concat "\n"

(** Generate modulepreload link for JS *)
let modulepreload_tag ?(base_url = "") href =
  let full_href = if base_url = "" then "/" ^ href else base_url ^ "/" ^ href in
  Printf.sprintf {|<link rel="modulepreload" href="%s">|} full_href

(** Generate preload link for various asset types *)
let preload_tag ?(base_url = "") ?(crossorigin = false) ~as_type href =
  let full_href = if base_url = "" then "/" ^ href else base_url ^ "/" ^ href in
  let crossorigin_attr = if crossorigin then " crossorigin" else "" in
  Printf.sprintf {|<link rel="preload" href="%s" as="%s"%s>|}
    full_href as_type crossorigin_attr

(** Detect asset type from file extension *)
let detect_asset_type filename =
  match Filename.extension filename |> String.lowercase_ascii with
  | ".js" | ".mjs" -> "script"
  | ".css" -> "style"
  | ".woff" | ".woff2" -> "font"
  | ".ttf" | ".otf" -> "font"
  | ".png" | ".jpg" | ".jpeg" | ".gif" | ".webp" | ".avif" -> "image"
  | ".svg" -> "image"
  | _ -> "fetch"

(** Generate all preload tags for an entry's dependencies *)
let preload_all ?(base_url = "") ~manifest src =
  let entry = Manifest.find manifest src in
  match entry with
  | None -> ""
  | Some e ->
    let tags = ref [] in
    (* Preload main script *)
    tags := modulepreload_tag ~base_url e.file :: !tags;
    (* Preload CSS *)
    List.iter (fun css ->
      tags := preload_tag ~base_url ~as_type:"style" css :: !tags
    ) e.css;
    (* Preload imported modules *)
    List.iter (fun imp ->
      match Manifest.resolve manifest imp with
      | Some file -> tags := modulepreload_tag ~base_url file :: !tags
      | None -> ()
    ) e.imports;
    List.rev !tags |> String.concat "\n"

(** Generate complete head tags for a React entry point *)
let head_tags ?(base_url = "") ~manifest ~entry () =
  let preloads = preload_all ~base_url ~manifest entry in
  let css = css_tags ~base_url ~manifest entry in
  if preloads = "" && css = "" then ""
  else if preloads = "" then css
  else if css = "" then preloads
  else preloads ^ "\n" ^ css

(** Generate complete body tags (scripts at end of body) *)
let body_tags ?(base_url = "") ~manifest ~entry () =
  script_tag ~base_url ~manifest entry

(** Asset configuration for building URLs *)
type config = {
  base_url: string;
  manifest: Manifest.t;
}

let make_config ?(base_url = "") manifest =
  { base_url; manifest }

(** Helper functions using config *)
let resolve_url config src =
  url ~base_url:config.base_url ~manifest:config.manifest src

let script config src =
  script_tag ~base_url:config.base_url ~manifest:config.manifest src

let stylesheet config src =
  link_tag ~base_url:config.base_url ~manifest:config.manifest src

let all_styles config src =
  css_tags ~base_url:config.base_url ~manifest:config.manifest src

let all_preloads config src =
  preload_all ~base_url:config.base_url ~manifest:config.manifest src

(** Inline critical CSS (reads file content) *)
let inline_css ?(dir = "dist") css_path =
  let full_path = Filename.concat dir css_path in
  try
    let content = In_channel.(with_open_text full_path input_all) in
    Printf.sprintf "<style>%s</style>" content
  with Sys_error _ ->
    Printf.sprintf {|<link rel="stylesheet" href="/%s">|} css_path

(** Generate integrity hash for SRI (placeholder - would need crypto) *)
let integrity_hash _content =
  (* In production, this would compute SHA-384/SHA-512 hash *)
  None

(** Generate script tag with integrity *)
let script_tag_sri ?(base_url = "") ~manifest src =
  let href = url ~base_url ~manifest src in
  (* Note: integrity would be computed from file content *)
  Printf.sprintf {|<script type="module" src="%s" crossorigin="anonymous"></script>|} href
