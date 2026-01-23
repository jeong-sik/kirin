(** HTML Shell Generation for Hydration

    Generates complete HTML documents with React hydration setup.
    Embeds initial data before scripts to ensure data is available.
*)

(** HTML shell options *)
type options = {
  title: string;
  lang: string;
  meta: (string * string) list;
  head_extra: string list;
  scripts: string list;
  styles: string list;
  initial_data: Yojson.Safe.t option;
  dehydrated_state: Yojson.Safe.t option;
  root_id: string;
  root_class: string option;
  body_class: string option;
  body_attrs: (string * string) list;
  nonce: string option;
}

let default_options = {
  title = "";
  lang = "en";
  meta = [];
  head_extra = [];
  scripts = [];
  styles = [];
  initial_data = None;
  dehydrated_state = None;
  root_id = "root";
  root_class = None;
  body_class = None;
  body_attrs = [];
  nonce = None;
}

(** Render HTML document *)
let render options =
  let buf = Buffer.create 4096 in
  let add s = Buffer.add_string buf s in
  let addln s = Buffer.add_string buf s; Buffer.add_char buf '\n' in

  (* DOCTYPE and html tag *)
  addln "<!DOCTYPE html>";
  add (Printf.sprintf {|<html lang="%s">|} (Meta.escape_html options.lang));
  addln "";

  (* Head section *)
  addln "<head>";
  addln (Meta.charset ());
  addln (Meta.viewport ());

  (* Title *)
  if options.title <> "" then
    addln (Meta.title options.title);

  (* Meta tags from pairs *)
  if options.meta <> [] then
    addln (Meta.render_pairs options.meta);

  (* Stylesheets *)
  List.iter (fun href ->
    addln (Printf.sprintf {|<link rel="stylesheet" href="%s">|} (Meta.escape_html href))
  ) options.styles;

  (* Extra head content *)
  List.iter addln options.head_extra;

  addln "</head>";

  (* Body section *)
  let body_class_attr = match options.body_class with
    | Some c -> Printf.sprintf {| class="%s"|} (Meta.escape_html c)
    | None -> ""
  in
  let body_extra_attrs =
    options.body_attrs
    |> List.map (fun (k, v) -> Printf.sprintf {| %s="%s"|} k (Meta.escape_html v))
    |> String.concat ""
  in
  add (Printf.sprintf "<body%s%s>" body_class_attr body_extra_attrs);
  addln "";

  (* Root div *)
  let root_class_attr = match options.root_class with
    | Some c -> Printf.sprintf {| class="%s"|} (Meta.escape_html c)
    | None -> ""
  in
  addln (Printf.sprintf {|<div id="%s"%s></div>|}
    (Meta.escape_html options.root_id) root_class_attr);

  (* Initial data script - BEFORE React scripts *)
  Option.iter (fun data ->
    match options.nonce with
    | Some nonce -> addln (Data.script_tag_with_nonce ~nonce data)
    | None -> addln (Data.script_tag data)
  ) options.initial_data;

  (* Dehydrated state for React Query *)
  Option.iter (fun state ->
    addln (Data.script_tag ~var_name:"__DEHYDRATED_STATE__" state)
  ) options.dehydrated_state;

  (* Scripts *)
  List.iter (fun src ->
    let nonce_attr = match options.nonce with
      | Some n -> Printf.sprintf {| nonce="%s"|} (Meta.escape_html n)
      | None -> ""
    in
    addln (Printf.sprintf {|<script type="module" src="%s"%s></script>|}
      (Meta.escape_html src) nonce_attr)
  ) options.scripts;

  addln "</body>";
  addln "</html>";

  Buffer.contents buf

(** Convenience function for simple hydration shell *)
let shell
    ~title
    ?(lang = "en")
    ?(meta = [])
    ?(initial_data = None)
    ~manifest
    ~entry
    () =
  let styles = Manifest.css_for manifest entry in
  let script = match Manifest.resolve manifest entry with
    | Some file -> [file]
    | None -> [entry]
  in
  render {
    default_options with
    title;
    lang;
    meta;
    styles;
    scripts = script;
    initial_data;
  }

(** Generate response from options *)
let response options =
  let html = render options in
  Kirin.Response.html html

(** Generate response from simple parameters *)
let simple_response
    ~title
    ?(meta = [])
    ?(initial_data = None)
    ~manifest
    ~entry
    () =
  let html = shell ~title ~meta ~initial_data ~manifest ~entry () in
  Kirin.Response.html html

(** Builder pattern for options *)
module Builder = struct
  type t = options

  let create () = default_options

  let title t b = { b with title = t }
  let lang l b = { b with lang = l }
  let meta pairs b = { b with meta = pairs }
  let add_meta ~name ~content b =
    { b with meta = (name, content) :: b.meta }
  let head_extra extra b = { b with head_extra = extra }
  let add_head h b = { b with head_extra = h :: b.head_extra }
  let scripts s b = { b with scripts = s }
  let add_script s b = { b with scripts = s :: b.scripts }
  let styles s b = { b with styles = s }
  let add_style s b = { b with styles = s :: b.styles }
  let initial_data d b = { b with initial_data = Some d }
  let dehydrated_state s b = { b with dehydrated_state = Some s }
  let root_id id b = { b with root_id = id }
  let root_class c b = { b with root_class = Some c }
  let body_class c b = { b with body_class = Some c }
  let body_attr ~name ~value b =
    { b with body_attrs = (name, value) :: b.body_attrs }
  let nonce n b = { b with nonce = Some n }

  let build b = b
  let render b = render b
  let response b = response b
end

(** With manifest helper for setting up scripts/styles automatically *)
let with_manifest ~manifest ~entry options =
  let styles = Manifest.css_for manifest entry in
  let script = match Manifest.resolve manifest entry with
    | Some file -> [file]
    | None -> [entry]
  in
  { options with
    styles = options.styles @ styles;
    scripts = options.scripts @ script;
  }

(** Add preload hints to head *)
let with_preloads ~manifest ~entry options =
  let preloads = Manifest.preload_hints manifest entry in
  { options with head_extra = preloads :: options.head_extra }

(** SSR placeholder for streaming *)
let streaming_placeholder ?(id = "ssr-outlet") () =
  Printf.sprintf {|<!--$%s--><!--/$%s-->|} id id

(** Suspense boundary placeholder *)
let suspense_placeholder ~id ~fallback () =
  Printf.sprintf {|<template id="S:%s"></template>%s<!--/$-->|} id fallback
