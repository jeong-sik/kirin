(** Solid.js Hydration

    Server-side HTML shell for Solid.js hydration. *)

(** {1 Shell Configuration} *)

(** HTML shell options *)
type options = {
  title: string;
  lang: string;
  meta_tags: string;
  scripts: string list;
  styles: string list;
  initial_data: Data.initial_data;
  body_attrs: string;
  root_id: string;
  nonce: string option;  (* CSP nonce *)
}

(** Default options *)
let default_options = {
  title = "";
  lang = "en";
  meta_tags = "";
  scripts = [];
  styles = [];
  initial_data = Data.empty_initial_data;
  body_attrs = "";
  root_id = "app";
  nonce = None;
}

(** {1 Shell Rendering} *)

(** Generate script tag with optional nonce *)
let script_tag ?nonce src =
  match nonce with
  | Some n -> Printf.sprintf {|<script type="module" src="%s" nonce="%s"></script>|} src n
  | None -> Printf.sprintf {|<script type="module" src="%s"></script>|} src

(** Generate link tag for stylesheet *)
let style_tag href =
  Printf.sprintf {|<link rel="stylesheet" href="%s">|} href

(** Render HTML shell *)
let render ?(ssr_html = "") options =
  let nonce = options.nonce in
  let scripts_html = String.concat "\n    "
    (List.map (script_tag ?nonce) options.scripts) in
  let styles_html = String.concat "\n    "
    (List.map style_tag options.styles) in
  let data_scripts = Data.all_scripts options.initial_data in

  Printf.sprintf {|<!DOCTYPE html>
<html lang="%s">
<head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <title>%s</title>
    %s
    %s
</head>
<body%s>
    <div id="%s">%s</div>
    %s
    %s
</body>
</html>|}
    options.lang
    (Kirin.html_escape options.title)
    options.meta_tags
    styles_html
    (if options.body_attrs = "" then "" else " " ^ options.body_attrs)
    options.root_id
    ssr_html
    data_scripts
    scripts_html

(** {1 Quick Shell Builders} *)

(** Create shell for SPA (no SSR) *)
let spa ~title ~entry_script ?styles () =
  let options = {
    default_options with
    title;
    scripts = [entry_script];
    styles = Option.value ~default:[] styles;
  } in
  render options

(** Create shell with SSR content *)
let with_ssr ~title ~entry_script ~ssr_html ?styles ?initial_data () =
  let options = {
    default_options with
    title;
    scripts = [entry_script];
    styles = Option.value ~default:[] styles;
    initial_data = Option.value ~default:Data.empty_initial_data initial_data;
  } in
  render ~ssr_html options

(** {1 Vite Integration} *)

(** Create shell with Vite manifest *)
let with_vite ~title ~manifest ~entry ?dev_mode () =
  let scripts, styles = match dev_mode with
    | Some true ->
      (* Dev mode: use Vite dev server *)
      [Printf.sprintf "/@vite/client"; Printf.sprintf "/%s" entry], []
    | _ ->
      (* Production: use manifest *)
      match List.assoc_opt entry manifest with
      | Some info ->
        let file = match info with
          | `Assoc fields ->
            (match List.assoc_opt "file" fields with
             | Some (`String f) -> f
             | _ -> entry)
          | _ -> entry
        in
        let css = match info with
          | `Assoc fields ->
            (match List.assoc_opt "css" fields with
             | Some (`List css_files) ->
               List.filter_map (function `String s -> Some s | _ -> None) css_files
             | _ -> [])
          | _ -> []
        in
        ["/" ^ file], List.map (fun c -> "/" ^ c) css
      | None ->
        [entry], []
  in
  let options = {
    default_options with
    title;
    scripts;
    styles;
  } in
  render options

(** {1 Island Architecture} *)

(** Generate island wrapper *)
let island ~component_name ~props_json content =
  Printf.sprintf {|<solid-island data-component="%s" data-props='%s'>%s</solid-island>|}
    component_name
    (Data.serialize props_json)
    content

(** Generate lazy island (loads component on demand) *)
let lazy_island ~component_path ~props_json =
  Printf.sprintf {|<solid-island data-import="%s" data-props='%s'></solid-island>|}
    component_path
    (Data.serialize props_json)

(** {1 Streaming Placeholders} *)

(** Generate streaming placeholder *)
let streaming_placeholder ~id ~fallback =
  Printf.sprintf {|<template id="solid-stream-%s">%s</template>|} id fallback

(** Generate streaming replacement script *)
let streaming_replacement ~id content =
  Printf.sprintf {|<script>
(function(){
  var t=document.getElementById("solid-stream-%s");
  if(t){var r=document.createRange();r.setStartAfter(t);r.insertNode(r.createContextualFragment(%s));t.remove();}
})();
</script>|} id (Printf.sprintf "'%s'" (Data.serialize (`String content)))
