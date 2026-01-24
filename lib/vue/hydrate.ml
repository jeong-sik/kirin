(** Vue/Nuxt Hydration

    Nuxt-style HTML shell generation for SSR and hydration. *)

(** {1 Shell Options} *)

(** Hydration shell options *)
type options = {
  title: string;
  lang: string;
  dir: string;
  head_tags: string;
  body_attrs: string;
  scripts: string list;
  styles: string list;
  preload_hints: string;
  nonce: string option;
}

(** Default options *)
let default_options = {
  title = "";
  lang = "en";
  dir = "ltr";
  head_tags = "";
  body_attrs = "";
  scripts = [];
  styles = [];
  preload_hints = "";
  nonce = None;
}

(** {1 Shell Generation} *)

(** Generate SPA shell (client-side only) *)
let spa ~title ?(entry_script="/.nuxt/entry.js") () =
  Printf.sprintf {|<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1.0" />
  <title>%s</title>
</head>
<body>
  <div id="__nuxt"></div>
  <script type="module" src="%s"></script>
</body>
</html>|} title entry_script

(** Generate SSR shell with payload *)
let with_ssr ~title ~entry_script ~ssr_html ?(payload="") () =
  Printf.sprintf {|<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1.0" />
  <title>%s</title>
</head>
<body>
  <div id="__nuxt">%s</div>
  %s
  <script type="module" src="%s"></script>
</body>
</html>|} title ssr_html payload entry_script

(** Render full HTML with options *)
let render ~options ~ssr_html ~payload ~entry_script =
  let nonce_attr = match options.nonce with
    | Some n -> Printf.sprintf " nonce=\"%s\"" n
    | None -> ""
  in
  let preload = if options.preload_hints = "" then ""
    else options.preload_hints ^ "\n"
  in
  let styles = options.styles
    |> List.map (fun href ->
      Printf.sprintf {|  <link rel="stylesheet" href="%s" />|} href)
    |> String.concat "\n"
  in
  let head_scripts = options.scripts
    |> List.map (fun src ->
      Printf.sprintf {|  <script%s src="%s"></script>|} nonce_attr src)
    |> String.concat "\n"
  in
  Printf.sprintf {|<!DOCTYPE html>
<html lang="%s" dir="%s">
<head>
  <meta charset="UTF-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1.0" />
  <title>%s</title>
%s%s%s%s</head>
<body%s>
  <div id="__nuxt">%s</div>
%s  <script%s type="module" src="%s"></script>
</body>
</html>|}
    options.lang options.dir options.title
    preload options.head_tags styles head_scripts
    (if options.body_attrs = "" then "" else " " ^ options.body_attrs)
    ssr_html payload nonce_attr entry_script

(** {1 Nuxt Shell Helpers} *)

(** Generate Nuxt 3 shell *)
let nuxt3_shell ~title ~app_html ~payload ~entry_script =
  render
    ~options:{ default_options with title }
    ~ssr_html:app_html
    ~payload
    ~entry_script

(** Generate development shell *)
let dev_shell ~title ?(vite_port=3000) () =
  let entry = Printf.sprintf "http://localhost:%d/@vite/client" vite_port in
  Printf.sprintf {|<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1.0" />
  <title>%s - Dev</title>
  <script type="module" src="%s"></script>
</head>
<body>
  <div id="__nuxt">Loading...</div>
  <script type="module" src="http://localhost:%d/app.ts"></script>
</body>
</html>|} title entry vite_port

(** {1 Error Page} *)

(** Generate error page *)
let error_page ~status ~message =
  Printf.sprintf {|<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1.0" />
  <title>Error %d</title>
  <style>
    body { font-family: system-ui; display: flex; justify-content: center; align-items: center; min-height: 100vh; margin: 0; }
    .error { text-align: center; }
    h1 { font-size: 4rem; margin: 0; color: #666; }
    p { color: #999; }
  </style>
</head>
<body>
  <div class="error">
    <h1>%d</h1>
    <p>%s</p>
  </div>
</body>
</html>|} status status message

(** {1 Streaming Placeholders} *)

(** Generate streaming placeholder *)
let streaming_placeholder ~id =
  Printf.sprintf {|<template id="%s"><!--[--></template>|} id

(** Generate streaming replacement script *)
let streaming_replacement ~id ~html =
  Printf.sprintf {|<script>
(function(){
  var t = document.getElementById('%s');
  if(t) {
    var d = document.createElement('div');
    d.innerHTML = '%s';
    t.replaceWith(...d.childNodes);
  }
})();
</script>|}
    id
    (Str.global_replace (Str.regexp "'") "\\'" html)

(** {1 Island Hydration} *)

(** Hydration priority *)
type priority =
  | Eager      (* Hydrate immediately *)
  | Lazy       (* Hydrate on idle *)
  | Visible    (* Hydrate when visible *)
  | Interact   (* Hydrate on interaction *)

(** Priority to string *)
let priority_to_string = function
  | Eager -> "eager"
  | Lazy -> "lazy"
  | Visible -> "visible"
  | Interact -> "interact"

(** Generate island wrapper *)
let island_wrapper ~id ~component ~priority ~html =
  Printf.sprintf {|<div data-island="%s" data-component="%s" data-priority="%s">%s</div>|}
    id component (priority_to_string priority) html

(** {1 Teleport} *)

(** Generate teleport target *)
let teleport_target ~id =
  Printf.sprintf {|<div id="%s"></div>|} id

(** {1 Serialization} *)

(** Options to JSON *)
let options_to_json opts =
  `Assoc [
    ("title", `String opts.title);
    ("lang", `String opts.lang);
    ("dir", `String opts.dir);
    ("scripts", `List (List.map (fun s -> `String s) opts.scripts));
    ("styles", `List (List.map (fun s -> `String s) opts.styles));
  ]
