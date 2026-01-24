(** Vue/Nuxt Meta Tags

    Nuxt-style head management with useHead and useSeoMeta. *)

(** {1 Meta Tag Types} *)

(** Link tag *)
type link = {
  rel: string;
  href: string;
  hreflang: string option;
  type_: string option;
  sizes: string option;
  media: string option;
  crossorigin: string option;
}

(** Script tag *)
type script = {
  src: string option;
  content: string option;
  type_: string option;
  async: bool;
  defer: bool;
  crossorigin: string option;
}

(** Meta tag *)
type meta = {
  name: string option;
  property: string option;
  content: string;
  http_equiv: string option;
  charset: string option;
}

(** {1 Head Definition} *)

(** Complete head definition (useHead) *)
type head = {
  title: string option;
  title_template: string option;  (* e.g., "%s - My Site" *)
  base: string option;
  links: link list;
  metas: meta list;
  scripts: script list;
  styles: string list;
  html_attrs: (string * string) list;
  body_attrs: (string * string) list;
  noscript: string list;
}

(** Empty head *)
let empty = {
  title = None;
  title_template = None;
  base = None;
  links = [];
  metas = [];
  scripts = [];
  styles = [];
  html_attrs = [];
  body_attrs = [];
  noscript = [];
}

(** {1 Head Builder} *)

(** Set title *)
let with_title title head =
  { head with title = Some title }

(** Set title template *)
let with_title_template template head =
  { head with title_template = Some template }

(** Add meta tag *)
let with_meta ~name ~content head =
  let meta = { name = Some name; property = None; content; http_equiv = None; charset = None } in
  { head with metas = meta :: head.metas }

(** Add property meta tag (OG/Twitter) *)
let with_property ~property ~content head =
  let meta = { name = None; property = Some property; content; http_equiv = None; charset = None } in
  { head with metas = meta :: head.metas }

(** Add link tag *)
let with_link ~rel ~href ?type_ ?sizes head =
  let link = { rel; href; hreflang = None; type_; sizes; media = None; crossorigin = None } in
  { head with links = link :: head.links }

(** Add script tag *)
let with_script ~src ?(async=false) ?(defer=false) head =
  let script = { src = Some src; content = None; type_ = None; async; defer; crossorigin = None } in
  { head with scripts = script :: head.scripts }

(** Add inline script *)
let with_inline_script content head =
  let script = { src = None; content = Some content; type_ = None; async = false; defer = false; crossorigin = None } in
  { head with scripts = script :: head.scripts }

(** Add style *)
let with_style css head =
  { head with styles = css :: head.styles }

(** Set HTML attribute *)
let with_html_attr ~name ~value head =
  { head with html_attrs = (name, value) :: head.html_attrs }

(** Set body attribute *)
let with_body_attr ~name ~value head =
  { head with body_attrs = (name, value) :: head.body_attrs }

(** {1 SEO Meta (useSeoMeta)} *)

(** SEO meta configuration *)
type seo_meta = {
  title: string option;
  og_title: string option;
  description: string option;
  og_description: string option;
  og_image: string option;
  og_url: string option;
  og_type: string option;
  og_site_name: string option;
  og_locale: string option;
  twitter_card: string option;
  twitter_site: string option;
  twitter_creator: string option;
  twitter_title: string option;
  twitter_description: string option;
  twitter_image: string option;
  robots: string option;
  canonical: string option;
}

(** Empty SEO meta *)
let empty_seo = {
  title = None;
  og_title = None;
  description = None;
  og_description = None;
  og_image = None;
  og_url = None;
  og_type = None;
  og_site_name = None;
  og_locale = None;
  twitter_card = None;
  twitter_site = None;
  twitter_creator = None;
  twitter_title = None;
  twitter_description = None;
  twitter_image = None;
  robots = None;
  canonical = None;
}

(** Create SEO meta with common fields *)
let seo ~title ?description ?image ?url () = {
  empty_seo with
  title = Some title;
  og_title = Some title;
  description;
  og_description = description;
  og_image = image;
  og_url = url;
}

(** {1 Rendering} *)

(** Render meta tag to HTML *)
let render_meta m =
  match m.name, m.property, m.charset with
  | Some n, _, _ ->
    Printf.sprintf {|<meta name="%s" content="%s" />|} n m.content
  | _, Some p, _ ->
    Printf.sprintf {|<meta property="%s" content="%s" />|} p m.content
  | _, _, Some c ->
    Printf.sprintf {|<meta charset="%s" />|} c
  | _ -> ""

(** Render link tag to HTML *)
let render_link (l : link) =
  let type_attr = match l.type_ with
    | Some t -> Printf.sprintf " type=\"%s\"" t
    | None -> ""
  in
  let sizes_attr = match l.sizes with
    | Some s -> Printf.sprintf " sizes=\"%s\"" s
    | None -> ""
  in
  Printf.sprintf {|<link rel="%s" href="%s"%s%s />|}
    l.rel l.href type_attr sizes_attr

(** Render script tag to HTML *)
let render_script s =
  let async_attr = if s.async then " async" else "" in
  let defer_attr = if s.defer then " defer" else "" in
  match s.src, s.content with
  | Some src, _ ->
    Printf.sprintf {|<script src="%s"%s%s></script>|}
      src async_attr defer_attr
  | None, Some content ->
    Printf.sprintf {|<script>%s</script>|} content
  | None, None -> ""

(** Render complete head to HTML *)
let render (head : head) =
  let title = match head.title, head.title_template with
    | Some t, Some template ->
      Printf.sprintf "<title>%s</title>\n"
        (Str.global_replace (Str.regexp "%s") t template)
    | Some t, None ->
      Printf.sprintf "<title>%s</title>\n" t
    | None, _ -> ""
  in
  let metas = head.metas |> List.map render_meta |> String.concat "\n" in
  let links = head.links |> List.map render_link |> String.concat "\n" in
  let scripts = head.scripts |> List.map render_script |> String.concat "\n" in
  let styles = head.styles |> List.map (fun css ->
    Printf.sprintf "<style>%s</style>" css
  ) |> String.concat "\n" in
  String.concat "\n" [title; metas; links; styles; scripts]

(** Render SEO meta to head *)
let seo_to_head seo =
  let add_meta name value head =
    match value with
    | Some v -> with_meta ~name ~content:v head
    | None -> head
  in
  let add_prop prop value head =
    match value with
    | Some v -> with_property ~property:prop ~content:v head
    | None -> head
  in
  empty
  |> (fun h -> match seo.title with Some t -> with_title t h | None -> h)
  |> add_meta "description" seo.description
  |> add_meta "robots" seo.robots
  |> add_prop "og:title" seo.og_title
  |> add_prop "og:description" seo.og_description
  |> add_prop "og:image" seo.og_image
  |> add_prop "og:url" seo.og_url
  |> add_prop "og:type" seo.og_type
  |> add_prop "og:site_name" seo.og_site_name
  |> add_prop "twitter:card" seo.twitter_card
  |> add_prop "twitter:site" seo.twitter_site
  |> add_prop "twitter:title" seo.twitter_title
  |> add_prop "twitter:description" seo.twitter_description
  |> add_prop "twitter:image" seo.twitter_image
  |> (fun h -> match seo.canonical with
      | Some url -> with_link ~rel:"canonical" ~href:url h
      | None -> h)

(** {1 Serialization} *)

(** Meta to JSON *)
let meta_to_json m =
  let fields = [("content", `String m.content)] in
  let with_name = match m.name with
    | Some n -> ("name", `String n) :: fields
    | None -> fields
  in
  let with_prop = match m.property with
    | Some p -> ("property", `String p) :: with_name
    | None -> with_name
  in
  `Assoc with_prop

(** Head to JSON *)
let head_to_json (head : head) =
  `Assoc [
    ("title", match head.title with Some t -> `String t | None -> `Null);
    ("titleTemplate", match head.title_template with Some t -> `String t | None -> `Null);
    ("meta", `List (List.map meta_to_json head.metas));
    ("link", `List (List.map (fun l -> `Assoc [
      ("rel", `String l.rel);
      ("href", `String l.href);
    ]) head.links));
  ]

(** SEO meta to JSON *)
let seo_to_json seo =
  let opt_str key = function
    | Some v -> [(key, `String v)]
    | None -> []
  in
  `Assoc (
    opt_str "title" seo.title @
    opt_str "description" seo.description @
    opt_str "ogTitle" seo.og_title @
    opt_str "ogDescription" seo.og_description @
    opt_str "ogImage" seo.og_image @
    opt_str "ogUrl" seo.og_url @
    opt_str "twitterCard" seo.twitter_card @
    opt_str "canonical" seo.canonical
  )
