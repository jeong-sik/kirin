(** Angular Meta Tags

    Angular Meta service helpers for SSR. *)

(** {1 Meta Types} *)

(** Meta tag definition *)
type meta_tag =
  | Name of { name: string; content: string }
  | Property of { property: string; content: string }
  | HttpEquiv of { http_equiv: string; content: string }
  | Charset of string

(** Link tag definition *)
type link_tag = {
  rel: string;
  href: string;
  hreflang: string option;
  type_: string option;
  sizes: string option;
  media: string option;
}

(** Script tag definition *)
type script_tag = {
  src: string option;
  content: string option;
  type_: string option;
  defer: bool;
  async_: bool;
  module_: bool;
}

(** Head configuration *)
type head = {
  title: string option;
  base_href: string option;
  metas: meta_tag list;
  links: link_tag list;
  scripts: script_tag list;
}

(** {1 Head Building} *)

(** Empty head *)
let empty = {
  title = None;
  base_href = None;
  metas = [];
  links = [];
  scripts = [];
}

(** Set title *)
let with_title title head =
  { head with title = Some title }

(** Set base href *)
let with_base href head =
  { head with base_href = Some href }

(** Add meta by name *)
let with_meta ~name ~content head =
  { head with metas = Name { name; content } :: head.metas }

(** Add meta property (Open Graph) *)
let with_property ~property ~content head =
  { head with metas = Property { property; content } :: head.metas }

(** Add charset *)
let with_charset charset head =
  { head with metas = Charset charset :: head.metas }

(** Add http-equiv *)
let with_http_equiv ~http_equiv ~content head =
  { head with metas = HttpEquiv { http_equiv; content } :: head.metas }

(** Add link *)
let with_link ~rel ~href ?hreflang ?type_ ?sizes ?media () head =
  let link = { rel; href; hreflang; type_; sizes; media } in
  { head with links = link :: head.links }

(** Add external script *)
let with_script ~src ?type_ ?(defer=false) ?(async_=false) ?(module_=false) () head =
  let script = { src = Some src; content = None; type_; defer; async_; module_ } in
  { head with scripts = script :: head.scripts }

(** Add inline script *)
let with_inline_script content head =
  let script = { src = None; content = Some content; type_ = None; defer = false; async_ = false; module_ = false } in
  { head with scripts = script :: head.scripts }

(** {1 Convenience} *)

(** Create SEO head *)
let seo ~title ?description ?canonical ?og_title ?og_description ?og_image ?og_url () =
  let head = empty |> with_title title |> with_charset "UTF-8" in
  let head = match description with
    | Some d -> head |> with_meta ~name:"description" ~content:d
    | None -> head
  in
  let head = match canonical with
    | Some url -> head |> with_link ~rel:"canonical" ~href:url ()
    | None -> head
  in
  let head = match og_title with
    | Some t -> head |> with_property ~property:"og:title" ~content:t
    | None -> head
  in
  let head = match og_description with
    | Some d -> head |> with_property ~property:"og:description" ~content:d
    | None -> head
  in
  let head = match og_image with
    | Some img -> head |> with_property ~property:"og:image" ~content:img
    | None -> head
  in
  let head = match og_url with
    | Some url -> head |> with_property ~property:"og:url" ~content:url
    | None -> head
  in
  head

(** {1 Rendering} *)

(** Render meta tag *)
let render_meta = function
  | Name { name; content } ->
    Printf.sprintf {|<meta name="%s" content="%s">|} name content
  | Property { property; content } ->
    Printf.sprintf {|<meta property="%s" content="%s">|} property content
  | HttpEquiv { http_equiv; content } ->
    Printf.sprintf {|<meta http-equiv="%s" content="%s">|} http_equiv content
  | Charset charset ->
    Printf.sprintf {|<meta charset="%s">|} charset

(** Render link tag *)
let render_link link =
  let attrs = [
    Printf.sprintf {|rel="%s"|} link.rel;
    Printf.sprintf {|href="%s"|} link.href;
  ] @ (match link.hreflang with Some h -> [Printf.sprintf {|hreflang="%s"|} h] | None -> [])
    @ (match link.type_ with Some t -> [Printf.sprintf {|type="%s"|} t] | None -> [])
    @ (match link.sizes with Some s -> [Printf.sprintf {|sizes="%s"|} s] | None -> [])
    @ (match link.media with Some m -> [Printf.sprintf {|media="%s"|} m] | None -> [])
  in
  Printf.sprintf "<link %s>" (String.concat " " attrs)

(** Render script tag *)
let render_script script =
  let attrs = []
    @ (match script.src with Some s -> [Printf.sprintf {|src="%s"|} s] | None -> [])
    @ (match script.type_ with Some t -> [Printf.sprintf {|type="%s"|} t] | None -> [])
    @ (if script.defer then ["defer"] else [])
    @ (if script.async_ then ["async"] else [])
    @ (if script.module_ then [{|type="module"|}] else [])
  in
  match script.content with
  | Some content ->
    Printf.sprintf "<script%s>%s</script>"
      (if attrs = [] then "" else " " ^ String.concat " " attrs)
      content
  | None ->
    Printf.sprintf "<script %s></script>" (String.concat " " attrs)

(** Render all head content *)
let render head =
  let parts = [] in
  let parts = match head.title with
    | Some t -> Printf.sprintf "<title>%s</title>" t :: parts
    | None -> parts
  in
  let parts = match head.base_href with
    | Some b -> Printf.sprintf {|<base href="%s">|} b :: parts
    | None -> parts
  in
  let parts = List.rev_map render_meta head.metas @ parts in
  let parts = List.rev_map render_link head.links @ parts in
  let parts = List.rev_map render_script head.scripts @ parts in
  String.concat "\n" (List.rev parts)

(** {1 Serialization} *)

(** Meta to JSON *)
let meta_to_json = function
  | Name { name; content } ->
    `Assoc [("name", `String name); ("content", `String content)]
  | Property { property; content } ->
    `Assoc [("property", `String property); ("content", `String content)]
  | HttpEquiv { http_equiv; content } ->
    `Assoc [("httpEquiv", `String http_equiv); ("content", `String content)]
  | Charset charset ->
    `Assoc [("charset", `String charset)]

(** Head to JSON *)
let to_json head =
  `Assoc [
    ("title", match head.title with Some t -> `String t | None -> `Null);
    ("baseHref", match head.base_href with Some b -> `String b | None -> `Null);
    ("metas", `List (List.map meta_to_json head.metas));
  ]
