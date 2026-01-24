(** Qwik Meta Tags

    Head management for QwikCity. *)

(** {1 Meta Types} *)

(** Document head configuration *)
type t = {
  title: string option;
  metas: meta list;
  links: link list;
  scripts: script list;
  styles: string list;
}

and meta = {
  meta_name: string option;
  meta_property: string option;
  meta_content: string;
  meta_http_equiv: string option;
}

and link = {
  link_rel: string;
  link_href: string;
  link_type: string option;
  link_sizes: string option;
  link_as: string option;
  link_crossorigin: bool;
}

and script = {
  script_src: string option;
  script_content: string option;
  script_type: string option;
  script_async: bool;
  script_defer: bool;
}

(** {1 Head Construction} *)

(** Empty head *)
let empty = {
  title = None;
  metas = [];
  links = [];
  scripts = [];
  styles = [];
}

(** Set title *)
let with_title title head =
  { head with title = Some title }

(** Add meta by name *)
let with_meta ~name ~content head =
  let meta = {
    meta_name = Some name;
    meta_property = None;
    meta_content = content;
    meta_http_equiv = None;
  } in
  { head with metas = meta :: head.metas }

(** Add meta by property *)
let with_property ~property ~content head =
  let meta = {
    meta_name = None;
    meta_property = Some property;
    meta_content = content;
    meta_http_equiv = None;
  } in
  { head with metas = meta :: head.metas }

(** Add link *)
let with_link ~rel ~href ?type_ ?sizes ?as_ ?(crossorigin=false) () head =
  let link = {
    link_rel = rel;
    link_href = href;
    link_type = type_;
    link_sizes = sizes;
    link_as = as_;
    link_crossorigin = crossorigin;
  } in
  { head with links = link :: head.links }

(** Add script *)
let with_script ?src ?content ?type_ ?(async_=false) ?(defer=false) () head =
  let script = {
    script_src = src;
    script_content = content;
    script_type = type_;
    script_async = async_;
    script_defer = defer;
  } in
  { head with scripts = script :: head.scripts }

(** Add inline style *)
let with_style css head =
  { head with styles = css :: head.styles }

(** {1 SEO Helpers} *)

(** Create SEO-optimized head *)
let seo ~title ?description ?canonical ?og_image () =
  let head = empty |> with_title title in
  let head = match description with
    | Some d -> head
      |> with_meta ~name:"description" ~content:d
      |> with_property ~property:"og:description" ~content:d
    | None -> head
  in
  let head = head
    |> with_property ~property:"og:title" ~content:title
    |> with_property ~property:"og:type" ~content:"website"
  in
  let head = match canonical with
    | Some url -> head
      |> with_link ~rel:"canonical" ~href:url ()
      |> with_property ~property:"og:url" ~content:url
    | None -> head
  in
  let head = match og_image with
    | Some img -> head |> with_property ~property:"og:image" ~content:img
    | None -> head
  in
  head

(** Add Twitter Card *)
let with_twitter_card ~card_type ?site ?creator head =
  let head = head |> with_meta ~name:"twitter:card" ~content:card_type in
  let head = match site with
    | Some s -> head |> with_meta ~name:"twitter:site" ~content:s
    | None -> head
  in
  let head = match creator with
    | Some c -> head |> with_meta ~name:"twitter:creator" ~content:c
    | None -> head
  in
  head

(** {1 Rendering} *)

(** Render meta tag *)
let render_meta meta =
  let attrs = [
    Option.map (fun n -> Printf.sprintf {|name="%s"|} n) meta.meta_name;
    Option.map (fun p -> Printf.sprintf {|property="%s"|} p) meta.meta_property;
    Option.map (fun h -> Printf.sprintf {|http-equiv="%s"|} h) meta.meta_http_equiv;
    Some (Printf.sprintf {|content="%s"|} meta.meta_content);
  ] |> List.filter_map Fun.id |> String.concat " " in
  Printf.sprintf "<meta %s />" attrs

(** Render link tag *)
let render_link link =
  let attrs = [
    Some (Printf.sprintf {|rel="%s"|} link.link_rel);
    Some (Printf.sprintf {|href="%s"|} link.link_href);
    Option.map (fun t -> Printf.sprintf {|type="%s"|} t) link.link_type;
    Option.map (fun s -> Printf.sprintf {|sizes="%s"|} s) link.link_sizes;
    Option.map (fun a -> Printf.sprintf {|as="%s"|} a) link.link_as;
    if link.link_crossorigin then Some "crossorigin" else None;
  ] |> List.filter_map Fun.id |> String.concat " " in
  Printf.sprintf "<link %s />" attrs

(** Render script tag *)
let render_script script =
  let attrs = [
    Option.map (fun s -> Printf.sprintf {|src="%s"|} s) script.script_src;
    Option.map (fun t -> Printf.sprintf {|type="%s"|} t) script.script_type;
    if script.script_async then Some "async" else None;
    if script.script_defer then Some "defer" else None;
  ] |> List.filter_map Fun.id |> String.concat " " in
  match script.script_content with
  | Some content -> Printf.sprintf "<script %s>%s</script>" attrs content
  | None -> Printf.sprintf "<script %s></script>" attrs

(** Render style tag *)
let render_style css =
  Printf.sprintf "<style>%s</style>" css

(** Render complete head *)
let render head =
  let parts = [
    Option.map (fun t -> Printf.sprintf "<title>%s</title>" t) head.title;
  ] @ List.map (fun m -> Some (render_meta m)) (List.rev head.metas)
    @ List.map (fun l -> Some (render_link l)) (List.rev head.links)
    @ List.map (fun s -> Some (render_script s)) (List.rev head.scripts)
    @ List.map (fun c -> Some (render_style c)) (List.rev head.styles)
  in
  parts |> List.filter_map Fun.id |> String.concat "\n"

(** {1 Serialization} *)

(** Head to JSON *)
let to_json head =
  `Assoc [
    ("title", match head.title with Some t -> `String t | None -> `Null);
    ("metas", `List (List.map (fun m ->
      `Assoc [
        ("name", match m.meta_name with Some n -> `String n | None -> `Null);
        ("property", match m.meta_property with Some p -> `String p | None -> `Null);
        ("content", `String m.meta_content);
      ]
    ) head.metas));
    ("links", `List (List.map (fun l ->
      `Assoc [
        ("rel", `String l.link_rel);
        ("href", `String l.link_href);
      ]
    ) head.links));
  ]
