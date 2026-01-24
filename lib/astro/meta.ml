(** Astro Meta Tags

    Head/meta tag management for SEO and social sharing. *)

(** {1 Meta Types} *)

(** Meta tag *)
type meta_tag =
  | Name of { name: string; content: string }
  | Property of { property: string; content: string }
  | HttpEquiv of { http_equiv: string; content: string }
  | Charset of string

(** Link tag *)
type link_tag = {
  rel: string;
  href: string;
  type_: string option;
  sizes: string option;
  media: string option;
  crossorigin: string option;
}

(** Script tag *)
type script_tag = {
  src: string option;
  inline: string option;
  type_: string option;
  async: bool;
  defer: bool;
  module_: bool;
}

(** Head content *)
type t = {
  title: string option;
  meta: meta_tag list;
  links: link_tag list;
  scripts: script_tag list;
  base: string option;
  canonical: string option;
}

(** {1 Head Construction} *)

(** Empty head *)
let empty = {
  title = None;
  meta = [];
  links = [];
  scripts = [];
  base = None;
  canonical = None;
}

(** Set title *)
let with_title title head = { head with title = Some title }

(** Add meta tag *)
let with_meta tag head = { head with meta = tag :: head.meta }

(** Add link tag *)
let with_link link head = { head with links = link :: head.links }

(** Add script tag *)
let with_script script head = { head with scripts = script :: head.scripts }

(** Set base URL *)
let with_base url head = { head with base = Some url }

(** Set canonical URL *)
let with_canonical url head = { head with canonical = Some url }

(** {1 Meta Tag Helpers} *)

(** Description meta *)
let description content =
  Name { name = "description"; content }

(** Keywords meta *)
let keywords content =
  Name { name = "keywords"; content }

(** Author meta *)
let author content =
  Name { name = "author"; content }

(** Viewport meta *)
let viewport content =
  Name { name = "viewport"; content }

(** Robots meta *)
let robots content =
  Name { name = "robots"; content }

(** Charset meta *)
let charset encoding = Charset encoding

(** {1 Open Graph} *)

(** OG title *)
let og_title content =
  Property { property = "og:title"; content }

(** OG description *)
let og_description content =
  Property { property = "og:description"; content }

(** OG image *)
let og_image content =
  Property { property = "og:image"; content }

(** OG URL *)
let og_url content =
  Property { property = "og:url"; content }

(** OG type *)
let og_type content =
  Property { property = "og:type"; content }

(** OG site name *)
let og_site_name content =
  Property { property = "og:site_name"; content }

(** {1 Twitter Cards} *)

(** Twitter card *)
let twitter_card content =
  Name { name = "twitter:card"; content }

(** Twitter title *)
let twitter_title content =
  Name { name = "twitter:title"; content }

(** Twitter description *)
let twitter_description content =
  Name { name = "twitter:description"; content }

(** Twitter image *)
let twitter_image content =
  Name { name = "twitter:image"; content }

(** Twitter site *)
let twitter_site content =
  Name { name = "twitter:site"; content }

(** {1 Link Helpers} *)

(** Stylesheet link *)
let stylesheet href = {
  rel = "stylesheet";
  href;
  type_ = Some "text/css";
  sizes = None;
  media = None;
  crossorigin = None;
}

(** Preconnect link *)
let preconnect href = {
  rel = "preconnect";
  href;
  type_ = None;
  sizes = None;
  media = None;
  crossorigin = Some "anonymous";
}

(** Preload link *)
let preload ~href ~as_ = {
  rel = "preload";
  href;
  type_ = None;
  sizes = None;
  media = None;
  crossorigin = Some as_;
}

(** Favicon link *)
let favicon href = {
  rel = "icon";
  href;
  type_ = Some "image/x-icon";
  sizes = None;
  media = None;
  crossorigin = None;
}

(** Apple touch icon *)
let apple_touch_icon ~href ~sizes = {
  rel = "apple-touch-icon";
  href;
  type_ = None;
  sizes = Some sizes;
  media = None;
  crossorigin = None;
}

(** {1 SEO Helpers} *)

(** Create SEO-optimized head *)
let seo ~title ?description:desc ?og_image_url ?twitter_card_type ?canonical_url () =
  let head = empty |> with_title title in
  let head = match desc with
    | Some d -> head
      |> with_meta (description d)
      |> with_meta (og_description d)
      |> with_meta (twitter_description d)
    | None -> head
  in
  let head = head
    |> with_meta (og_title title)
    |> with_meta (twitter_title title)
  in
  let head = match og_image_url with
    | Some url -> head
      |> with_meta (og_image url)
      |> with_meta (twitter_image url)
    | None -> head
  in
  let head = match twitter_card_type with
    | Some t -> head |> with_meta (twitter_card t)
    | None -> head |> with_meta (twitter_card "summary")
  in
  let head = match canonical_url with
    | Some url -> head |> with_canonical url
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
  | Charset encoding ->
    Printf.sprintf {|<meta charset="%s">|} encoding

(** Render link tag *)
let render_link link =
  let attrs = [
    Some (Printf.sprintf {|rel="%s"|} link.rel);
    Some (Printf.sprintf {|href="%s"|} link.href);
    Option.map (Printf.sprintf {|type="%s"|}) link.type_;
    Option.map (Printf.sprintf {|sizes="%s"|}) link.sizes;
    Option.map (Printf.sprintf {|media="%s"|}) link.media;
    Option.map (Printf.sprintf {|crossorigin="%s"|}) link.crossorigin;
  ] |> List.filter_map Fun.id in
  Printf.sprintf "<link %s>" (String.concat " " attrs)

(** Render script tag *)
let render_script script =
  let attrs = [
    Option.map (Printf.sprintf {|src="%s"|}) script.src;
    Option.map (Printf.sprintf {|type="%s"|}) script.type_;
    (if script.async then Some "async" else None);
    (if script.defer then Some "defer" else None);
    (if script.module_ then Some {|type="module"|} else None);
  ] |> List.filter_map Fun.id in
  match script.inline with
  | Some content -> Printf.sprintf "<script %s>%s</script>" (String.concat " " attrs) content
  | None -> Printf.sprintf "<script %s></script>" (String.concat " " attrs)

(** Render full head *)
let render head =
  let parts = [] in
  let parts = match head.title with
    | Some t -> Printf.sprintf "<title>%s</title>" t :: parts
    | None -> parts
  in
  let parts = match head.base with
    | Some b -> Printf.sprintf {|<base href="%s">|} b :: parts
    | None -> parts
  in
  let parts = match head.canonical with
    | Some c -> Printf.sprintf {|<link rel="canonical" href="%s">|} c :: parts
    | None -> parts
  in
  let parts = List.fold_left (fun acc m -> render_meta m :: acc) parts head.meta in
  let parts = List.fold_left (fun acc l -> render_link l :: acc) parts head.links in
  let parts = List.fold_left (fun acc s -> render_script s :: acc) parts head.scripts in
  String.concat "\n  " (List.rev parts)
