(** SEO Meta Tag Helpers

    Generates HTML meta tags for SEO, Open Graph, Twitter Cards,
    and other common head elements.
*)

(** Escape HTML special characters *)
let escape_html s =
  let buf = Buffer.create (String.length s) in
  String.iter (function
    | '<' -> Buffer.add_string buf "&lt;"
    | '>' -> Buffer.add_string buf "&gt;"
    | '&' -> Buffer.add_string buf "&amp;"
    | '"' -> Buffer.add_string buf "&quot;"
    | '\'' -> Buffer.add_string buf "&#x27;"
    | c -> Buffer.add_char buf c
  ) s;
  Buffer.contents buf

(** Generate title tag *)
let title t =
  Printf.sprintf "<title>%s</title>" (escape_html t)

(** Generate meta description *)
let description d =
  Printf.sprintf {|<meta name="description" content="%s">|} (escape_html d)

(** Generate generic meta tag *)
let meta ~name ~content =
  Printf.sprintf {|<meta name="%s" content="%s">|}
    (escape_html name) (escape_html content)

(** Generate Open Graph meta tag *)
let og ~property ~content =
  Printf.sprintf {|<meta property="og:%s" content="%s">|}
    (escape_html property) (escape_html content)

(** Generate Twitter Card meta tag *)
let twitter ~name ~content =
  Printf.sprintf {|<meta name="twitter:%s" content="%s">|}
    (escape_html name) (escape_html content)

(** Generate canonical link *)
let canonical url =
  Printf.sprintf {|<link rel="canonical" href="%s">|} (escape_html url)

(** Generate charset meta *)
let charset ?(encoding = "UTF-8") () =
  Printf.sprintf {|<meta charset="%s">|} (escape_html encoding)

(** Generate viewport meta *)
let viewport ?(content = "width=device-width, initial-scale=1.0") () =
  Printf.sprintf {|<meta name="viewport" content="%s">|} (escape_html content)

(** Generate robots meta *)
let robots ?(index = true) ?(follow = true) () =
  let directives = [
    (if index then "index" else "noindex");
    (if follow then "follow" else "nofollow");
  ] in
  Printf.sprintf {|<meta name="robots" content="%s">|}
    (String.concat ", " directives)

(** Generate favicon link *)
let favicon ?(mime_type = "image/x-icon") href =
  Printf.sprintf {|<link rel="icon" type="%s" href="%s">|}
    (escape_html mime_type) (escape_html href)

(** Generate apple-touch-icon link *)
let apple_touch_icon ?sizes href =
  match sizes with
  | Some s ->
    Printf.sprintf {|<link rel="apple-touch-icon" sizes="%s" href="%s">|}
      (escape_html s) (escape_html href)
  | None ->
    Printf.sprintf {|<link rel="apple-touch-icon" href="%s">|}
      (escape_html href)

(** Generate theme-color meta *)
let theme_color color =
  Printf.sprintf {|<meta name="theme-color" content="%s">|} (escape_html color)

(** Generate alternate language link *)
let alternate ~hreflang ~href =
  Printf.sprintf {|<link rel="alternate" hreflang="%s" href="%s">|}
    (escape_html hreflang) (escape_html href)

(** Generate preconnect link for performance *)
let preconnect ?(crossorigin = false) href =
  if crossorigin then
    Printf.sprintf {|<link rel="preconnect" href="%s" crossorigin>|}
      (escape_html href)
  else
    Printf.sprintf {|<link rel="preconnect" href="%s">|}
      (escape_html href)

(** Generate dns-prefetch link *)
let dns_prefetch href =
  Printf.sprintf {|<link rel="dns-prefetch" href="%s">|} (escape_html href)

(** Generate JSON-LD structured data script *)
let json_ld json =
  let json_str = Yojson.Safe.to_string json in
  Printf.sprintf {|<script type="application/ld+json">%s</script>|} json_str

(** Meta tag configuration for a page *)
type config = {
  title: string option;
  description: string option;
  keywords: string list;
  author: string option;
  canonical: string option;
  og_title: string option;
  og_description: string option;
  og_image: string option;
  og_url: string option;
  og_type: string option;
  og_site_name: string option;
  twitter_card: string option;
  twitter_site: string option;
  twitter_creator: string option;
  twitter_title: string option;
  twitter_description: string option;
  twitter_image: string option;
  robots_index: bool;
  robots_follow: bool;
  theme_color: string option;
  extra: string list;
}

let empty_config = {
  title = None;
  description = None;
  keywords = [];
  author = None;
  canonical = None;
  og_title = None;
  og_description = None;
  og_image = None;
  og_url = None;
  og_type = None;
  og_site_name = None;
  twitter_card = None;
  twitter_site = None;
  twitter_creator = None;
  twitter_title = None;
  twitter_description = None;
  twitter_image = None;
  robots_index = true;
  robots_follow = true;
  theme_color = None;
  extra = [];
}

(** Render all meta tags from config *)
let render_config config =
  let tags = ref [] in
  let add tag = tags := tag :: !tags in

  (* Basic meta *)
  Option.iter (fun t -> add (title t)) config.title;
  Option.iter (fun d -> add (description d)) config.description;
  if config.keywords <> [] then
    add (meta ~name:"keywords" ~content:(String.concat ", " config.keywords));
  Option.iter (fun a -> add (meta ~name:"author" ~content:a)) config.author;
  Option.iter (fun c -> add (canonical c)) config.canonical;

  (* Open Graph *)
  Option.iter (fun t -> add (og ~property:"title" ~content:t)) config.og_title;
  Option.iter (fun d -> add (og ~property:"description" ~content:d)) config.og_description;
  Option.iter (fun i -> add (og ~property:"image" ~content:i)) config.og_image;
  Option.iter (fun u -> add (og ~property:"url" ~content:u)) config.og_url;
  Option.iter (fun t -> add (og ~property:"type" ~content:t)) config.og_type;
  Option.iter (fun s -> add (og ~property:"site_name" ~content:s)) config.og_site_name;

  (* Twitter Cards *)
  Option.iter (fun c -> add (twitter ~name:"card" ~content:c)) config.twitter_card;
  Option.iter (fun s -> add (twitter ~name:"site" ~content:s)) config.twitter_site;
  Option.iter (fun c -> add (twitter ~name:"creator" ~content:c)) config.twitter_creator;
  Option.iter (fun t -> add (twitter ~name:"title" ~content:t)) config.twitter_title;
  Option.iter (fun d -> add (twitter ~name:"description" ~content:d)) config.twitter_description;
  Option.iter (fun i -> add (twitter ~name:"image" ~content:i)) config.twitter_image;

  (* Robots *)
  if not (config.robots_index && config.robots_follow) then
    add (robots ~index:config.robots_index ~follow:config.robots_follow ());

  (* Theme color *)
  Option.iter (fun c -> add (theme_color c)) config.theme_color;

  (* Extra tags *)
  List.iter add config.extra;

  List.rev !tags |> String.concat "\n"

(** Render meta tags from simple key-value pairs *)
let render_pairs pairs =
  pairs
  |> List.map (fun (name, content) ->
    match name with
    | "title" -> title content
    | "description" -> description content
    | "canonical" -> canonical content
    | _ when String.length name > 3 && String.sub name 0 3 = "og:" ->
      og ~property:(String.sub name 3 (String.length name - 3)) ~content
    | _ when String.length name > 8 && String.sub name 0 8 = "twitter:" ->
      twitter ~name:(String.sub name 8 (String.length name - 8)) ~content
    | _ -> meta ~name ~content
  )
  |> String.concat "\n"

(** Common head tags: charset + viewport *)
let common_head () =
  charset () ^ "\n" ^ viewport ()

(** Convenience builder for config *)
let make
    ?title ?description ?(keywords = []) ?author ?canonical
    ?og_title ?og_description ?og_image ?og_url ?og_type ?og_site_name
    ?twitter_card ?twitter_site ?twitter_creator
    ?twitter_title ?twitter_description ?twitter_image
    ?(robots_index = true) ?(robots_follow = true)
    ?theme_color ?(extra = [])
    () =
  {
    title;
    description;
    keywords;
    author;
    canonical;
    og_title;
    og_description;
    og_image;
    og_url;
    og_type;
    og_site_name;
    twitter_card;
    twitter_site;
    twitter_creator;
    twitter_title;
    twitter_description;
    twitter_image;
    robots_index;
    robots_follow;
    theme_color;
    extra;
  }
