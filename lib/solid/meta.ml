(** Solid.js Meta Tags

    SEO and document head management for Solid.js SSR. *)

(** {1 Meta Tag Types} *)

(** Meta tag definition *)
type meta_tag =
  | Title of string
  | Description of string
  | Charset of string
  | Viewport of string
  | OgTitle of string
  | OgDescription of string
  | OgImage of string
  | OgUrl of string
  | OgType of string
  | TwitterCard of string
  | TwitterTitle of string
  | TwitterDescription of string
  | TwitterImage of string
  | Canonical of string
  | Robots of string
  | Custom of string * string  (* name, content *)

(** {1 Tag Rendering} *)

(** Render single meta tag *)
let render_tag = function
  | Title t ->
    Printf.sprintf "<title>%s</title>" (Kirin.html_escape t)
  | Description d ->
    Printf.sprintf {|<meta name="description" content="%s">|} (Kirin.html_escape d)
  | Charset c ->
    Printf.sprintf {|<meta charset="%s">|} c
  | Viewport v ->
    Printf.sprintf {|<meta name="viewport" content="%s">|} v
  | OgTitle t ->
    Printf.sprintf {|<meta property="og:title" content="%s">|} (Kirin.html_escape t)
  | OgDescription d ->
    Printf.sprintf {|<meta property="og:description" content="%s">|} (Kirin.html_escape d)
  | OgImage url ->
    Printf.sprintf {|<meta property="og:image" content="%s">|} url
  | OgUrl url ->
    Printf.sprintf {|<meta property="og:url" content="%s">|} url
  | OgType t ->
    Printf.sprintf {|<meta property="og:type" content="%s">|} t
  | TwitterCard c ->
    Printf.sprintf {|<meta name="twitter:card" content="%s">|} c
  | TwitterTitle t ->
    Printf.sprintf {|<meta name="twitter:title" content="%s">|} (Kirin.html_escape t)
  | TwitterDescription d ->
    Printf.sprintf {|<meta name="twitter:description" content="%s">|} (Kirin.html_escape d)
  | TwitterImage url ->
    Printf.sprintf {|<meta name="twitter:image" content="%s">|} url
  | Canonical url ->
    Printf.sprintf {|<link rel="canonical" href="%s">|} url
  | Robots r ->
    Printf.sprintf {|<meta name="robots" content="%s">|} r
  | Custom (name, content) ->
    Printf.sprintf {|<meta name="%s" content="%s">|} name (Kirin.html_escape content)

(** Render all meta tags *)
let render_tags tags =
  List.map render_tag tags |> String.concat "\n"

(** {1 Builder Pattern} *)

(** Meta builder state *)
type builder = {
  mutable tags: meta_tag list;
}

(** Create builder *)
let create () = { tags = [] }

(** Add title *)
let title t b = b.tags <- Title t :: b.tags; b

(** Add description *)
let description d b = b.tags <- Description d :: b.tags; b

(** Add charset *)
let charset c b = b.tags <- Charset c :: b.tags; b

(** Add viewport *)
let viewport v b = b.tags <- Viewport v :: b.tags; b

(** Add Open Graph tags *)
let og ~title ?description ?image ?url ?(og_type = "website") () b =
  b.tags <- OgTitle title :: b.tags;
  (match description with Some d -> b.tags <- OgDescription d :: b.tags | None -> ());
  (match image with Some i -> b.tags <- OgImage i :: b.tags | None -> ());
  (match url with Some u -> b.tags <- OgUrl u :: b.tags | None -> ());
  b.tags <- OgType og_type :: b.tags;
  b

(** Add Twitter Card tags *)
let twitter ~card ~title ?description ?image () b =
  b.tags <- TwitterCard card :: b.tags;
  b.tags <- TwitterTitle title :: b.tags;
  (match description with Some d -> b.tags <- TwitterDescription d :: b.tags | None -> ());
  (match image with Some i -> b.tags <- TwitterImage i :: b.tags | None -> ());
  b

(** Add canonical URL *)
let canonical url b = b.tags <- Canonical url :: b.tags; b

(** Add robots directive *)
let robots r b = b.tags <- Robots r :: b.tags; b

(** Add custom meta tag *)
let custom ~name ~content b = b.tags <- Custom (name, content) :: b.tags; b

(** Build and render *)
let build b = render_tags (List.rev b.tags)

(** {1 Convenience Functions} *)

(** Create default meta tags *)
let defaults ?(charset_value = "utf-8") ?(viewport_value = "width=device-width, initial-scale=1") () =
  let b = create () in
  let b = charset charset_value b in
  let b = viewport viewport_value b in
  b

(** Quick meta for page *)
let page ~title:page_title ?description:page_description ?canonical_url ?og_image () =
  let b = defaults () in
  let b = title page_title b in
  (match page_description with
   | Some d ->
     let b = description d b in
     let b = og ~title:page_title ~description:d ?image:og_image ?url:canonical_url () b in
     let b = twitter ~card:"summary_large_image" ~title:page_title ~description:d ?image:og_image () b in
     (match canonical_url with Some u -> canonical u b | None -> b)
   | None ->
     let b = og ~title:page_title ?image:og_image ?url:canonical_url () b in
     (match canonical_url with Some u -> canonical u b | None -> b))

(** {1 JSON Serialization} *)

(** Meta tag to JSON *)
let tag_to_json = function
  | Title t -> `Assoc [("type", `String "title"); ("value", `String t)]
  | Description d -> `Assoc [("type", `String "description"); ("value", `String d)]
  | Charset c -> `Assoc [("type", `String "charset"); ("value", `String c)]
  | Viewport v -> `Assoc [("type", `String "viewport"); ("value", `String v)]
  | OgTitle t -> `Assoc [("type", `String "og:title"); ("value", `String t)]
  | OgDescription d -> `Assoc [("type", `String "og:description"); ("value", `String d)]
  | OgImage url -> `Assoc [("type", `String "og:image"); ("value", `String url)]
  | OgUrl url -> `Assoc [("type", `String "og:url"); ("value", `String url)]
  | OgType t -> `Assoc [("type", `String "og:type"); ("value", `String t)]
  | TwitterCard c -> `Assoc [("type", `String "twitter:card"); ("value", `String c)]
  | TwitterTitle t -> `Assoc [("type", `String "twitter:title"); ("value", `String t)]
  | TwitterDescription d -> `Assoc [("type", `String "twitter:description"); ("value", `String d)]
  | TwitterImage url -> `Assoc [("type", `String "twitter:image"); ("value", `String url)]
  | Canonical url -> `Assoc [("type", `String "canonical"); ("value", `String url)]
  | Robots r -> `Assoc [("type", `String "robots"); ("value", `String r)]
  | Custom (name, content) -> `Assoc [("type", `String "custom"); ("name", `String name); ("value", `String content)]

(** Builder to JSON *)
let to_json b =
  `List (List.rev_map tag_to_json b.tags)
