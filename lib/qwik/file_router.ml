(** Qwik File-based Router

    QwikCity file-based routing conventions. *)

(** {1 File Types} *)

(** QwikCity file types *)
type file_type =
  | Index                 (* index.tsx - route component *)
  | Layout                (* layout.tsx - layout component *)
  | Plugin                (* plugin.ts or plugin@name.ts *)
  | Service               (* service.ts - middleware *)
  | OnRequest            (* onRequest, onGet, onPost handlers *)
  | Loader                (* routeLoader$ definitions *)
  | Action                (* routeAction$ definitions *)
  | Unknown

(** Route segment type *)
type segment_type =
  | Static of string      (* users *)
  | Dynamic of string     (* [id] *)
  | CatchAll of string    (* [...slug] *)
  | Optional of string    (* [[optional]] *)
  | Group of string       (* (marketing) - route group, no URL segment *)

(** Parsed segment *)
type segment = {
  raw: string;
  segment_type: segment_type;
  priority: int;          (* For sorting: static=0, dynamic=1, optional=2, catchall=3 *)
}

(** {1 Segment Parsing} *)

(** Parse segment from directory/file name *)
let parse_segment name =
  let len = String.length name in
  if len = 0 then
    { raw = name; segment_type = Static ""; priority = 0 }
  else if name.[0] = '(' && name.[len-1] = ')' then
    (* Route group (marketing) *)
    let group = String.sub name 1 (len - 2) in
    { raw = name; segment_type = Group group; priority = 0 }
  else if name.[0] = '[' && name.[len-1] = ']' then
    let inner = String.sub name 1 (len - 2) in
    let inner_len = String.length inner in
    if inner_len > 3 && String.sub inner 0 3 = "..." then
      (* Catch-all [...slug] *)
      let param = String.sub inner 3 (inner_len - 3) in
      { raw = name; segment_type = CatchAll param; priority = 3 }
    else if inner_len > 2 && inner.[0] = '[' && inner.[inner_len-1] = ']' then
      (* Optional [[optional]] *)
      let param = String.sub inner 1 (inner_len - 2) in
      { raw = name; segment_type = Optional param; priority = 2 }
    else
      (* Dynamic [id] *)
      { raw = name; segment_type = Dynamic inner; priority = 1 }
  else
    (* Static segment *)
    { raw = name; segment_type = Static name; priority = 0 }

(** Parse full path into segments *)
let parse_path path =
  path
  |> String.split_on_char '/'
  |> List.filter (fun s -> s <> "")
  |> List.map parse_segment

(** {1 File Detection} *)

(** Detect file type from filename *)
let detect_file_type filename =
  let basename = Filename.basename filename in
  let name_no_ext = Filename.remove_extension basename in
  if String.lowercase_ascii name_no_ext = "index" then Index
  else if String.lowercase_ascii name_no_ext = "layout" then Layout
  else if String.length name_no_ext >= 6 && String.sub name_no_ext 0 6 = "plugin" then Plugin
  else if String.lowercase_ascii name_no_ext = "service" then Service
  else
    (* Check for loader/action exports in content - simplified *)
    Unknown

(** Check if file is a route file *)
let is_route_file filename =
  let ext = Filename.extension filename in
  List.mem ext [".tsx"; ".jsx"; ".ts"; ".js"; ".mdx"]

(** {1 Path Generation} *)

(** Convert segment to URL path part *)
let segment_to_path seg =
  match seg.segment_type with
  | Static s -> s
  | Dynamic _ -> ":param"      (* Will be replaced with actual param name *)
  | CatchAll _ -> "*"          (* Wildcard *)
  | Optional _ -> ""           (* May or may not be present *)
  | Group _ -> ""              (* No URL segment for groups *)

(** Convert segments to URL pattern *)
let segments_to_pattern segments =
  segments
  |> List.filter_map (fun seg ->
    match seg.segment_type with
    | Static s when s <> "" -> Some s
    | Dynamic param -> Some (":" ^ param)
    | CatchAll param -> Some ("*" ^ param)
    | Optional param -> Some (":?" ^ param)  (* Custom notation for optional *)
    | Group _ -> None
    | _ -> None
  )
  |> String.concat "/"
  |> fun s -> "/" ^ s

(** {1 Route Discovery} *)

(** Discovered route *)
type discovered_route = {
  dir_path: string;
  url_pattern: string;
  has_index: bool;
  has_layout: bool;
  loaders: string list;
  actions: string list;
  plugins: string list;
  children: discovered_route list;
}

(** Discover routes from directory (simplified) *)
let discover_routes _root_dir =
  (* In a real implementation, this would recursively scan the filesystem *)
  (* For now, return a mock discovered route *)
  {
    dir_path = "routes";
    url_pattern = "/";
    has_index = true;
    has_layout = true;
    loaders = [];
    actions = [];
    plugins = [];
    children = [];
  }

(** Convert discovered route to route_def *)
let rec to_route_def discovered =
  let route = Route_def.ssr discovered.url_pattern in
  let route = if discovered.has_layout then
    Route_def.with_layout "layout" route
  else route in
  let route = List.fold_left (fun r l -> Route_def.with_loader l r) route discovered.loaders in
  let route = List.fold_left (fun r a -> Route_def.with_action a r) route discovered.actions in
  let route = Route_def.with_children (List.map to_route_def discovered.children) route in
  route

(** {1 Pattern Matching} *)

(** Extract params from URL using pattern *)
let extract_params ~pattern ~url =
  let pattern_segs = String.split_on_char '/' pattern |> List.filter ((<>) "") in
  let url_segs = String.split_on_char '/' url |> List.filter ((<>) "") in

  let rec extract acc pats urls =
    match pats, urls with
    | [], [] -> Some (List.rev acc)
    | [], _ -> None  (* URL has more segments *)
    | p :: prest, u :: urest ->
      if String.length p > 0 && p.[0] = ':' then
        (* Dynamic param *)
        let name = String.sub p 1 (String.length p - 1) in
        extract ((name, u) :: acc) prest urest
      else if p.[0] = '*' then
        (* Catch-all - collect rest *)
        let name = String.sub p 1 (String.length p - 1) in
        let rest = String.concat "/" (u :: urest) in
        Some (List.rev ((name, rest) :: acc))
      else if p = u then
        extract acc prest urest
      else
        None
    | _ :: prest, [] ->
      (* Check if remaining patterns are optional *)
      if List.for_all (fun p -> String.length p > 1 && p.[0] = ':' && p.[1] = '?') prest then
        Some (List.rev acc)
      else
        None
  in
  extract [] pattern_segs url_segs

(** {1 Serialization} *)

(** Segment to JSON *)
let segment_to_json seg =
  let type_str, param = match seg.segment_type with
    | Static s -> ("static", Some s)
    | Dynamic p -> ("dynamic", Some p)
    | CatchAll p -> ("catchAll", Some p)
    | Optional p -> ("optional", Some p)
    | Group g -> ("group", Some g)
  in
  `Assoc [
    ("raw", `String seg.raw);
    ("type", `String type_str);
    ("param", match param with Some p -> `String p | None -> `Null);
    ("priority", `Int seg.priority);
  ]

(** Discovered route to JSON *)
let rec discovered_to_json route =
  `Assoc [
    ("dirPath", `String route.dir_path);
    ("urlPattern", `String route.url_pattern);
    ("hasIndex", `Bool route.has_index);
    ("hasLayout", `Bool route.has_layout);
    ("loaders", `List (List.map (fun l -> `String l) route.loaders));
    ("actions", `List (List.map (fun a -> `String a) route.actions));
    ("plugins", `List (List.map (fun p -> `String p) route.plugins));
    ("children", `List (List.map discovered_to_json route.children));
  ]
