(** Vue/Nuxt File Router

    Nuxt-style file-based routing with pages/, layouts/, middleware/. *)

(** {1 File Patterns} *)

(** Nuxt file types *)
type file_type =
  | Page           (* pages/*.vue *)
  | Layout         (* layouts/*.vue *)
  | Middleware     (* middleware/*.ts *)
  | ServerRoute    (* server/api/*.ts, server/routes/*.ts *)
  | Plugin         (* plugins/*.ts *)
  | Composable     (* composables/*.ts *)

(** Segment type in route path *)
type segment_type =
  | Static of string      (* users *)
  | Dynamic of string     (* [id] *)
  | CatchAll of string    (* [...slug] *)
  | Optional of string    (* [[id]] *)

(** Parsed segment *)
type segment = {
  raw: string;
  segment_type: segment_type;
  param_name: string option;
}

(** {1 Path Parsing} *)

(** Parse a single segment *)
let parse_segment s =
  let len = String.length s in
  if len = 0 then
    { raw = s; segment_type = Static ""; param_name = None }
  else if len > 4 && String.sub s 0 2 = "[[" && String.sub s (len - 2) 2 = "]]" then
    (* [[optional]] parameter *)
    let name = String.sub s 2 (len - 4) in
    { raw = s; segment_type = Optional name; param_name = Some name }
  else if len > 5 && String.sub s 0 4 = "[..." && s.[len - 1] = ']' then
    (* [...catchAll] parameter *)
    let name = String.sub s 4 (len - 5) in
    { raw = s; segment_type = CatchAll name; param_name = Some name }
  else if len > 2 && s.[0] = '[' && s.[len - 1] = ']' then
    (* [dynamic] parameter *)
    let name = String.sub s 1 (len - 2) in
    { raw = s; segment_type = Dynamic name; param_name = Some name }
  else
    { raw = s; segment_type = Static s; param_name = None }

(** Parse full path into segments *)
let parse_path path =
  let parts = String.split_on_char '/' path in
  List.filter (fun s -> s <> "") parts
  |> List.map parse_segment

(** {1 Route Pattern Generation} *)

(** Generate Express-style pattern from path *)
let path_to_pattern path =
  let segments = parse_path path in
  let parts = List.map (fun seg ->
    match seg.segment_type with
    | Static s -> s
    | Dynamic name -> ":" ^ name
    | CatchAll name -> "*" ^ name
    | Optional name -> ":" ^ name ^ "?"
  ) segments in
  "/" ^ String.concat "/" parts

(** Extract params from path *)
let extract_params path =
  let segments = parse_path path in
  List.filter_map (fun seg ->
    match seg.segment_type with
    | Static _ -> None
    | Dynamic name -> Some (name, Route_def.String)
    | CatchAll name -> Some (name, Route_def.String)
    | Optional name -> Some (name, Route_def.Optional Route_def.String)
  ) segments

(** {1 Nuxt File Discovery} *)

(** Discovered route *)
type discovered_route = {
  file_path: string;      (* Relative path in pages/ *)
  route_path: string;     (* URL path *)
  file_type: file_type;
  is_index: bool;
  has_server_handler: bool;
}

(** Check if file is a page *)
let is_page_file name =
  Filename.check_suffix name ".vue" &&
  not (String.contains name '_')  (* Exclude _*.vue partials *)

(** Check if file is a layout *)
let is_layout_file name =
  Filename.check_suffix name ".vue"

(** Check if file is middleware *)
let is_middleware_file name =
  Filename.check_suffix name ".ts" ||
  Filename.check_suffix name ".js"

(** Check if file is server route *)
let is_server_route_file name =
  (Filename.check_suffix name ".ts" ||
   Filename.check_suffix name ".js" ||
   Filename.check_suffix name ".get.ts" ||
   Filename.check_suffix name ".post.ts" ||
   Filename.check_suffix name ".put.ts" ||
   Filename.check_suffix name ".delete.ts")

(** Convert file path to route path *)
let file_to_route_path file_path =
  let without_ext = Filename.remove_extension file_path in
  (* Handle index files *)
  let path = if Filename.basename without_ext = "index" then
    Filename.dirname without_ext
  else
    without_ext
  in
  (* Ensure leading slash *)
  if path = "." || path = "" then "/"
  else if path.[0] = '/' then path
  else "/" ^ path

(** Discover route from file *)
let discover_route ~base_dir file_path =
  let route_path = file_to_route_path file_path in
  let full_path = Filename.concat base_dir file_path in
  let is_index = Filename.basename (Filename.remove_extension file_path) = "index" in
  (* Check for corresponding server handler *)
  let server_handler_path =
    Filename.concat "server/api" (route_path ^ ".ts")
  in
  let has_server_handler = Sys.file_exists (Filename.concat base_dir server_handler_path) in
  {
    file_path = full_path;
    route_path;
    file_type = Page;
    is_index;
    has_server_handler;
  }

(** Discover all routes from a pages directory *)
let rec discover_routes_aux ~base_dir ~prefix dir =
  let full_dir = Filename.concat base_dir dir in
  if not (Sys.file_exists full_dir && Sys.is_directory full_dir) then []
  else
    let entries = Sys.readdir full_dir |> Array.to_list in
    List.concat_map (fun entry ->
      let rel_path = Filename.concat prefix entry in
      let full_path = Filename.concat full_dir entry in
      if Sys.is_directory full_path then
        discover_routes_aux ~base_dir ~prefix:rel_path entry
      else if is_page_file entry then
        [discover_route ~base_dir rel_path]
      else
        []
    ) entries

(** Discover all routes from pages directory *)
let discover_routes pages_dir =
  discover_routes_aux ~base_dir:pages_dir ~prefix:"" ""

(** {1 Route Tree Building} *)

(** Route tree node *)
type route_node = {
  segment: string;
  route: discovered_route option;
  children: route_node list;
}

(** Create empty node *)
let empty_node segment = {
  segment;
  route = None;
  children = [];
}

(** Insert route into tree *)
let rec insert_route segments route node =
  match segments with
  | [] ->
    { node with route = Some route }
  | seg :: rest ->
    let child = try
      List.find (fun n -> n.segment = seg) node.children
    with Not_found ->
      empty_node seg
    in
    let updated_child = insert_route rest route child in
    let other_children = List.filter (fun n -> n.segment <> seg) node.children in
    { node with children = updated_child :: other_children }

(** Build tree from routes *)
let build_tree routes =
  let root = empty_node "" in
  List.fold_left (fun tree route ->
    let segments = parse_path route.route_path in
    let seg_strings = List.map (fun s -> s.raw) segments in
    insert_route seg_strings route tree
  ) root routes

(** {1 Nuxt Config} *)

(** Nuxt router config *)
type router_config = {
  strict_trailing_slash: bool;
  sensitive: bool;
  hash_mode: bool;
  scroll_behavior_type: string;  (* "auto", "smooth" *)
}

(** Default router config *)
let default_config = {
  strict_trailing_slash = false;
  sensitive = false;
  hash_mode = false;
  scroll_behavior_type = "auto";
}

(** {1 Serialization} *)

(** Segment to JSON *)
let segment_to_json seg =
  let type_str = match seg.segment_type with
    | Static _ -> "static"
    | Dynamic _ -> "dynamic"
    | CatchAll _ -> "catchAll"
    | Optional _ -> "optional"
  in
  `Assoc [
    ("raw", `String seg.raw);
    ("type", `String type_str);
    ("paramName", match seg.param_name with
      | Some n -> `String n
      | None -> `Null);
  ]

(** Discovered route to JSON *)
let discovered_to_json route =
  `Assoc [
    ("filePath", `String route.file_path);
    ("routePath", `String route.route_path);
    ("isIndex", `Bool route.is_index);
    ("hasServerHandler", `Bool route.has_server_handler);
  ]

(** Config to JSON *)
let config_to_json config =
  `Assoc [
    ("strictTrailingSlash", `Bool config.strict_trailing_slash);
    ("sensitive", `Bool config.sensitive);
    ("hashMode", `Bool config.hash_mode);
    ("scrollBehaviorType", `String config.scroll_behavior_type);
  ]
