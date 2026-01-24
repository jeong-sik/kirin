(** Angular File Router

    Angular-style file-based routing with app/ directory convention. *)

(** {1 File Types} *)

(** Angular file types in app/ directory *)
type file_type =
  | Component       (* *.component.ts *)
  | Module          (* *.module.ts - legacy NgModule *)
  | Routes          (* *.routes.ts - standalone routes *)
  | ServerRoutes    (* app.routes.server.ts *)
  | Guard           (* *.guard.ts *)
  | Resolver        (* *.resolver.ts *)
  | Service         (* *.service.ts *)
  | Interceptor     (* *.interceptor.ts *)

(** {1 Route Segment Types} *)

(** Segment type in route path *)
type segment_type =
  | Static of string      (* users *)
  | Dynamic of string     (* :id *)
  | Wildcard              (* ** *)
  | Optional of string    (* :id? - via optional route) *)

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
  else if s = "**" then
    { raw = s; segment_type = Wildcard; param_name = Some "wildcard" }
  else if len > 1 && s.[0] = ':' then
    let name = String.sub s 1 (len - 1) in
    { raw = s; segment_type = Dynamic name; param_name = Some name }
  else
    { raw = s; segment_type = Static s; param_name = None }

(** Parse full path into segments *)
let parse_path path =
  let parts = String.split_on_char '/' path in
  List.filter (fun s -> s <> "") parts
  |> List.map parse_segment

(** {1 Route Pattern Generation} *)

(** Generate Angular-style pattern from path *)
let path_to_pattern path =
  let segments = parse_path path in
  let parts = List.map (fun seg ->
    match seg.segment_type with
    | Static s -> s
    | Dynamic name -> ":" ^ name
    | Wildcard -> "**"
    | Optional name -> ":" ^ name
  ) segments in
  String.concat "/" parts

(** Extract params from path *)
let extract_params path =
  let segments = parse_path path in
  List.filter_map (fun seg ->
    match seg.segment_type with
    | Static _ -> None
    | Dynamic name -> Some (name, Route_def.String)
    | Wildcard -> Some ("wildcard", Route_def.Slug)
    | Optional name -> Some (name, Route_def.Optional Route_def.String)
  ) segments

(** {1 Route Discovery} *)

(** Discovered route *)
type discovered_route = {
  file_path: string;
  route_path: string;
  file_type: file_type;
  component_name: string option;
  is_lazy: bool;
  render_mode: Route_def.render_mode option;
}

(** Check file type *)
let detect_file_type name =
  if Filename.check_suffix name ".component.ts" then Some Component
  else if Filename.check_suffix name ".module.ts" then Some Module
  else if Filename.check_suffix name ".routes.ts" then Some Routes
  else if name = "app.routes.server.ts" then Some ServerRoutes
  else if Filename.check_suffix name ".guard.ts" then Some Guard
  else if Filename.check_suffix name ".resolver.ts" then Some Resolver
  else if Filename.check_suffix name ".service.ts" then Some Service
  else if Filename.check_suffix name ".interceptor.ts" then Some Interceptor
  else None

(** Extract component name from file *)
let component_name_from_file path =
  let base = Filename.basename path in
  let without_ext = Filename.remove_extension base in
  (* user.component -> UserComponent *)
  let parts = String.split_on_char '.' without_ext in
  match parts with
  | name :: _ ->
    let capitalized = String.capitalize_ascii name in
    Some (capitalized ^ "Component")
  | [] -> None

(** Convert directory path to route path *)
let dir_to_route_path dir_path =
  let parts = String.split_on_char '/' dir_path in
  let route_parts = List.filter_map (fun part ->
    if part = "" || part = "app" then None
    else if String.length part > 0 && part.[0] = '[' then
      (* [id] -> :id *)
      let len = String.length part in
      if len > 2 && part.[len - 1] = ']' then
        Some (":" ^ String.sub part 1 (len - 2))
      else Some part
    else Some part
  ) parts in
  "/" ^ String.concat "/" route_parts

(** Discover route from component file *)
let discover_route ~base_dir file_path =
  let dir = Filename.dirname file_path in
  let route_path = dir_to_route_path dir in
  let file_type = match detect_file_type (Filename.basename file_path) with
    | Some ft -> ft
    | None -> Component
  in
  {
    file_path = Filename.concat base_dir file_path;
    route_path;
    file_type;
    component_name = component_name_from_file file_path;
    is_lazy = false;
    render_mode = None;
  }

(** Recursively discover routes *)
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
      else
        match detect_file_type entry with
        | Some Component -> [discover_route ~base_dir rel_path]
        | Some Routes -> [discover_route ~base_dir rel_path]
        | _ -> []
    ) entries

(** Discover all routes from app directory *)
let discover_routes app_dir =
  discover_routes_aux ~base_dir:app_dir ~prefix:"" ""

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

(** {1 Serialization} *)

(** Segment to JSON *)
let segment_to_json seg =
  let type_str = match seg.segment_type with
    | Static _ -> "static"
    | Dynamic _ -> "dynamic"
    | Wildcard -> "wildcard"
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
    ("componentName", match route.component_name with
      | Some n -> `String n
      | None -> `Null);
    ("isLazy", `Bool route.is_lazy);
  ]
