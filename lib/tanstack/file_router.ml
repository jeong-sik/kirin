(** File-based Route Discovery

    Scan directory structure to generate routes. *)

(** {1 File Types} *)

(** Route file type *)
type file_type =
  | Page        (* page.ml or index.ml *)
  | Layout      (* layout.ml *)
  | Loading     (* loading.ml - loading state *)
  | ErrorBoundary (* error.ml - error boundary *)
  | NotFound    (* not-found.ml - 404 *)

let file_type_of_name name =
  match name with
  | "page.ml" | "index.ml" -> Some Page
  | "layout.ml" -> Some Layout
  | "loading.ml" -> Some Loading
  | "error.ml" -> Some ErrorBoundary
  | "not-found.ml" -> Some NotFound
  | _ -> None

(** {1 Route Segment Types} *)

(** Segment in a route path *)
type segment =
  | Static of string       (* normal segment *)
  | Dynamic of string      (* [id] → :id *)
  | CatchAll of string     (* [...slug] → *slug *)
  | Optional of string     (* [[id]] → :id? *)
  | Group of string        (* (group) → no URL segment *)

(** Parse directory name to segment *)
let parse_segment name =
  let len = String.length name in
  if len >= 5 && String.sub name 0 4 = "[..." && String.get name (len-1) = ']' then
    (* [...slug] → catch-all *)
    CatchAll (String.sub name 4 (len - 5))
  else if len >= 4 && String.sub name 0 2 = "[[" && String.sub name (len-2) 2 = "]]" then
    (* [[id]] → optional *)
    Optional (String.sub name 2 (len - 4))
  else if len >= 2 && String.get name 0 = '[' && String.get name (len-1) = ']' then
    (* [id] → dynamic *)
    Dynamic (String.sub name 1 (len - 2))
  else if len >= 2 && String.get name 0 = '(' && String.get name (len-1) = ')' then
    (* (group) → route group *)
    Group (String.sub name 1 (len - 2))
  else
    Static name

(** Convert segment to path pattern *)
let segment_to_pattern = function
  | Static s -> s
  | Dynamic name -> ":" ^ name
  | CatchAll name -> "*" ^ name
  | Optional name -> ":" ^ name ^ "?"
  | Group _ -> ""  (* groups don't add to URL *)

(** {1 Discovered Route} *)

(** A discovered route from file system *)
type discovered_route = {
  id: string;
  path: string;
  file_path: string;
  segments: segment list;
  has_layout: bool;
  has_loading: bool;
  has_error: bool;
  children: discovered_route list;
}

(** {1 Directory Scanning} *)

(** Check if path is a directory *)
let is_directory path =
  try Sys.is_directory path
  with Sys_error _ -> false

(** Check if path is a file *)
let is_file path =
  try not (Sys.is_directory path)
  with Sys_error _ -> false

(** List directory contents *)
let list_dir path =
  try
    Sys.readdir path |> Array.to_list |> List.sort String.compare
  with Sys_error _ -> []

(** Check for special files in directory *)
let check_special_files dir =
  let files = list_dir dir in
  let has_page = List.exists (fun f -> f = "page.ml" || f = "index.ml") files in
  let has_layout = List.mem "layout.ml" files in
  let has_loading = List.mem "loading.ml" files in
  let has_error = List.mem "error.ml" files in
  (has_page, has_layout, has_loading, has_error)

(** Generate route ID from path *)
let route_id_of_path segments =
  let parts = List.filter_map (function
    | Static s -> Some s
    | Dynamic s -> Some ("$" ^ s)
    | CatchAll s -> Some ("$$" ^ s)
    | Optional s -> Some ("$" ^ s)
    | Group _ -> None
  ) segments in
  if parts = [] then "root"
  else String.concat "_" parts

(** {1 Route Discovery} *)

(** Recursively discover routes *)
let rec discover_routes ~base_path ~parent_segments dir =
  let entries = list_dir dir in
  let subdirs = List.filter (fun e ->
    let path = Filename.concat dir e in
    is_directory path && not (String.get e 0 = '.' || String.get e 0 = '_')
  ) entries in

  let (has_page, has_layout, has_loading, has_error) = check_special_files dir in

  let current_segments = parent_segments in
  let current_path = "/" ^ String.concat "/" (List.filter_map (fun seg ->
    let p = segment_to_pattern seg in
    if p = "" then None else Some p
  ) current_segments) in

  (* Discover child routes *)
  let children = List.filter_map (fun subdir ->
    let segment = parse_segment subdir in
    let child_dir = Filename.concat dir subdir in
    let child_segments = current_segments @ [segment] in
    let child_routes = discover_routes ~base_path ~parent_segments:child_segments child_dir in
    match child_routes with
    | [] -> None
    | routes -> Some routes
  ) subdirs |> List.flatten in

  (* Create route for this directory if it has a page *)
  if has_page || has_layout || children <> [] then
    let route = {
      id = route_id_of_path current_segments;
      path = if current_path = "/" then "/" else current_path;
      file_path = Filename.concat dir "page.ml";
      segments = current_segments;
      has_layout;
      has_loading;
      has_error;
      children;
    } in
    [route]
  else
    children

(** Discover all routes from a root directory *)
let discover root_dir =
  if is_directory root_dir then
    discover_routes ~base_path:root_dir ~parent_segments:[] root_dir
  else
    []

(** {1 Route Manifest} *)

(** Route manifest entry *)
type manifest_entry = {
  m_id: string;
  m_path: string;
  m_file: string;
  m_parent: string option;
  m_index: bool;
}

(** Flatten routes to manifest entries *)
let rec flatten_routes ?parent routes =
  List.concat_map (fun route ->
    let entry = {
      m_id = route.id;
      m_path = route.path;
      m_file = route.file_path;
      m_parent = parent;
      m_index = route.path = "/" || String.length route.path > 0 &&
                String.get route.path (String.length route.path - 1) = '/';
    } in
    entry :: flatten_routes ~parent:route.id route.children
  ) routes

(** Generate manifest JSON *)
let manifest_to_json routes =
  let entries = flatten_routes routes in
  let entry_to_json e =
    `Assoc [
      ("id", `String e.m_id);
      ("path", `String e.m_path);
      ("file", `String e.m_file);
      ("parent", match e.m_parent with Some p -> `String p | None -> `Null);
      ("index", `Bool e.m_index);
    ]
  in
  `List (List.map entry_to_json entries)

(** {1 TypeScript Generation} *)

(** Generate TypeScript route tree type *)
let generate_route_tree_type routes =
  let rec route_to_type route =
    let children_type = match route.children with
      | [] -> ""
      | children ->
        let child_types = List.map route_to_type children in
        Printf.sprintf "\n  children: {\n%s\n  };" (String.concat "\n" child_types)
    in
    Printf.sprintf "    '%s': { path: '%s';%s }," route.id route.path children_type
  in
  let route_types = List.map route_to_type routes in
  Printf.sprintf "interface RouteTree {\n%s\n}" (String.concat "\n" route_types)
