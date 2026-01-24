(** Astro File-based Router

    Automatic route discovery from pages/ directory.

    File patterns:
    - index.astro -> /
    - about.astro -> /about
    - [slug].astro -> /:slug (dynamic)
    - [...slug].astro -> /... (catch-all)
    - _partial.astro -> ignored (underscore prefix) *)

(** {1 Segment Types} *)

(** Route segment *)
type segment =
  | Static of string         (* Regular path segment *)
  | Dynamic of string        (* [param] *)
  | CatchAll of string       (* [...param] *)
  | OptionalCatchAll of string (* [[...param]] *)

(** {1 Segment Parsing} *)

(** Parse a filename segment *)
let parse_segment s =
  let len = String.length s in
  if len >= 6 && String.sub s 0 3 = "[[." && String.sub s (len - 2) 2 = "]]" then
    (* [[...param]] optional catch-all *)
    let inner = String.sub s 3 (len - 5) in
    if String.length inner >= 3 && String.sub inner 0 3 = ".." then
      OptionalCatchAll (String.sub inner 2 (String.length inner - 2))
    else
      Static s
  else if len >= 5 && String.sub s 0 1 = "[" && String.sub s (len - 1) 1 = "]" then
    let inner = String.sub s 1 (len - 2) in
    if String.length inner >= 3 && String.sub inner 0 3 = "..." then
      (* [...param] catch-all *)
      CatchAll (String.sub inner 3 (String.length inner - 3))
    else
      (* [param] dynamic *)
      Dynamic inner
  else
    Static s

(** Segment to path pattern *)
let segment_to_pattern = function
  | Static s -> s
  | Dynamic name -> Printf.sprintf "[%s]" name
  | CatchAll name -> Printf.sprintf "[...%s]" name
  | OptionalCatchAll name -> Printf.sprintf "[[...%s]]" name

(** {1 Route Discovery} *)

(** Check if file should be included *)
let should_include filename =
  (* Exclude files starting with _ or . *)
  String.length filename > 0 &&
  filename.[0] <> '_' &&
  filename.[0] <> '.' &&
  (* Only .astro files *)
  (Filename.check_suffix filename ".astro" ||
   Filename.check_suffix filename ".md" ||
   Filename.check_suffix filename ".mdx")

(** Remove file extension *)
let remove_extension filename =
  try
    let idx = String.rindex filename '.' in
    String.sub filename 0 idx
  with Not_found -> filename

(** Convert file path to route path *)
let file_to_route_path file_path =
  let parts = String.split_on_char '/' file_path in
  let parts = List.filter ((<>) "") parts in
  let parts = List.map (fun p ->
    let name = remove_extension p in
    if name = "index" then ""
    else segment_to_pattern (parse_segment name)
  ) parts in
  let path = "/" ^ String.concat "/" (List.filter ((<>) "") parts) in
  if path = "" then "/" else path

(** Discover routes from directory (simulated) *)
let discover_routes pages_dir =
  (* In real impl, would scan directory *)
  let _ = pages_dir in
  []

(** {1 Route Sorting} *)

(** Route priority for sorting *)
let route_priority route =
  let segments = String.split_on_char '/' route.Route_def.path in
  let static_count = List.length (List.filter (fun s ->
    String.length s > 0 && s.[0] <> '['
  ) segments) in
  let dynamic_count = List.length (List.filter (fun s ->
    String.length s > 0 && s.[0] = '[' && not (String.contains s '.')
  ) segments) in
  let catchall = if Route_def.has_rest_params route then 1 else 0 in
  (* Higher is better: static > dynamic > catchall *)
  (static_count * 100) + (dynamic_count * 10) - (catchall * 1000)

(** Sort routes by specificity *)
let sort_routes routes =
  List.sort (fun a b ->
    compare (route_priority b) (route_priority a)
  ) routes

(** {1 Route Generation} *)

(** Generate route from file *)
let route_from_file ~pages_dir file_path =
  let full_path = Filename.concat pages_dir file_path in
  let route_path = file_to_route_path file_path in
  let is_md = Filename.check_suffix file_path ".md" || Filename.check_suffix file_path ".mdx" in
  (* Markdown files are always static by default *)
  let prerender = if is_md then Route_def.Static else Route_def.OnDemand in
  let _ = full_path in
  { Route_def.
    path = route_path;
    name = Some (remove_extension (Filename.basename file_path));
    prerender;
    islands = [];
    layout = None;
    middleware = [];
    get_static_paths = Route_def.is_dynamic (Route_def.static route_path);
  }

(** {1 API Routes} *)

(** Check if file is API route *)
let is_api_route file_path =
  String.length file_path >= 4 &&
  String.sub file_path 0 4 = "api/"

(** API route methods *)
type api_method = GET | POST | PUT | DELETE | PATCH | ALL

(** Parse API route method from export name *)
let parse_api_method = function
  | "GET" | "get" -> Some GET
  | "POST" | "post" -> Some POST
  | "PUT" | "put" -> Some PUT
  | "DELETE" | "delete" -> Some DELETE
  | "PATCH" | "patch" -> Some PATCH
  | "ALL" | "all" -> Some ALL
  | _ -> None

(** {1 Endpoint Discovery} *)

(** Endpoint definition *)
type endpoint = {
  path: string;
  method_: api_method;
  handler: string;
}

(** Generate endpoint from API file *)
let endpoint_from_file ~pages_dir file_path method_ =
  let route_path = file_to_route_path file_path in
  let _ = pages_dir in
  { path = route_path;
    method_;
    handler = file_path;
  }
