(** Route Manifest

    Generated manifest for client-side routing. *)

(** {1 Manifest Types} *)

(** Route entry in manifest *)
type route_entry = {
  id: string;
  path: string;
  parent_id: string option;
  index: bool;
  has_loader: bool;
  has_action: bool;
  has_error_boundary: bool;
  has_loading: bool;
}

(** Full manifest *)
type t = {
  version: int;
  routes: route_entry list;
  root_id: string option;
}

(** {1 Manifest Building} *)

(** Create empty manifest *)
let empty = {
  version = 1;
  routes = [];
  root_id = None;
}

(** Add route to manifest *)
let add_route entry manifest =
  { manifest with routes = entry :: manifest.routes }

(** Set root route *)
let set_root id manifest =
  { manifest with root_id = Some id }

(** Find route by ID *)
let find_route id manifest =
  List.find_opt (fun r -> r.id = id) manifest.routes

(** Find route by path *)
let find_route_by_path path manifest =
  List.find_opt (fun r -> r.path = path) manifest.routes

(** Get child routes *)
let children_of id manifest =
  List.filter (fun r -> r.parent_id = Some id) manifest.routes

(** Get root routes *)
let root_routes manifest =
  List.filter (fun r -> r.parent_id = None) manifest.routes

(** {1 Path Matching} *)

(** Match result *)
type match_result = {
  route: route_entry;
  params: (string * string) list;
  search_params: (string * string) list;
}

(** Parse path into segments *)
let parse_path path =
  String.split_on_char '/' path
  |> List.filter (fun s -> s <> "")

(** Parse search params from query string *)
let parse_search query =
  if query = "" then []
  else
    String.split_on_char '&' query
    |> List.filter_map (fun pair ->
      match String.split_on_char '=' pair with
      | [k; v] -> Some (k, v)
      | [k] -> Some (k, "")
      | _ -> None
    )

(** Check if path matches pattern *)
let path_matches pattern path =
  let pattern_segs = parse_path pattern in
  let path_segs = parse_path path in

  let rec match_segments params pattern_segs path_segs =
    match pattern_segs, path_segs with
    | [], [] -> Some params
    | [], _ -> None  (* path has extra segments *)
    | ["*"], rest ->
      (* Catch-all matches everything *)
      Some (("*", String.concat "/" rest) :: params)
    | (p :: ps), (s :: ss) ->
      if String.length p > 0 && String.get p 0 = ':' then
        (* Dynamic segment *)
        let param_name = String.sub p 1 (String.length p - 1) in
        let is_optional = String.length param_name > 0 &&
                          String.get param_name (String.length param_name - 1) = '?' in
        let param_name = if is_optional then
            String.sub param_name 0 (String.length param_name - 1)
          else param_name in
        match_segments ((param_name, s) :: params) ps ss
      else if p = s then
        match_segments params ps ss
      else
        None
    | (p :: ps), [] ->
      (* Check if remaining are optional *)
      if String.length p > 1 && String.get p 0 = ':' &&
         String.get p (String.length p - 1) = '?' then
        match_segments params ps []
      else
        None
  in
  match_segments [] pattern_segs path_segs

(** Find matching route *)
let match_path path manifest =
  let (path_part, query_part) = match String.split_on_char '?' path with
    | [p; q] -> (p, q)
    | [p] -> (p, "")
    | _ -> (path, "")
  in
  let search_params = parse_search query_part in

  List.find_map (fun route ->
    match path_matches route.path path_part with
    | Some params -> Some {
        route;
        params = List.rev params;
        search_params;
      }
    | None -> None
  ) manifest.routes

(** {1 Manifest Serialization} *)

(** Convert route entry to JSON *)
let route_entry_to_json entry =
  `Assoc [
    ("id", `String entry.id);
    ("path", `String entry.path);
    ("parentId", match entry.parent_id with Some p -> `String p | None -> `Null);
    ("index", `Bool entry.index);
    ("hasLoader", `Bool entry.has_loader);
    ("hasAction", `Bool entry.has_action);
    ("hasErrorBoundary", `Bool entry.has_error_boundary);
    ("hasLoading", `Bool entry.has_loading);
  ]

(** Convert manifest to JSON *)
let to_json manifest =
  `Assoc [
    ("version", `Int manifest.version);
    ("routes", `List (List.map route_entry_to_json manifest.routes));
    ("rootId", match manifest.root_id with Some id -> `String id | None -> `Null);
  ]

(** Parse route entry from JSON *)
let route_entry_of_json json =
  match json with
  | `Assoc fields ->
    let get_string key = match List.assoc_opt key fields with
      | Some (`String s) -> Some s
      | _ -> None
    in
    let get_bool key = match List.assoc_opt key fields with
      | Some (`Bool b) -> b
      | _ -> false
    in
    (match get_string "id", get_string "path" with
     | Some id, Some path ->
       Some {
         id;
         path;
         parent_id = get_string "parentId";
         index = get_bool "index";
         has_loader = get_bool "hasLoader";
         has_action = get_bool "hasAction";
         has_error_boundary = get_bool "hasErrorBoundary";
         has_loading = get_bool "hasLoading";
       }
     | _ -> None)
  | _ -> None

(** Parse manifest from JSON *)
let of_json json =
  match json with
  | `Assoc fields ->
    let routes = match List.assoc_opt "routes" fields with
      | Some (`List routes_json) ->
        List.filter_map route_entry_of_json routes_json
      | _ -> []
    in
    let root_id = match List.assoc_opt "rootId" fields with
      | Some (`String id) -> Some id
      | _ -> None
    in
    let version = match List.assoc_opt "version" fields with
      | Some (`Int v) -> v
      | _ -> 1
    in
    { version; routes; root_id }
  | _ -> empty

(** {1 Manifest Generation} *)

(** Generate manifest from discovered routes *)
let from_discovered_routes routes =
  let rec collect_entries parent_id routes =
    List.concat_map (fun (route : File_router.discovered_route) ->
      let entry = {
        id = route.id;
        path = route.path;
        parent_id;
        index = route.path = "/" || List.length route.children = 0;
        has_loader = true;  (* Assume all pages have loaders *)
        has_action = false; (* Default to no action *)
        has_error_boundary = route.has_error;
        has_loading = route.has_loading;
      } in
      entry :: collect_entries (Some route.id) route.children
    ) routes
  in
  let entries = collect_entries None routes in
  let root_id = match entries with
    | e :: _ -> Some e.id
    | [] -> None
  in
  { version = 1; routes = entries; root_id }
