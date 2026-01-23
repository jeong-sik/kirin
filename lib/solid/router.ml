(** Solid Router Integration

    Integration with @solidjs/router for file-based routing. *)

(** {1 Route Types} *)

(** Route definition *)
type route = {
  path: string;
  component: string;  (* Component import path *)
  children: route list;
  data: string option;  (* Data function import path *)
  preload: bool;
}

(** Route match result *)
type match_result = {
  route: route;
  params: (string * string) list;
  search: (string * string) list;
}

(** {1 Route Building} *)

(** Create route *)
let route ~path ~component ?data ?(preload = false) ?(children = []) () = {
  path;
  component;
  children;
  data;
  preload;
}

(** Create index route *)
let index ~component ?data () =
  route ~path:"" ~component ?data ()

(** Create catch-all route *)
let catch_all ~component ?data () =
  route ~path:"*" ~component ?data ()

(** Create layout route (no path, just children) *)
let layout ~component ?(children = []) () = {
  path = "";
  component;
  children;
  data = None;
  preload = false;
}

(** {1 Route Matching} *)

(** Parse path into segments *)
let parse_path path =
  String.split_on_char '/' path
  |> List.filter (fun s -> s <> "")

(** Check if segment is dynamic *)
let is_dynamic_segment seg =
  String.length seg > 0 && String.get seg 0 = ':'

(** Extract param name from dynamic segment *)
let param_name seg =
  if is_dynamic_segment seg then
    String.sub seg 1 (String.length seg - 1)
  else seg

(** Match single segment *)
let match_segment pattern segment =
  if pattern = "*" then
    Some [("*", segment)]
  else if is_dynamic_segment pattern then
    Some [(param_name pattern, segment)]
  else if pattern = segment then
    Some []
  else
    None

(** Match path against pattern *)
let match_path pattern path =
  let pattern_segs = parse_path pattern in
  let path_segs = parse_path path in

  let rec match_segs acc patterns paths =
    match patterns, paths with
    | [], [] -> Some (List.rev acc)
    | ["*"], rest ->
      Some (List.rev (("*", String.concat "/" rest) :: acc))
    | p :: ps, s :: ss ->
      (match match_segment p s with
       | Some params -> match_segs (params @ acc) ps ss
       | None -> None)
    | [], _ -> None  (* Extra path segments *)
    | _, [] ->
      (* Check if remaining patterns are optional *)
      if List.for_all (fun p ->
        String.length p > 1 && String.get p (String.length p - 1) = '?'
      ) patterns then
        Some (List.rev acc)
      else
        None
  in
  match_segs [] pattern_segs path_segs

(** Find matching route *)
let rec find_route path routes =
  List.find_map (fun route ->
    match match_path route.path path with
    | Some params ->
      Some { route; params; search = [] }
    | None ->
      (* Check children *)
      find_route path route.children
  ) routes

(** {1 Route Tree Generation} *)

(** Generate Solid Router config *)
let rec route_to_config route =
  let base = Printf.sprintf {|{
  path: "%s",
  component: lazy(() => import("%s"))|} route.path route.component in

  let data = match route.data with
    | Some d -> Printf.sprintf {|,
  data: () => import("%s").then(m => m.default)|} d
    | None -> ""
  in

  let children = match route.children with
    | [] -> ""
    | kids ->
      let child_configs = List.map route_to_config kids in
      Printf.sprintf {|,
  children: [
%s
  ]|} (String.concat ",\n" child_configs)
  in

  base ^ data ^ children ^ "\n}"

(** Generate routes array *)
let routes_to_config routes =
  let configs = List.map route_to_config routes in
  Printf.sprintf "[\n%s\n]" (String.concat ",\n" configs)

(** {1 File-Based Route Discovery} *)

(** Discover routes from directory structure *)
let rec discover_routes ~base_path dir =
  try
    let entries = Sys.readdir dir in
    let routes = Array.fold_left (fun acc entry ->
      let full_path = Filename.concat dir entry in
      if Sys.is_directory full_path then
        let child_routes = discover_routes ~base_path full_path in
        let route_path = "/" ^ (String.sub full_path
          (String.length base_path + 1)
          (String.length full_path - String.length base_path - 1)) in
        (* Check for index component *)
        let index_file = Filename.concat full_path "index.tsx" in
        if Sys.file_exists index_file then
          let r = route
            ~path:route_path
            ~component:(index_file)
            ~children:child_routes
            ()
          in
          r :: acc
        else if child_routes <> [] then
          (* Layout without index *)
          let r = { path = route_path; component = ""; children = child_routes;
                    data = None; preload = false } in
          r :: acc
        else
          acc
      else if Filename.check_suffix entry ".tsx" && entry <> "index.tsx" then
        let name = Filename.chop_suffix entry ".tsx" in
        let route_path = "/" ^ name in
        let r = route ~path:route_path ~component:full_path () in
        r :: acc
      else
        acc
    ) [] entries in
    List.rev routes
  with _ -> []

(** {1 Preload Hints} *)

(** Generate preload link for route *)
let preload_link route =
  Printf.sprintf {|<link rel="modulepreload" href="%s">|} route.component

(** Generate all preload links for matched route *)
let rec preload_links route =
  let self = if route.preload then [preload_link route] else [] in
  let children_links = List.concat_map preload_links route.children in
  self @ children_links

(** {1 Data Loading} *)

(** Route data loader result *)
type 'a loader_result =
  | Data of 'a
  | Redirect of string * int
  | NotFound
  | ServerError of string

(** Serialize loader result *)
let serialize_loader_result serialize = function
  | Data d ->
    `Assoc [("type", `String "data"); ("data", serialize d)]
  | Redirect (url, status) ->
    `Assoc [("type", `String "redirect"); ("url", `String url); ("status", `Int status)]
  | NotFound ->
    `Assoc [("type", `String "notFound")]
  | ServerError msg ->
    `Assoc [("type", `String "error"); ("message", `String msg)]
