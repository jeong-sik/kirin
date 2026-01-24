(** Remix Routes

    File-based routing with nested routes support.
    Routes define loaders, actions, and component rendering. *)

(** {1 Route Types} *)

(** Route segment *)
type segment =
  | Static of string
  | Dynamic of string  (* :param *)
  | Splat              (* * *)
  | Optional of string (* :param? *)

(** Route definition *)
type t = {
  path: string;
  segments: segment list;
  loader: (Loader.loader_context -> Yojson.Safe.t Loader.loader_result) option;
  action: (Action.action_context -> Yojson.Safe.t Action.action_result) option;
  error_boundary: bool;
  index: bool;
  children: t list;
}

(** {1 Route Parsing} *)

(** Parse segment from string *)
let parse_segment s =
  if s = "*" then Splat
  else if String.length s > 0 && s.[0] = ':' then
    let name = String.sub s 1 (String.length s - 1) in
    if String.length name > 0 && name.[String.length name - 1] = '?' then
      Optional (String.sub name 0 (String.length name - 1))
    else
      Dynamic name
  else
    Static s

(** Parse path into segments *)
let parse_path path =
  path
  |> String.split_on_char '/'
  |> List.filter (fun s -> s <> "")
  |> List.map parse_segment

(** {1 Route Creation} *)

(** Create route *)
let create ?(loader = None) ?(action = None) ?(error_boundary = false) ?(index = false) ?(children = []) path =
  {
    path;
    segments = parse_path path;
    loader;
    action;
    error_boundary;
    index;
    children;
  }

(** Create index route *)
let index ?loader ?action () =
  create ~index:true ?loader ?action ""

(** Create layout route (no path, just nesting) *)
let layout ?loader ?error_boundary children =
  create ~children ?loader ?error_boundary ""

(** Add loader to route *)
let with_loader loader route =
  { route with loader = Some loader }

(** Add action to route *)
let with_action action route =
  { route with action = Some action }

(** Add error boundary *)
let with_error_boundary route =
  { route with error_boundary = true }

(** Add children *)
let with_children children route =
  { route with children }

(** {1 Route Matching} *)

(** Match result *)
type match_result = {
  route: t;
  params: (string * string) list;
  pathname: string;
}

(** Match segment against path part *)
let match_segment segment part =
  match segment with
  | Static s -> if s = part then Some [] else None
  | Dynamic name -> Some [(name, part)]
  | Splat -> Some [("*", part)]
  | Optional name ->
    if part = "" then Some [] else Some [(name, part)]

(** Match route against path *)
let match_route route path_parts =
  let rec match_segments segments parts params =
    match segments, parts with
    | [], [] -> Some params
    | [], _ -> None  (* Route shorter than path *)
    | [Splat], rest ->
      Some (params @ [("*", String.concat "/" rest)])
    | segment :: rest_segments, part :: rest_parts ->
      (match match_segment segment part with
       | Some new_params ->
         match_segments rest_segments rest_parts (params @ new_params)
       | None -> None)
    | _segment :: _rest, [] ->
      (* Check if remaining segments are optional *)
      None
  in
  match_segments route.segments path_parts []

(** Find matching route in tree *)
let rec find_match routes path_parts =
  routes |> List.find_map (fun route ->
    match match_route route path_parts with
    | Some params ->
      Some { route; params; pathname = route.path }
    | None ->
      (* Try children *)
      find_match route.children path_parts
  )

(** Find all matching routes (for nested routes) *)
let rec find_matches routes path_parts acc =
  routes |> List.fold_left (fun found route ->
    match match_route route path_parts with
    | Some params ->
      let match_ = { route; params; pathname = route.path } in
      (* Continue with children for nested matches *)
      let remaining = List.length route.segments in
      let child_parts = if remaining < List.length path_parts then
        List.filteri (fun i _ -> i >= remaining) path_parts
      else [] in
      find_matches route.children child_parts (found @ [match_])
    | None -> found
  ) acc

(** {1 Route Execution} *)

(** Execute loader for route *)
let execute_loader route ctx =
  match route.loader with
  | Some loader -> loader ctx
  | None -> Loader.Data `Null

(** Execute action for route *)
let execute_action route ctx =
  match route.action with
  | Some action -> action ctx
  | None -> Action.ServerError "No action handler"

(** Execute all loaders in parallel (for nested routes) *)
let execute_loaders matches ctx =
  matches |> List.map (fun m ->
    let ctx_with_params = Loader.with_params m.params ctx in
    (m.route.path, execute_loader m.route ctx_with_params)
  )

(** {1 URL Generation} *)

(** Generate URL from route and params *)
let generate_url route params =
  route.segments |> List.map (fun seg ->
    match seg with
    | Static s -> s
    | Dynamic name ->
      (match List.assoc_opt name params with
       | Some v -> v
       | None -> ":" ^ name)
    | Optional name ->
      (match List.assoc_opt name params with
       | Some v -> v
       | None -> "")
    | Splat ->
      (match List.assoc_opt "*" params with
       | Some v -> v
       | None -> "*")
  ) |> String.concat "/"

(** {1 Serialization} *)

(** Segment to JSON *)
let segment_to_json = function
  | Static s -> `Assoc [("type", `String "static"); ("value", `String s)]
  | Dynamic name -> `Assoc [("type", `String "dynamic"); ("name", `String name)]
  | Splat -> `Assoc [("type", `String "splat")]
  | Optional name -> `Assoc [("type", `String "optional"); ("name", `String name)]

(** Route to JSON (without handlers) *)
let rec to_json route =
  `Assoc [
    ("path", `String route.path);
    ("segments", `List (List.map segment_to_json route.segments));
    ("hasLoader", `Bool (Option.is_some route.loader));
    ("hasAction", `Bool (Option.is_some route.action));
    ("errorBoundary", `Bool route.error_boundary);
    ("index", `Bool route.index);
    ("children", `List (List.map to_json route.children));
  ]

(** Match result to JSON *)
let match_to_json m =
  `Assoc [
    ("pathname", `String m.pathname);
    ("params", `Assoc (List.map (fun (k, v) -> (k, `String v)) m.params));
  ]
