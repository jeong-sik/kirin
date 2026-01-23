(** Kirin Browser - Client-Side Router

    SPA-style routing for browser applications.
    Works with History API for client-side navigation.

    {b Example:}
    {[
      let router = Router.create [
        Router.route "/" (fun _params -> render_home ());
        Router.route "/users/:id" (fun params ->
          let id = List.assoc "id" params in
          render_user id);
        Router.route "/about" (fun _params -> render_about ());
      ]

      (* Start listening for navigation *)
      Router.start router;

      (* Navigate programmatically *)
      Router.navigate router "/users/123";
    ]}
*)

(** {1 Types} *)

(** Route parameters extracted from URL *)
type params = (string * string) list

(** Route handler function *)
type handler = params -> unit

(** Single route definition *)
type route = {
  pattern : string;
  segments : segment list;
  handler : handler;
}

(** Segment type for pattern matching *)
and segment =
  | Static of string
  | Param of string
  | Wildcard

(** Router instance *)
type t = {
  routes : route list;
  not_found : handler option;
  mutable listener : History.listener_id option;
  mutable current_path : string;
}

(** {1 Internal Helpers} *)

(** Parse pattern into segments *)
let parse_pattern pattern =
  let parts = String.split_on_char '/' pattern
    |> List.filter (fun s -> String.length s > 0) in
  List.map (fun part ->
    if String.length part > 0 && part.[0] = ':' then
      Param (String.sub part 1 (String.length part - 1))
    else if part = "*" then
      Wildcard
    else
      Static part
  ) parts

(** Match a path against a pattern, returning params if match *)
let match_route (route : route) path : params option =
  let path_parts = String.split_on_char '/' path
    |> List.filter (fun s -> String.length s > 0) in
  let rec match_segments segs parts acc =
    match segs, parts with
    | [], [] -> Some (List.rev acc)
    | [], _ -> None
    | [Wildcard], _ -> Some (List.rev acc)  (* Wildcard matches rest *)
    | Wildcard :: _, _ -> Some (List.rev acc)
    | _, [] -> None
    | Static s :: rest_segs, p :: rest_parts ->
      if s = p then match_segments rest_segs rest_parts acc
      else None
    | Param name :: rest_segs, p :: rest_parts ->
      match_segments rest_segs rest_parts ((name, p) :: acc)
  in
  match_segments route.segments path_parts []

(** Find matching route *)
let find_route routes path =
  let rec find = function
    | [] -> None
    | route :: rest ->
      match match_route route path with
      | Some params -> Some (route, params)
      | None -> find rest
  in
  find routes

(** {1 Route Builder} *)

(** Create a route definition *)
let route pattern handler =
  { pattern; segments = parse_pattern pattern; handler }

(** Create a route with explicit segments (for advanced use) *)
let route_with_segments pattern segments handler =
  { pattern; segments; handler }

(** {1 Router Creation} *)

(** Create a new router *)
let create ?(not_found : handler option) routes =
  { routes; not_found; listener = None; current_path = "" }

(** Add a route to existing router *)
let add_route router route_def =
  { router with routes = router.routes @ [route_def] }

(** Set not found handler *)
let set_not_found router handler =
  { router with not_found = Some handler }

(** {1 Routing} *)

(** Dispatch to matching route *)
let dispatch router path =
  router.current_path <- path;
  match find_route router.routes path with
  | Some (route, params) -> route.handler params
  | None ->
    match router.not_found with
    | Some handler -> handler []
    | None -> ()  (* Silently ignore *)

(** {1 Navigation} *)

(** Navigate to a path *)
let navigate router path =
  History.push_state path;
  dispatch router path

(** Navigate without adding to history *)
let replace router path =
  History.replace_state path;
  dispatch router path

(** Go back *)
let back () = History.back ()

(** Go forward *)
let forward () = History.forward ()

(** {1 Lifecycle} *)

(** Start the router (listen for navigation events) *)
let start router =
  (* Handle popstate (browser back/forward) *)
  let listener = History.on_popstate (fun event ->
    dispatch router event.path
  ) in
  router.listener <- Some listener;
  (* Dispatch initial route *)
  dispatch router (History.Location.pathname ())

(** Stop the router *)
let stop router =
  match router.listener with
  | Some id ->
    History.remove_listener id;
    router.listener <- None
  | None -> ()

(** {1 Link Helpers} *)

open Js_of_ocaml

(** Create onclick handler for SPA links *)
let link_handler router path (event : Dom_html.mouseEvent Js.t) =
  Dom.preventDefault event;
  navigate router path

(** Check if path matches current route *)
let is_active router path =
  router.current_path = path

(** Check if path starts with prefix (for nested routes) *)
let is_active_prefix router prefix =
  String.length router.current_path >= String.length prefix
  && String.sub router.current_path 0 (String.length prefix) = prefix

(** {1 Query String Helpers} *)

(** Get current query parameters *)
let query_params () = History.Location.query_params ()

(** Get a specific query parameter *)
let query_param name =
  List.assoc_opt name (query_params ())

(** Build query string from params *)
let build_query_string params =
  match params with
  | [] -> ""
  | _ ->
    "?" ^ (List.map (fun (k, v) -> k ^ "=" ^ v) params |> String.concat "&")

(** Navigate with query params *)
let navigate_with_query router path params =
  navigate router (path ^ build_query_string params)

(** {1 Hash Routing (fallback for older browsers)} *)

module HashRouter = struct
  type t = {
    routes : route list;
    not_found : handler option;
    mutable listener : History.listener_id option;
  }

  let create ?(not_found : handler option) routes =
    { routes; not_found; listener = None }

  let dispatch router path =
    match find_route router.routes path with
    | Some (route, params) -> route.handler params
    | None ->
      match router.not_found with
      | Some handler -> handler []
      | None -> ()

  let navigate router path =
    History.Hash.set path;
    dispatch router path

  let start router =
    let listener = History.Hash.on_change (fun path ->
      dispatch router path
    ) in
    router.listener <- Some listener;
    dispatch router (History.Hash.get ())

  let stop router =
    match router.listener with
    | Some id ->
      History.remove_listener id;
      router.listener <- None
    | None -> ()
end
