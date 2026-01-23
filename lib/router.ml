(** Router module - Route matching and dispatch *)

(** Handler type - Direct style, no Lwt! *)
type handler = Request.t -> Response.t

(** Route definition *)
type route = {
  meth : Http.Method.t;
  pattern : string;  (* e.g., "/users/:id" *)
  segments : segment list;
  handler : handler;
}

(** Segment in a route pattern *)
and segment =
  | Static of string   (* Literal match *)
  | Param of string    (* :name capture *)
  | Wildcard           (* * match anything *)

(** Router is a list of routes *)
type t = route list

(** Parse a route pattern into segments *)
let parse_pattern pattern =
  let parts = String.split_on_char '/' pattern in
  let parts = List.filter (fun s -> s <> "") parts in
  List.map (fun part ->
    if String.length part > 0 && part.[0] = ':' then
      Param (String.sub part 1 (String.length part - 1))
    else if part = "*" then
      Wildcard
    else
      Static part
  ) parts

(** Create a route with given method and pattern *)
let route meth pattern handler =
  { meth; pattern; segments = parse_pattern pattern; handler }

(** HTTP method shortcuts *)
let get pattern handler = route `GET pattern handler
let post pattern handler = route `POST pattern handler
let put pattern handler = route `PUT pattern handler
let patch pattern handler = route `PATCH pattern handler
let delete pattern handler = route `DELETE pattern handler
let head pattern handler = route `HEAD pattern handler
let options pattern handler = route `OPTIONS pattern handler

(** Match a path against route segments, returning extracted params *)
let match_segments segments path_parts =
  let rec loop segs parts params =
    match segs, parts with
    | [], [] -> Some (List.rev params)
    | [], _ -> None  (* Extra path parts *)
    | _, [] -> None  (* Missing path parts *)
    | Static s :: rest_segs, p :: rest_parts ->
      if s = p then loop rest_segs rest_parts params
      else None
    | Param name :: rest_segs, p :: rest_parts ->
      loop rest_segs rest_parts ((name, p) :: params)
    | Wildcard :: _, _ ->
      Some (List.rev params)  (* Wildcard matches rest *)
  in
  loop segments path_parts []

(** Find matching route for a request *)
let find_route routes req =
  let path = Request.path req in
  let path_parts =
    String.split_on_char '/' path
    |> List.filter (fun s -> s <> "")
  in
  let meth = Request.meth req in

  let rec try_routes = function
    | [] -> None
    | route :: rest ->
      if route.meth = meth then
        match match_segments route.segments path_parts with
        | Some params -> Some (route, params)
        | None -> try_routes rest
      else
        try_routes rest
  in
  try_routes routes

(** Dispatch request to matching handler *)
let dispatch routes req =
  match find_route routes req with
  | Some (route, params) ->
    let req = Request.with_params params req in
    route.handler req
  | None ->
    Response.not_found ()

(** Create a router from routes - returns a handler *)
let router routes = fun req -> dispatch routes req

(** Scoped routes with prefix and middleware *)
let scope prefix middlewares routes =
  let apply_middlewares handler =
    List.fold_right (fun mw h -> mw h) middlewares handler
  in
  List.map (fun route ->
    let pattern = prefix ^ route.pattern in
    let handler = apply_middlewares route.handler in
    { route with pattern; segments = parse_pattern pattern; handler }
  ) routes
