(** TanStack Router Handler

    Kirin route handler integration. *)

open Kirin

(** {1 Handler Types} *)

(** Handler configuration *)
type config = {
  manifest: Manifest.t;
  not_found_handler: Request.t -> Response.t;
  error_handler: exn -> Request.t -> Response.t;
}

(** Default not found handler *)
let default_not_found _req =
  Response.html ~status:`Not_found "<h1>404 Not Found</h1>"

(** Default error handler *)
let default_error_handler exn _req =
  Response.html ~status:`Internal_server_error
    (Printf.sprintf "<h1>500 Internal Server Error</h1><pre>%s</pre>"
       (Printexc.to_string exn))

(** Default config *)
let default_config manifest = {
  manifest;
  not_found_handler = default_not_found;
  error_handler = default_error_handler;
}

(** {1 Context Building} *)

(** Build loader context from request *)
let build_loader_context ~params ~search_params ctx = {
  Route_def.ctx;
  params;
  search_params;
}

(** Build action context from request *)
let build_action_context ~params ~method_ ~body ctx = {
  Route_def.ctx;
  params;
  method_;
  body;
}

(** Parse HTTP method *)
let parse_method = function
  | `GET -> Route_def.GET
  | `POST -> Route_def.POST
  | `PUT -> Route_def.PUT
  | `PATCH -> Route_def.PATCH
  | `DELETE -> Route_def.DELETE
  | _ -> Route_def.GET

(** Convert int status to Http.Status.t for redirects *)
let redirect_status_of_int = function
  | 301 -> `Moved_permanently
  | 303 -> `See_other
  | 307 -> `Temporary_redirect
  | 308 -> `Permanent_redirect
  | _ -> `Found  (* 302 default *)

(** {1 Route Handlers} *)

(** Type for registered route handlers *)
type 'ctx route_handler = {
  route_id: string;
  loader: ('ctx Route_def.loader_context -> (Yojson.Safe.t, string) result) option;
  action: ('ctx Route_def.action_context -> (Yojson.Safe.t, string) result) option;
}

(** Route handler registry *)
type 'ctx registry = {
  handlers: (string, 'ctx route_handler) Hashtbl.t;
}

(** Create registry *)
let create_registry () = {
  handlers = Hashtbl.create 32;
}

(** Register route handler *)
let register_handler ~route_id ?loader ?action registry =
  Hashtbl.replace registry.handlers route_id { route_id; loader; action }

(** Find handler by route ID *)
let find_handler route_id registry =
  Hashtbl.find_opt registry.handlers route_id

(** {1 Request Handling} *)

(** Handle loader request *)
let handle_loader ~ctx_factory ~registry route_match req =
  let route = route_match.Manifest.route in
  match find_handler route.Manifest.id registry with
  | None ->
    Response.json (`Assoc [("error", `String "Handler not found")])
    |> Response.with_status `Not_found
  | Some handler ->
    match handler.loader with
    | None ->
      Response.json (`Assoc [("data", `Null)])
    | Some loader ->
      let ctx = ctx_factory req in
      let loader_ctx = build_loader_context
        ~params:route_match.Manifest.params
        ~search_params:route_match.Manifest.search_params
        ctx in
      match loader loader_ctx with
      | Ok data ->
        Response.json (`Assoc [
          ("data", data);
          ("routeId", `String route.Manifest.id);
        ])
      | Error msg ->
        (* Check for special errors *)
        match Loader.parse_redirect msg with
        | Some (status_code, url) ->
          Response.redirect ~status:(redirect_status_of_int status_code) url
        | None ->
          if Loader.is_not_found msg then
            Response.json (`Assoc [("error", `String "Not found")])
            |> Response.with_status `Not_found
          else if Loader.is_unauthorized msg then
            Response.json (`Assoc [("error", `String "Unauthorized")])
            |> Response.with_status `Unauthorized
          else
            Response.json (`Assoc [("error", `String msg)])
            |> Response.with_status `Internal_server_error

(** Handle action request *)
let handle_action ~ctx_factory ~registry route_match req =
  let route = route_match.Manifest.route in
  match find_handler route.Manifest.id registry with
  | None ->
    Response.json (`Assoc [("error", `String "Handler not found")])
    |> Response.with_status `Not_found
  | Some handler ->
    match handler.action with
    | None ->
      Response.json (`Assoc [("error", `String "No action handler")])
      |> Response.with_status `Method_not_allowed
    | Some action ->
      let ctx = ctx_factory req in
      let body = match Request.json_body req with
        | Ok json -> Some json
        | Error _ -> None
      in
      let action_ctx = build_action_context
        ~params:route_match.Manifest.params
        ~method_:(parse_method (Request.meth req))
        ~body
        ctx in
      match action action_ctx with
      | Ok data ->
        Response.json (`Assoc [
          ("success", `Bool true);
          ("data", data);
        ])
      | Error msg ->
        Response.json (`Assoc [
          ("success", `Bool false);
          ("error", `String msg);
        ])
        |> Response.with_status `Bad_request

(** {1 Main Handler} *)

(** Create main request handler *)
let create_handler ~config ~ctx_factory ~registry =
  fun req ->
    let path = Request.path req in
    let method_ = Request.meth req in

    (* Check if it's an API request (loader/action) *)
    let is_loader_request =
      Request.header "X-Loader-Request" req = Some "1" ||
      method_ = `GET in
    let is_action_request = method_ = `POST || method_ = `PUT ||
                            method_ = `PATCH || method_ = `DELETE in

    match Manifest.match_path path config.manifest with
    | None ->
      config.not_found_handler req
    | Some route_match ->
      if is_action_request && Request.header "X-Action-Request" req = Some "1" then
        handle_action ~ctx_factory ~registry route_match req
      else if is_loader_request then
        handle_loader ~ctx_factory ~registry route_match req
      else
        (* Regular page request - return HTML shell *)
        let route = route_match.Manifest.route in
        Response.html (Printf.sprintf {|
<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <script>window.__ROUTE_ID__ = '%s';</script>
  <script>window.__ROUTE_PARAMS__ = %s;</script>
</head>
<body>
  <div id="root"></div>
  <script type="module" src="/app.js"></script>
</body>
</html>
|} route.Manifest.id
          (Yojson.Safe.to_string (`Assoc (List.map (fun (k, v) ->
            (k, `String v)) route_match.Manifest.params))))

(** {1 Route Registration} *)

(** Create routes for TanStack router *)
let routes ~config ~ctx_factory ~registry =
  let handler = create_handler ~config ~ctx_factory ~registry in
  [
    get "/*" handler;
    post "/*" handler;
    put "/*" handler;
    patch "/*" handler;
    delete "/*" handler;
  ]

(** Create manifest route *)
let manifest_route manifest =
  get "/_routes/manifest.json" (fun _req ->
    Response.json (Manifest.to_json manifest)
  )
