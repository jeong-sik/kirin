type config = {
  manifest : Manifest.t;
  not_found_handler : Kirin.Request.t -> Kirin.Response.t;
  error_handler : exn -> Kirin.Request.t -> Kirin.Response.t;
}
val default_not_found : 'a -> Kirin.Response.t
val default_error_handler : exn -> 'a -> Kirin.Response.t
val default_config : Manifest.t -> config
val build_loader_context :
  params:(string * string) list ->
  search_params:(string * string) list ->
  'a -> 'a Route_def.loader_context
val build_action_context :
  params:(string * string) list ->
  method_:Route_def.http_method ->
  body:Yojson.Safe.t option ->
  'a -> 'a Route_def.action_context
val parse_method :
  [> `DELETE | `GET | `PATCH | `POST | `PUT ] ->
  Route_def.http_method
val redirect_status_of_int :
  int ->
  [> `Found
   | `Moved_permanently
   | `Permanent_redirect
   | `See_other
   | `Temporary_redirect ]
type 'ctx route_handler = {
  route_id : string;
  loader :
    ('ctx Route_def.loader_context ->
     (Yojson.Safe.t, string) result)
    option;
  action :
    ('ctx Route_def.action_context ->
     (Yojson.Safe.t, string) result)
    option;
}
type 'ctx registry = { handlers : (string, 'ctx route_handler) Hashtbl.t; }
val create_registry : unit -> 'a registry
val register_handler :
  route_id:string ->
  ?loader:('a Route_def.loader_context ->
           (Yojson.Safe.t, string) result) ->
  ?action:('a Route_def.action_context ->
           (Yojson.Safe.t, string) result) ->
  'a registry -> unit
val find_handler : string -> 'a registry -> 'a route_handler option
val handle_loader :
  ctx_factory:('a -> 'b) ->
  registry:'b registry ->
  Manifest.match_result -> 'a -> Kirin.Response.t
val handle_action :
  ctx_factory:(Kirin.Request.t -> 'a) ->
  registry:'a registry ->
  Manifest.match_result ->
  Kirin.Request.t -> Kirin.Response.t
val create_handler :
  config:config ->
  ctx_factory:(Kirin.Request.t -> 'a) ->
  registry:'a registry -> Kirin.Request.t -> Kirin.Response.t
val routes :
  config:config ->
  ctx_factory:(Kirin.Request.t -> 'a) ->
  registry:'a registry -> Kirin.Router.route list
val manifest_route : Manifest.t -> Kirin.Router.route
