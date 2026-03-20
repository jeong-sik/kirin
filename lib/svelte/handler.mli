type options = {
  ssr_engine : Ssr.t option;
  manifest : Manifest.t option;
  fallback_html : string;
  dev_mode : bool;
  vite_port : int;
}
val default_options : options
val html_response :
  options:'a -> Protocol.render_response -> Kirin.Response.t
val fallback_response : options:'a -> Kirin.Response.t
val page_handler : options:options -> Kirin.Request.t -> Kirin.Response.t
val load_handler :
  loader:(Loader.load_context ->
          Loader.load_output) ->
  Kirin.Request.t -> Kirin.Response.t
val action_handler :
  actions:Action.named_action list ->
  Kirin.Request.t -> Kirin.Response.t
val health_handler : options:options -> 'a -> Kirin.Response.t
val is_vite_request : string -> bool
val vite_proxy : port:int -> string -> Kirin.Response.t
val routes : options:options -> Kirin.Router.route list
val load_routes :
  loaders:(string *
           (Loader.load_context ->
            Loader.load_output))
          list ->
  Kirin.Router.route list
val action_routes :
  actions_map:(string * Action.named_action list) list ->
  Kirin.Router.route list
