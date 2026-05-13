type options = {
  ssr_engine : Ssr.t option;
  routes : Router.route list;
  fallback_html : string;
  dev_mode : bool;
  vite_port : int;
}
val default_options : options
val html_response :
  options:'a -> Protocol.render_response -> Kirin.Response.t
val fallback_response : options:'a -> Kirin.Response.t
val page_handler : options:options -> Kirin.Request.t -> Kirin.Response.t
val data_handler :
  options:'a ->
  loader:(url:string ->
          params:Kirin.Request.params ->
          Yojson.Safe.t Router.loader_result) ->
  Kirin.Request.t -> Kirin.Response.t
val health_handler : options:options -> 'a -> Kirin.Response.t
val is_vite_request : string -> bool
val vite_proxy : port:int -> string -> Kirin.Response.t
val routes : options:options -> Kirin.Router.route list
val data_routes :
  loaders:(string *
           (url:string ->
            params:Kirin.Request.params ->
            Yojson.Safe.t Router.loader_result))
          list ->
  Kirin.Router.route list
