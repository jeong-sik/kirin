module Route_def = Route_def
module File_router = File_router
module Loader = Loader
module Action = Action
module Manifest = Manifest
module Preload = Preload
module Handler = Handler
module Codegen = Codegen
type route_entry = Manifest.route_entry
type match_result = Manifest.match_result
type manifest = Manifest.t
val route :
  id:string ->
  path:string ->
  ?params:Route_def.param list ->
  ?meta:Route_def.meta ->
  ?loader:('a, 'b) Route_def.loader ->
  ?action:('a, 'c) Route_def.action ->
  ?children:string list -> unit -> ('a, 'b, 'c) Route_def.t
val discover_routes : string -> File_router.discovered_route list
val manifest_from_routes :
  File_router.discovered_route list -> Manifest.t
val loader_ok :
  ?headers:(string * string) list ->
  ?status:int -> 'a -> 'a Loader.loader_result
val loader_redirect : ?status:int -> string -> ('a, string) result
val loader_not_found : unit -> ('a, string) result
val loader_unauthorized : unit -> ('a, string) result
val action_success : 'a -> 'a Action.action_result
val action_error : string -> 'a Action.action_result
val action_redirect : ?status:int -> string -> 'a Action.action_result
val validation_error : (string * string) list -> 'a Action.action_result
type preload_strategy = Preload.strategy = Intent | Viewport | Render | None
val preload_hint : href:string -> strategy:Preload.strategy -> string
val module_preload : src:string -> string
val route_hints :
  loader_url:string ->
  js_modules:string list ->
  css_files:string list -> priority:Preload.priority -> string
val config : Manifest.t -> Handler.config
val create_registry : unit -> 'a Handler.registry
val register :
  route_id:string ->
  ?loader:('a Route_def.loader_context ->
           (Yojson.Safe.t, string) result) ->
  ?action:('a Route_def.action_context ->
           (Yojson.Safe.t, string) result) ->
  'a Handler.registry -> unit
val routes :
  config:Handler.config ->
  ctx_factory:(Kirin.Request.t -> 'a) ->
  registry:'a Handler.registry -> Kirin.Router.route list
val manifest_route : Manifest.t -> Kirin.Router.route
val generate_types : Manifest.t -> string
val generate_router : Manifest.t -> string
val generate_hooks : unit -> string
val setup :
  routes_dir:string ->
  ctx_factory:'a -> Manifest.t * 'b Handler.registry * Handler.config * 'a
val all_routes :
  manifest:Manifest.t ->
  registry:'a Handler.registry ->
  ctx_factory:(Kirin.Request.t -> 'a) -> Kirin.Router.route list
