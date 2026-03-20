module Qrl = Qrl
module Signal = Signal
module Route_def = Route_def
module File_router = File_router
module Container = Container
module Loader = Loader
module Action = Action
module Meta = Meta
module Protocol = Protocol
module Ssr = Ssr
module Handler = Handler
module Codegen = Codegen
val dev_config : ?port:int -> unit -> Handler.config
val ssr_config :
  dist_path:string ->
  ?workers:int -> ?timeout_s:float -> unit -> Handler.config
val static_config : dist_path:string -> unit -> Handler.config
val ssr_route : string -> Route_def.t
val static_route : string -> Route_def.t
val spa_route : string -> Route_def.t
val on_click : chunk:string -> symbol:string -> string
val on_input : chunk:string -> symbol:string -> string
val on_submit : chunk:string -> symbol:string -> string
val signal : 'a -> 'a Signal.signal
val computed : (unit -> 'a) -> 'a Signal.computed
val resource : (unit -> unit) -> 'a Signal.resource
val create_container : unit -> Container.t
val container_script : Container.t -> string
val loader : name:string -> (Loader.context -> 'a) -> 'a Loader.t
val run_loader : 'a Loader.t -> Loader.context -> 'a Loader.result
val action :
  name:string -> (Action.context -> 'a Action.result) -> 'a Action.t
val run_action : 'a Action.t -> Action.context -> 'a Action.result
val seo :
  title:string ->
  ?description:string ->
  ?canonical:string -> ?og_image:string -> unit -> Meta.t
val head : title:string -> unit -> Meta.t
val render_head : Meta.t -> string
val qwik_handler : Handler.config -> Handler.request_info -> Handler.response
val handle :
  config:Handler.config ->
  path:string ->
  ?query:(string * string) list ->
  ?headers:(string * string) list ->
  ?method_:string -> ?body:string -> unit -> Handler.response
val create_ssr_engine : ?config:Ssr.config -> unit -> Ssr.t
val render_ssr : Ssr.t -> url:string -> unit -> (string, string) result
val ssr_stats : Ssr.t -> Ssr.stats
val shutdown_ssr : Ssr.t -> unit
val generate_types : routes:Route_def.t list -> string
val generate_routes : routes:Route_def.t list -> string
val discover_routes : root_dir:'a -> unit -> File_router.discovered_route
