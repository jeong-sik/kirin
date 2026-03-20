module Route_def = Route_def
module File_router = File_router
module Transfer_state = Transfer_state
module Hydration = Hydration
module Meta = Meta
module Protocol = Protocol
module Ssr = Ssr
module Handler = Handler
module Codegen = Codegen
val dev_config : ?port:int -> unit -> Handler.config
val ssr_config :
  dist_path:string ->
  ?workers:int -> ?timeout_s:float -> unit -> Handler.config
val hybrid_config :
  dist_path:string ->
  server_routes:Route_def.t list -> unit -> Handler.config
val prerender_config : dist_path:string -> unit -> Handler.config
val ssr_route : string -> string -> Route_def.t
val csr_route : string -> string -> Route_def.t
val ssg_route : string -> string -> Route_def.t
val redirect : from:string -> to_:string -> unit -> Route_def.t
val lazy_route : string -> string -> Route_def.t
val create_transfer_state : unit -> Transfer_state.t
val cache_response :
  Transfer_state.t -> method_:string -> url:string -> Yojson.Safe.t -> unit
val get_cached :
  Transfer_state.t ->
  method_:string -> url:string -> unit -> Yojson.Safe.t option
val transfer_state_script : Transfer_state.t -> string
val create_hydration_context :
  ?mode:Hydration.hydration_mode -> unit -> Hydration.context
val hydration_boundary :
  id:string -> ?trigger:Hydration.trigger -> string -> string
val skip_hydration : string -> string -> string
val seo :
  title:string ->
  ?description:string ->
  ?canonical:string -> ?og_image:string -> unit -> Meta.head
val head : title:string -> unit -> Meta.head
val render_head : Meta.head -> string
val angular_handler :
  Handler.config -> Handler.request_info -> Handler.response
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
val prerender :
  Ssr.t ->
  routes:string list -> output_dir:string -> (string * string * string) list
val generate_types : routes:Route_def.t list -> string
val generate_server_routes :
  routes:Route_def.t list -> string
val discover_routes :
  app_dir:string -> unit -> File_router.discovered_route list
