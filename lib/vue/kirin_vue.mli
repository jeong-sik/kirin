module Route_def = Route_def
module File_router = File_router
module Loader = Loader
module Action = Action
module Manifest = Manifest
module Preload = Preload
module Meta = Meta
module Data = Data
module Hydrate = Hydrate
module Protocol = Protocol
module Worker = Worker
module Ssr = Ssr
module Streaming = Streaming
module Handler = Handler
module Codegen = Codegen
val dev_config : ?port:int -> unit -> Handler.config
val spa_config :
  manifest_path:string -> entry_point:string -> unit -> Handler.config
val hydration_config :
  manifest_path:string -> entry_point:string -> unit -> Handler.config
val ssr_config :
  manifest_path:string ->
  entry_point:string ->
  ?workers:int -> ?timeout_s:float -> unit -> Handler.config
val streaming_config :
  manifest_path:string -> entry_point:string -> unit -> Handler.config
val page :
  path:string ->
  ?layout:string ->
  ?middleware:Route_def.middleware list -> unit -> Route_def.t
val dynamic :
  path:string ->
  param_name:string ->
  ?param_type:Route_def.param_type -> unit -> Route_def.t
val catch_all : path:string -> unit -> Route_def.t
val fetch :
  key:string ->
  ?method_:string ->
  ?headers:(string * string) list ->
  ?query:(string * string) list -> unit -> Loader.fetch_options
val async_data :
  key:string -> ?server:bool -> ?lazy_:bool -> unit -> Loader.async_options
val seo :
  title:string ->
  ?description:string ->
  ?image:string -> ?url:string -> unit -> Meta.seo_meta
val head :
  title:string ->
  ?description:string ->
  ?og_title:string ->
  ?og_image:string -> ?canonical:string -> unit -> Meta.head
val payload : route_path:string -> unit -> Data.payload
val with_data : string -> Yojson.Safe.t -> Data.payload -> Data.payload
val with_state : string -> Yojson.Safe.t -> Data.payload -> Data.payload
val vue_handler : Handler.config -> Handler.request_info -> Handler.response
val handle :
  config:Handler.config ->
  path:string ->
  ?query:(string * string) list ->
  ?headers:(string * string) list ->
  ?method_:string -> ?body:string -> unit -> Handler.response
val is_dev : unit -> bool
val vite_dev_url : ?port:int -> string -> string
val generate_types :
  routes:Route_def.t list ->
  ?api_routes:Action.api_route list -> unit -> string
val generate_route_type : Route_def.t -> string
val modulepreload : string -> Preload.hint
val prefetch : string -> Preload.hint
val render_hints : Preload.hint list -> string
val not_found_page : unit -> string
val server_error_page : ?message:string -> unit -> string
val island :
  id:string ->
  component:string ->
  ?props:Yojson.Safe.t -> ?priority:string -> unit -> Data.island_data
val island_wrapper :
  id:string ->
  component:string -> priority:Hydrate.priority -> html:string -> string
val streaming_context : ?config:Streaming.config -> unit -> Streaming.context
val start_streaming : Streaming.context -> unit
val send_shell : Streaming.context -> html:string -> string
val send_suspense : Streaming.context -> id:string -> html:string -> string
val finish_streaming : Streaming.context -> string
val discover_routes :
  pages_dir:string -> unit -> File_router.discovered_route list
val manifest_from_routes :
  File_router.discovered_route list -> Manifest.t
