(** @since 1.0.0
    @status experimental
    This module's API may change in future versions. *)

module Meta = Meta
module Data = Data
module Hydrate = Hydrate
module Protocol = Protocol
module Worker = Worker
module Ssr = Ssr
module Streaming = Streaming
module Router = Router
module Handler = Handler
type ssr_engine = Ssr.t
type ssr_config = Ssr.config
type render_response = Protocol.render_response
type meta_builder = Meta.builder
type route = Router.route
val config : bundle:string -> Ssr.config
val create_engine : Ssr.config -> Ssr.t
val shutdown : Ssr.t -> unit
val render :
  Ssr.t ->
  url:string ->
  ?props:Yojson.Safe.t ->
  unit -> (Protocol.render_response, string) result
val render_cached :
  Ssr.t ->
  url:string ->
  ?props:Yojson.Safe.t ->
  unit -> (Protocol.render_response, string) result
val render_with_fallback :
  Ssr.t ->
  url:string -> ?props:Yojson.Safe.t -> fallback:string -> unit -> string
val spa_shell :
  title:string ->
  entry_script:string -> ?styles:string list -> unit -> string
val ssr_shell :
  title:string ->
  entry_script:string ->
  ssr_html:string ->
  ?styles:string list ->
  ?initial_data:Data.initial_data -> unit -> string
val vite_shell :
  title:string ->
  manifest:(string *
            [> `Assoc of
                 (string *
                  [> `List of [> `String of string ] list | `String of string
                  ])
                 list ])
           list ->
  entry:string -> ?dev_mode:bool -> unit -> string
val meta : unit -> Meta.builder
val build_meta : Meta.builder -> string
val page_meta :
  title:string ->
  ?description:string ->
  ?canonical_url:string -> ?og_image:string -> unit -> Meta.builder
val data_script : ?var:string -> Yojson.Safe.t -> string
val route_data_script : route_id:string -> Yojson.Safe.t -> string
val store_script : store_id:string -> Yojson.Safe.t -> string
val route :
  path:string ->
  component:string ->
  ?data:string ->
  ?preload:bool -> ?children:Router.route list -> unit -> Router.route
val index_route : component:string -> ?data:string -> unit -> Router.route
val catch_all_route :
  component:string -> ?data:string -> unit -> Router.route
val match_route : string -> Router.route list -> Router.match_result option
val discover_routes : base_path:string -> string -> Router.route list
val stream_sse :
  engine:Ssr.t ->
  url:string -> ?props:Yojson.Safe.t -> unit -> string
val progressive_marker :
  component_id:string -> priority:Streaming.priority -> string
val progressive_script : string
val default_options : Handler.options
val routes : options:Handler.options -> Kirin.Router.route list
val data_routes :
  loaders:(string *
           (url:string ->
            params:'a list ->
            Yojson.Safe.t Router.loader_result))
          list ->
  Kirin.Router.route list
val stats : Ssr.t -> Ssr.stats
val clear_cache : Ssr.t -> unit
val health_check :
  Ssr.t ->
  (Worker.state * Protocol.health_status option)
  array
val recover : Ssr.t -> unit
