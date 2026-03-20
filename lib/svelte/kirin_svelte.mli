(** @since 1.0.0
    @status experimental
    This module's API may change in future versions. *)

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
val handler :
  bundle:string ->
  ?num_workers:int -> ?cache_ttl:int -> unit -> Kirin.Router.route list
val dev_handler : ?vite_port:int -> unit -> Kirin.Router.route list
val load_manifest : string -> Manifest.t
val scan_routes : 'a -> Manifest.t
val page :
  path:string ->
  loader:(url:string ->
          params:(string * string) list -> Route_def.load_result) ->
  Route_def.t
val page_with_actions :
  path:string ->
  loader:(url:string ->
          params:(string * string) list -> Route_def.load_result) ->
  actions:(string *
           (url:string -> form_data:Yojson.Safe.t -> Route_def.action_result))
          list ->
  Route_def.t
val data : Yojson.Safe.t -> Loader.load_output
val redirect : ?status:int -> string -> Loader.load_output
val error : int -> string -> Loader.load_output
val not_found : Loader.load_output
val success : ?data:Yojson.Safe.t -> unit -> Action.action_output
val fail : ?status:int -> Yojson.Safe.t -> Action.action_output
val action_redirect : string -> Action.action_output
val meta :
  title:string ->
  ?description:string ->
  ?image:string -> ?canonical:string -> unit -> Meta.builder
val preload_link :
  href:string -> ?strategy:[< `Hover | `Off | `Tap ] -> string -> string
val stream : title:string -> entry_script:string -> string -> string
