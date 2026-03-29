(** @since 1.0.0
    @status experimental
    This module's API may change in future versions. *)

module Manifest = Manifest
module Assets = Assets
module Vite = Vite
module Meta = Meta
module Data = Data
module Hydrate = Hydrate
module Protocol = Protocol
module Worker = Worker
module Node_worker = Node_worker
module Ssr = Ssr
module Streaming = Streaming

val static_routes : ?config:Vite.prod_config -> unit -> Kirin.Router.route list

val hydrate
  :  title:string
  -> ?meta:(string * string) list
  -> ?initial_data:Yojson.Safe.t option
  -> manifest:Manifest.t
  -> entry:string
  -> unit
  -> string

val hydrate_response
  :  title:string
  -> ?meta:(string * string) list
  -> ?initial_data:Yojson.Safe.t option
  -> manifest:Manifest.t
  -> entry:string
  -> unit
  -> Kirin.Response.t

val create_ssr : ?config:Ssr.config -> unit -> Ssr.t

val ssr
  :  engine:Ssr.t
  -> url:string
  -> ?props:Yojson.Safe.t
  -> unit
  -> (string, string) Result.t

val ssr_handler : Ssr.t -> Kirin.Request.t -> Kirin.Response.t

val ssr_handler_with_fallback
  :  fallback:string
  -> Ssr.t
  -> Kirin.Request.t
  -> Kirin.Response.t
