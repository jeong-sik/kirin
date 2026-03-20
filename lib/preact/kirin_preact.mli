module Signals = Signals
module Compat = Compat
module Manifest = Manifest
module Hydrate = Hydrate
module Protocol = Protocol
module Ssr = Ssr
module Vite = Vite
val static_routes : ?config:Vite.config -> unit -> Kirin.Router.route list
val hydrate :
  title:string ->
  ?meta:(string * string) list ->
  ?initial_data:Yojson.Safe.t option ->
  ?signals_data:(string * Yojson.Safe.t) list ->
  manifest:Manifest.t -> entry:string -> unit -> string
val hydrate_response :
  title:string ->
  ?meta:(string * string) list ->
  ?initial_data:Yojson.Safe.t option ->
  ?signals_data:(string * Yojson.Safe.t) list ->
  manifest:Manifest.t ->
  entry:string -> unit -> Kirin.Response.t
val create_ssr : ?config:Ssr.ssr_config -> unit -> Ssr.t
val ssr :
  engine:Ssr.t ->
  url:string -> ?props:Yojson.Safe.t -> unit -> (string, 'a) result
val ssr_with_signals :
  engine:Ssr.t ->
  url:string -> signals:Yojson.Safe.t -> unit -> (string, 'a) result
val ssr_handler : Ssr.t -> 'a -> Kirin.Response.t
val ssr_handler_with_fallback :
  fallback:string -> Ssr.t -> 'a -> Kirin.Response.t
val signal : string -> Yojson.Safe.t -> Signals.signal
val computed :
  name:string -> dependencies:string list -> string -> Signals.computed
val signals_import : Signals.import_type list -> string
val compat_aliases : unit -> string
val check_library_compat : string -> Compat.library_compat option
