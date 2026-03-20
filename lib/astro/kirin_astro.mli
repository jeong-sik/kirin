(** @since 1.0.0
    @status experimental
    This module's API may change in future versions. *)

module Island = Island
module Route_def = Route_def
module File_router = File_router
module Content = Content
module Meta = Meta
module Integration = Integration
module View_transitions = View_transitions
module Protocol = Protocol
module Ssr = Ssr
module Handler = Handler
module Codegen = Codegen
val dev_config : ?port:int -> unit -> Handler.config
val static_config : dist_path:string -> unit -> Handler.config
val server_config :
  dist_path:string ->
  ?workers:int -> ?timeout_s:float -> unit -> Handler.config
val hybrid_config :
  dist_path:string -> ?workers:int -> unit -> Handler.config
val react_island :
  component:string ->
  ?directive:Island.client_directive ->
  ?props:(string * Yojson.Safe.t) list -> unit -> Island.t
val vue_island :
  component:string ->
  ?directive:Island.client_directive ->
  ?props:(string * Yojson.Safe.t) list -> unit -> Island.t
val svelte_island :
  component:string ->
  ?directive:Island.client_directive ->
  ?props:(string * Yojson.Safe.t) list -> unit -> Island.t
val solid_island :
  component:string ->
  ?directive:Island.client_directive ->
  ?props:(string * Yojson.Safe.t) list -> unit -> Island.t
val preact_island :
  component:string ->
  ?directive:Island.client_directive ->
  ?props:(string * Yojson.Safe.t) list -> unit -> Island.t
val client_load : Island.client_directive
val client_idle : Island.client_directive
val client_visible : Island.client_directive
val client_media : string -> Island.client_directive
val client_only : string -> Island.client_directive
val static_route : string -> Route_def.t
val ssr_route : string -> Route_def.t
val hybrid_route : string -> Route_def.t
val define_schema :
  string -> (string * Content.schema_field) list -> Content.schema
val define_collection :
  name:string -> schema:Content.schema -> Content.collection
val string_field : ?required:bool -> unit -> Content.schema_field
val number_field :
  ?required:bool -> ?min:float -> ?max:float -> unit -> Content.schema_field
val bool_field : ?required:bool -> unit -> Content.schema_field
val date_field : ?required:bool -> unit -> Content.schema_field
val enum_field : ?required:bool -> string list -> Content.schema_field
val reference_field : ?required:bool -> string -> Content.schema_field
val image_field : ?required:bool -> unit -> Content.schema_field
val seo :
  title:string ->
  ?description:string ->
  ?og_image:string -> ?canonical:string -> unit -> Meta.t
val head : title:string -> unit -> Meta.t
val render_head : Meta.t -> string
val react_integration : Integration.t
val vue_integration : Integration.t
val svelte_integration : Integration.t
val solid_integration : Integration.t
val preact_integration : Integration.t
val lit_integration : Integration.t
val alpine_integration : Integration.t
val enable_view_transitions :
  View_transitions.config -> View_transitions.config
val transition_element :
  name:string ->
  ?animation:View_transitions.animation ->
  ?persist:bool -> unit -> View_transitions.element
val persist_element : string -> View_transitions.element
val astro_handler :
  Handler.config -> Handler.request_info -> Handler.response
val handle :
  config:Handler.config ->
  path:string ->
  ?query:(string * string) list ->
  ?headers:(string * string) list ->
  ?method_:string -> ?body:string -> unit -> Handler.response
val create_ssr_engine : ?config:Ssr.config -> unit -> Ssr.t
val render_ssr :
  Ssr.t ->
  url:string ->
  ?props:[> `Assoc of 'a list ] ->
  ?islands:'b list -> unit -> (string, string) result
val ssr_stats : Ssr.t -> Ssr.stats
val shutdown_ssr : Ssr.t -> unit
val generate_types :
  routes:Route_def.t list ->
  collections:Content.collection list -> string
val generate_routes : routes:Route_def.t list -> string
val generate_config :
  integrations:Integration.t list ->
  output:Handler.output -> string
val discover_routes : pages_dir:'a -> unit -> 'b list
