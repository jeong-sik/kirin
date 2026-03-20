val schema_field_to_ts : Content.schema_field -> string
val is_field_required : Content.schema_field -> bool
val schema_to_ts : Content.schema -> string
val collection_entry_type : Content.collection -> string
val route_params_type : Route_def.t -> string
val route_to_ts : Route_def.t -> string
val generate_types_file :
  routes:Route_def.t list ->
  collections:Content.collection list -> string
val generate_routes_file : Route_def.t list -> string
val generate_astro_config :
  integrations:Integration.t list ->
  output:Handler.output -> string
val schema_field_to_zod : Content.schema_field -> string
val generate_content_config : Content.collection list -> string
val generate_island_types : Island.t list -> string
val generate_env_dts : unit -> string
