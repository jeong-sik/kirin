type config = {
  prefix : string;
  enable_batching : bool;
  max_batch_size : int;
}
val default_config : config
val parse_path : prefix:string -> string -> string option
val parse_query_input : Kirin.Request.t -> Yojson.Safe.t option
val parse_body_input : string -> (Yojson.Safe.t, string) result
val success_response : Yojson.Safe.t -> Kirin.Response.t
val error_response : ?code:int -> string -> Kirin.Response.t
val not_found_response : string -> Kirin.Response.t
val handle_query :
  'a Trpc_router.t ->
  (Kirin.Request.t -> 'a) -> string -> Kirin.Request.t -> Kirin.Response.t
val handle_mutation :
  'a Trpc_router.t ->
  ('b -> 'a) -> string -> string -> 'b -> Kirin.Response.t
val handle_batch :
  config:config ->
  'a Trpc_router.t ->
  ('b -> 'a) -> string -> 'b -> Kirin.Response.t
val create_handler :
  ?config:config ->
  ctx_factory:(Kirin.Request.t -> 'a) ->
  'a Trpc_router.t -> Kirin.Request.t -> Kirin.Response.t
val routes :
  ?config:config ->
  ctx_factory:(Kirin.Request.t -> 'a) ->
  'a Trpc_router.t -> Kirin.Router.route list
val route :
  ?config:config ->
  ctx_factory:(Kirin.Request.t -> 'a) ->
  'a Trpc_router.t -> Kirin.Router.route
