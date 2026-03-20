type auth_info = {
  user_id : string;
  claims : Yojson.Safe.t option;
  token_type : string;
}
val auth_header : string
val current_auth : auth_info option ref
val set_auth_info : 'a -> auth_info -> 'a
val get_auth_info : 'a -> auth_info option
val get_user_id : 'a -> string option
val get_claims : 'a -> Yojson.Safe.t option
val extract_bearer_token : Kirin.Request.t -> string option
val jwt :
  secret:string ->
  ?on_error:(Kirin.Request.t -> string -> Kirin.Response.t) ->
  (Kirin.Request.t -> Kirin.Response.t) ->
  Kirin.Request.t -> Kirin.Response.t
val jwt_with_claims :
  secret:string ->
  extract_user_id:(Yojson.Safe.t -> string option) ->
  ?on_error:(Kirin.Request.t -> string -> Kirin.Response.t) ->
  (Kirin.Request.t -> Kirin.Response.t) ->
  Kirin.Request.t -> Kirin.Response.t
val session :
  store:Session.store ->
  ?user_key:string ->
  ?on_error:(Kirin.Request.t -> Kirin.Response.t) ->
  (Kirin.Request.t -> Kirin.Response.t) ->
  Kirin.Request.t -> Kirin.Response.t
type api_key_location = Header of string | Query of string | Bearer
type api_key_validator = string -> string option
val api_key :
  validate:(string -> string option) ->
  ?location:api_key_location ->
  ?on_error:(Kirin.Request.t -> Kirin.Response.t) ->
  (Kirin.Request.t -> Kirin.Response.t) ->
  Kirin.Request.t -> Kirin.Response.t
val optional_jwt :
  secret:string -> (Kirin.Request.t -> 'a) -> Kirin.Request.t -> 'a
val require_role :
  role_claim:string ->
  required_roles:string list ->
  ?on_error:('a -> Kirin.Response.t) ->
  ('a -> Kirin.Response.t) -> 'a -> Kirin.Response.t
val any_of :
  (('a -> Kirin.Response.t) -> 'b -> Kirin.Response.t) list ->
  ?on_error:('b -> Kirin.Response.t) ->
  ('a -> Kirin.Response.t) -> 'b -> Kirin.Response.t
