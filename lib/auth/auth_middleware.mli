(** Authentication middleware.

    Auth info is stored per-request in [Kirin.Request.ctx] via [Hmap],
    so concurrent fibers never share mutable state. *)

type auth_info =
  { user_id : string
  ; claims : Yojson.Safe.t option
  ; token_type : string
  }

val auth_header : string

(** Hmap key used to store [auth_info] in the request context. *)
val auth_key : auth_info Hmap.key

val set_auth_info : Kirin.Request.t -> auth_info -> Kirin.Request.t
val get_auth_info : Kirin.Request.t -> auth_info option
val get_user_id : Kirin.Request.t -> string option
val get_claims : Kirin.Request.t -> Yojson.Safe.t option
val extract_bearer_token : Kirin.Request.t -> string option

val jwt
  :  secret:string
  -> ?on_error:(Kirin.Request.t -> string -> Kirin.Response.t)
  -> (Kirin.Request.t -> Kirin.Response.t)
  -> Kirin.Request.t
  -> Kirin.Response.t

val jwt_with_claims
  :  secret:string
  -> extract_user_id:(Yojson.Safe.t -> string option)
  -> ?on_error:(Kirin.Request.t -> string -> Kirin.Response.t)
  -> (Kirin.Request.t -> Kirin.Response.t)
  -> Kirin.Request.t
  -> Kirin.Response.t

val session
  :  store:Session.store
  -> ?user_key:string
  -> ?on_error:(Kirin.Request.t -> Kirin.Response.t)
  -> (Kirin.Request.t -> Kirin.Response.t)
  -> Kirin.Request.t
  -> Kirin.Response.t

type api_key_location =
  | Header of string
  | Query of string
  | Bearer

type api_key_validator = string -> string option

val api_key
  :  validate:(string -> string option)
  -> ?location:api_key_location
  -> ?on_error:(Kirin.Request.t -> Kirin.Response.t)
  -> (Kirin.Request.t -> Kirin.Response.t)
  -> Kirin.Request.t
  -> Kirin.Response.t

val optional_jwt
  :  secret:string
  -> (Kirin.Request.t -> Kirin.Response.t)
  -> Kirin.Request.t
  -> Kirin.Response.t

val require_role
  :  role_claim:string
  -> required_roles:string list
  -> ?on_error:(Kirin.Request.t -> Kirin.Response.t)
  -> (Kirin.Request.t -> Kirin.Response.t)
  -> Kirin.Request.t
  -> Kirin.Response.t

val any_of
  :  ((Kirin.Request.t -> Kirin.Response.t) -> Kirin.Request.t -> Kirin.Response.t) list
  -> ?on_error:(Kirin.Request.t -> Kirin.Response.t)
  -> (Kirin.Request.t -> Kirin.Response.t)
  -> Kirin.Request.t
  -> Kirin.Response.t
