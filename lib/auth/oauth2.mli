type provider = {
  name : string;
  client_id : string;
  client_secret : string;
  authorize_url : string;
  token_url : string;
  userinfo_url : string option;
  redirect_uri : string;
  scopes : string list;
}
type tokens = {
  access_token : string;
  token_type : string;
  expires_in : int option;
  refresh_token : string option;
  scope : string option;
  id_token : string option;
}
type user_info = {
  id : string;
  email : string option;
  name : string option;
  picture : string option;
  raw : Yojson.Safe.t;
}
module Provider :
  sig
    val create :
      name:string ->
      client_id:string ->
      client_secret:string ->
      authorize_url:string ->
      token_url:string ->
      ?userinfo_url:string ->
      redirect_uri:string -> ?scopes:string list -> unit -> provider
    val google :
      client_id:string ->
      client_secret:string -> redirect_uri:string -> provider
    val github :
      client_id:string ->
      client_secret:string -> redirect_uri:string -> provider
    val discord :
      client_id:string ->
      client_secret:string -> redirect_uri:string -> provider
    val microsoft :
      client_id:string ->
      client_secret:string ->
      redirect_uri:string -> ?tenant:string -> unit -> provider
  end
val generate_state : unit -> string
val authorization_url :
  ?scope:string list ->
  ?state:string ->
  ?extra_params:(string * string) list -> provider -> string * string
val parse_tokens : Yojson__Safe.t -> tokens
val exchange_code_params : provider -> string -> (string * string) list
val token_request_body : provider -> string -> string
val refresh_token_params : provider -> string -> (string * string) list
val parse_user_info : provider -> Yojson__Safe.t -> user_info
val generate_code_verifier : unit -> string
val generate_code_challenge : String.t -> string
val authorization_url_pkce :
  ?scope:string list ->
  ?state:string ->
  ?extra_params:(string * string) list ->
  provider -> string * string * string
val exchange_code_params_pkce :
  provider -> string -> string -> (string * string) list
val login_handler :
  provider -> ?scope:string list -> ?secure:bool -> unit -> 'a -> Kirin.Response.t
(** [login_handler provider ?scope ?secure ()] returns the handler.
    [secure] defaults to [true]: the [oauth_state] cookie is the sole
    CSRF guard for the callback, so it must never travel cleartext.
    Local-dev callers on plain HTTP can opt out with [~secure:false]. *)

val verify_state : Kirin.Request.t -> string -> bool
(** Constant-time comparison of the [oauth_state] cookie against the
    [state] value returned by the OAuth provider. Avoids the prefix-leak
    timing oracle that OCaml's [=] would expose on every callback. *)
