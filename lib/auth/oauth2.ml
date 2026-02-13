(** OAuth2 Client

    OAuth2 Authorization Code flow implementation.
    Supports common providers (Google, GitHub, etc.) and custom providers.

    {b Example:}
    {[
      let google = Oauth2.Provider.google
        ~client_id:"xxx.apps.googleusercontent.com"
        ~client_secret:"xxx"
        ~redirect_uri:"https://myapp.com/auth/callback" in

      (* Step 1: Redirect to authorization *)
      let auth_url = Oauth2.authorization_url google ~scope:["openid"; "email"] in

      (* Step 2: Handle callback *)
      let code = Kirin.Request.query req "code" in
      let tokens = Oauth2.exchange_code google code in
      let user_info = Oauth2.get_user_info google tokens.access_token
    ]}
*)

(** {1 Types} *)

(** OAuth2 provider configuration *)
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

(** Token response *)
type tokens = {
  access_token : string;
  token_type : string;
  expires_in : int option;
  refresh_token : string option;
  scope : string option;
  id_token : string option;  (** OpenID Connect *)
}

(** User info (common fields) *)
type user_info = {
  id : string;
  email : string option;
  name : string option;
  picture : string option;
  raw : Yojson.Safe.t;  (** Full response *)
}

(** {1 Provider Builders} *)

module Provider = struct
  (** Create custom provider *)
  let create ~name ~client_id ~client_secret ~authorize_url ~token_url
      ?userinfo_url ~redirect_uri ?(scopes = []) () =
    { name; client_id; client_secret; authorize_url; token_url;
      userinfo_url; redirect_uri; scopes }

  (** Google OAuth2 *)
  let google ~client_id ~client_secret ~redirect_uri =
    create ~name:"google"
      ~client_id ~client_secret
      ~authorize_url:"https://accounts.google.com/o/oauth2/v2/auth"
      ~token_url:"https://oauth2.googleapis.com/token"
      ~userinfo_url:"https://www.googleapis.com/oauth2/v2/userinfo"
      ~redirect_uri
      ~scopes:["openid"; "email"; "profile"]
      ()

  (** GitHub OAuth2 *)
  let github ~client_id ~client_secret ~redirect_uri =
    create ~name:"github"
      ~client_id ~client_secret
      ~authorize_url:"https://github.com/login/oauth/authorize"
      ~token_url:"https://github.com/login/oauth/access_token"
      ~userinfo_url:"https://api.github.com/user"
      ~redirect_uri
      ~scopes:["read:user"; "user:email"]
      ()

  (** Discord OAuth2 *)
  let discord ~client_id ~client_secret ~redirect_uri =
    create ~name:"discord"
      ~client_id ~client_secret
      ~authorize_url:"https://discord.com/api/oauth2/authorize"
      ~token_url:"https://discord.com/api/oauth2/token"
      ~userinfo_url:"https://discord.com/api/users/@me"
      ~redirect_uri
      ~scopes:["identify"; "email"]
      ()

  (** Microsoft/Azure AD OAuth2 *)
  let microsoft ~client_id ~client_secret ~redirect_uri ?(tenant = "common") () =
    create ~name:"microsoft"
      ~client_id ~client_secret
      ~authorize_url:(Printf.sprintf "https://login.microsoftonline.com/%s/oauth2/v2.0/authorize" tenant)
      ~token_url:(Printf.sprintf "https://login.microsoftonline.com/%s/oauth2/v2.0/token" tenant)
      ~userinfo_url:"https://graph.microsoft.com/v1.0/me"
      ~redirect_uri
      ~scopes:["openid"; "email"; "profile"]
      ()
end

(** {1 Authorization} *)

(** Generate state parameter for CSRF protection *)
let generate_state () =
  (* 32 random bytes -> 43-char base64url (no padding) *)
  Secure_random.random_base64url 32

(** Build authorization URL *)
let authorization_url ?(scope = []) ?(state = generate_state ())
    ?(extra_params = []) provider =
  let scopes = if scope = [] then provider.scopes else scope in
  let params = [
    ("client_id", provider.client_id);
    ("redirect_uri", provider.redirect_uri);
    ("response_type", "code");
    ("scope", String.concat " " scopes);
    ("state", state);
  ] @ extra_params in

  let query = params
    |> List.map (fun (k, v) -> k ^ "=" ^ Uri.pct_encode v)
    |> String.concat "&"
  in
  (provider.authorize_url ^ "?" ^ query, state)

(** {1 Token Exchange} *)

(** Parse token response *)
let parse_tokens json =
  let open Yojson.Safe.Util in
  {
    access_token = json |> member "access_token" |> to_string;
    token_type = json |> member "token_type" |> to_string_option |> Option.value ~default:"Bearer";
    expires_in = json |> member "expires_in" |> to_int_option;
    refresh_token = json |> member "refresh_token" |> to_string_option;
    scope = json |> member "scope" |> to_string_option;
    id_token = json |> member "id_token" |> to_string_option;
  }

(** Exchange authorization code for tokens (blocking, needs HTTP client) *)
let exchange_code_params provider code =
  [
    ("grant_type", "authorization_code");
    ("code", code);
    ("client_id", provider.client_id);
    ("client_secret", provider.client_secret);
    ("redirect_uri", provider.redirect_uri);
  ]

(** Build token request body *)
let token_request_body provider code =
  exchange_code_params provider code
  |> List.map (fun (k, v) -> k ^ "=" ^ Uri.pct_encode v)
  |> String.concat "&"

(** Refresh access token params *)
let refresh_token_params provider refresh_token =
  [
    ("grant_type", "refresh_token");
    ("refresh_token", refresh_token);
    ("client_id", provider.client_id);
    ("client_secret", provider.client_secret);
  ]

(** {1 User Info} *)

(** Parse user info (provider-specific) *)
let parse_user_info (provider : provider) json =
  let (provider_name : string) = provider.name in
  let open Yojson.Safe.Util in
  let id, email, user_name, picture = match provider_name with
    | "google" ->
        (json |> member "id" |> to_string,
         json |> member "email" |> to_string_option,
         json |> member "name" |> to_string_option,
         json |> member "picture" |> to_string_option)
    | "github" ->
        (json |> member "id" |> to_int |> string_of_int,
         json |> member "email" |> to_string_option,
         json |> member "name" |> to_string_option,
         json |> member "avatar_url" |> to_string_option)
    | "discord" ->
        (json |> member "id" |> to_string,
         json |> member "email" |> to_string_option,
         json |> member "username" |> to_string_option,
         (let discord_id = json |> member "id" |> to_string in
          let avatar = json |> member "avatar" |> to_string_option in
          Option.map (fun a -> Printf.sprintf "https://cdn.discordapp.com/avatars/%s/%s.png" discord_id a) avatar))
    | "microsoft" ->
        (json |> member "id" |> to_string,
         json |> member "mail" |> to_string_option,
         json |> member "displayName" |> to_string_option,
         None)
    | _ ->
        (json |> member "id" |> to_string,
         json |> member "email" |> to_string_option,
         json |> member "name" |> to_string_option,
         json |> member "picture" |> to_string_option)
  in
  { id; email; name = user_name; picture; raw = json }

(** {1 PKCE Support} *)

(** Generate PKCE code verifier *)
let generate_code_verifier () =
  (* 48 random bytes -> 64-char base64url verifier (within PKCE limits). *)
  Secure_random.random_base64url 48

(** Generate PKCE code challenge (S256 method) *)
let generate_code_challenge verifier =
  let hash = Digestif.SHA256.digest_string verifier in
  Base64.encode_string ~pad:false ~alphabet:Base64.uri_safe_alphabet
    (Digestif.SHA256.to_raw_string hash)

(** Authorization URL with PKCE *)
let authorization_url_pkce ?(scope = []) ?(state = generate_state ())
    ?(extra_params = []) provider =
  let verifier = generate_code_verifier () in
  let challenge = generate_code_challenge verifier in
  let params = extra_params @ [
    ("code_challenge", challenge);
    ("code_challenge_method", "S256");
  ] in
  let (url, state) = authorization_url ~scope ~state ~extra_params:params provider in
  (url, state, verifier)

(** Token exchange params with PKCE *)
let exchange_code_params_pkce provider code verifier =
  exchange_code_params provider code @ [("code_verifier", verifier)]

(** {1 Route Helpers} *)

(** Create login route handler *)
let login_handler provider ?(scope = []) () =
  fun _req ->
    let (url, state) = authorization_url ~scope provider in
    (* Store state in session for verification *)
    let attrs = Kirin.Cookie.{
      default_attributes with
      http_only = true;
      same_site = Some `Lax;
      max_age = Some 600;
    } in
    Kirin.Response.redirect url
    |> Kirin.Cookie.set ~attrs "oauth_state" state

(** Verify state parameter *)
let verify_state req state =
  match Kirin.Cookie.get "oauth_state" req with
  | Some stored when stored = state -> true
  | _ -> false
