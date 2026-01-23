(** Authentication Middleware

    Middleware for protecting routes with various authentication strategies.
    Supports JWT Bearer tokens, Session-based auth, and API keys.

    {b Example:}
    {[
      (* JWT protected route *)
      let protected = Auth_middleware.jwt ~secret:"jwt-secret" in
      Router.[
        get "/api/profile" (protected (fun req ->
          let user_id = Auth_middleware.get_user_id req in
          (* ... *)
        ))
      ]
    ]}
*)

(** {1 Types} *)

(** Authentication result stored in request *)
type auth_info = {
  user_id : string;
  claims : Yojson.Safe.t option;
  token_type : string;  (** "jwt", "session", "api_key" *)
}

(** {1 Request Context} *)

(** Header for storing auth info *)
let auth_header = "X-Auth-Info"

(** Internal ref to pass auth info through handler chain *)
let current_auth : auth_info option ref = ref None

(** Store auth info for current request *)
let set_auth_info _req info =
  current_auth := Some info;
  _req  (* Return unchanged request *)

(** Get auth info from current request *)
let get_auth_info _req =
  !current_auth

(** Get user ID from authenticated request *)
let get_user_id req =
  match get_auth_info req with
  | Some info -> Some info.user_id
  | None -> None

(** Get claims from authenticated request *)
let get_claims req =
  match get_auth_info req with
  | Some info -> info.claims
  | None -> None

(** {1 JWT Authentication} *)

(** Extract Bearer token from Authorization header *)
let extract_bearer_token req =
  match Kirin.Request.header "Authorization" req with
  | None -> None
  | Some auth ->
      if String.length auth > 7 && String.sub auth 0 7 = "Bearer " then
        Some (String.sub auth 7 (String.length auth - 7))
      else
        None

(** JWT authentication middleware *)
let jwt ~secret ?(on_error = fun _req msg ->
    Kirin.Response.json ~status:`Unauthorized
      (`Assoc [("error", `String msg)])) handler =
  fun req ->
    current_auth := None;  (* Reset auth for each request *)
    match extract_bearer_token req with
    | None -> on_error req "Missing or invalid Authorization header"
    | Some token ->
        match Jwt.decode ~secret token with
        | Error msg -> on_error req msg
        | Ok decoded ->
            let user_id = Option.value ~default:"" decoded.Jwt.claims.sub in
            let info = { user_id; claims = Some decoded.Jwt.payload; token_type = "jwt" } in
            let req = set_auth_info req info in
            handler req

(** JWT middleware with custom claim extraction *)
let jwt_with_claims ~secret ~extract_user_id ?on_error handler =
  let on_error = Option.value on_error ~default:(fun _req msg ->
    Kirin.Response.json ~status:`Unauthorized (`Assoc [("error", `String msg)])) in
  fun req ->
    current_auth := None;  (* Reset auth for each request *)
    match extract_bearer_token req with
    | None -> on_error req "Missing Authorization header"
    | Some token ->
        match Jwt.decode ~secret token with
        | Error msg -> on_error req msg
        | Ok decoded ->
            match extract_user_id decoded.Jwt.payload with
            | None -> on_error req "Cannot extract user ID from token"
            | Some user_id ->
                let info = { user_id; claims = Some decoded.Jwt.payload; token_type = "jwt" } in
                let req = set_auth_info req info in
                handler req

(** {1 Session Authentication} *)

(** Session authentication middleware *)
let session ~store ?(user_key = "user_id") ?(on_error = fun _req ->
    Kirin.Response.redirect "/login") handler =
  fun req ->
    match Session.get_id req with
    | None -> on_error req
    | Some session_id ->
        match Session.get store session_id user_key with
        | None -> on_error req
        | Some user_id ->
            let info = { user_id; claims = None; token_type = "session" } in
            let req = set_auth_info req info in
            handler req

(** {1 API Key Authentication} *)

(** API key location *)
type api_key_location =
  | Header of string  (** Custom header name *)
  | Query of string   (** Query parameter name *)
  | Bearer            (** Authorization: Bearer <key> *)

(** API key validator type *)
type api_key_validator = string -> string option  (** Returns user_id if valid *)

(** API key authentication middleware *)
let api_key ~validate ?(location = Header "X-API-Key") ?(on_error = fun _req ->
    Kirin.Response.json ~status:`Unauthorized
      (`Assoc [("error", `String "Invalid API key")])) handler =
  fun req ->
    let key = match location with
      | Header name -> Kirin.Request.header name req
      | Query name -> Kirin.Request.query name req
      | Bearer -> extract_bearer_token req
    in
    match key with
    | None -> on_error req
    | Some k ->
        match validate k with
        | None -> on_error req
        | Some user_id ->
            let info = { user_id; claims = None; token_type = "api_key" } in
            let req = set_auth_info req info in
            handler req

(** {1 Optional Authentication} *)

(** Try to authenticate but don't require it *)
let optional_jwt ~secret handler =
  fun req ->
    match extract_bearer_token req with
    | None -> handler req
    | Some token ->
        match Jwt.decode ~secret token with
        | Error _ -> handler req  (* Continue without auth *)
        | Ok decoded ->
            let user_id = Option.value ~default:"" decoded.claims.sub in
            let info = { user_id; claims = Some decoded.payload; token_type = "jwt" } in
            let req = set_auth_info req info in
            handler req

(** {1 Role-Based Access Control} *)

(** Check if user has required role *)
let require_role ~role_claim ~required_roles ?(on_error = fun _req ->
    Kirin.Response.json ~status:`Forbidden
      (`Assoc [("error", `String "Insufficient permissions")])) handler =
  fun req ->
    match get_claims req with
    | None -> on_error req
    | Some claims ->
        let user_roles =
          let open Yojson.Safe.Util in
          match claims |> member role_claim with
          | `List roles -> List.filter_map to_string_option roles
          | `String role -> [role]
          | _ -> []
        in
        if List.exists (fun r -> List.mem r user_roles) required_roles then
          handler req
        else
          on_error req

(** {1 Composite Middleware} *)

(** Chain multiple auth methods (first success wins) *)
let any_of auth_middlewares ?(on_error = fun _req ->
    Kirin.Response.json ~status:`Unauthorized
      (`Assoc [("error", `String "Authentication required")])) handler =
  fun req ->
    let rec try_auth = function
      | [] -> on_error req
      | auth :: rest ->
          let resp = auth (fun req ->
            (* Check if auth was successful *)
            match get_auth_info req with
            | Some _ -> handler req
            | None -> Kirin.Response.text ""  (* Dummy, shouldn't happen *)
          ) req in
          (* Check if it was an error response *)
          if Http.Status.to_int (Kirin.Response.status resp) >= 400 then
            try_auth rest
          else
            resp
    in
    try_auth auth_middlewares
