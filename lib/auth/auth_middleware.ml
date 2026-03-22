(** Authentication Middleware

    Middleware for protecting routes with various authentication strategies.
    Supports JWT Bearer tokens, Session-based auth, and API keys.

    Auth info is stored in the per-request Hmap context, so concurrent
    fibers serving different requests never share state.

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

(** Hmap key for storing auth_info in Request.ctx.
    Each request carries its own Hmap, eliminating data races
    that the previous global ref caused under concurrent fibers. *)
let auth_key : auth_info Hmap.key = Hmap.Key.create ()

(** Store auth info in the request context. Returns a new request. *)
let set_auth_info req info =
  let ctx = Hmap.add auth_key info (Kirin.Request.ctx req) in
  Kirin.Request.with_ctx ctx req

(** Get auth info from request context *)
let get_auth_info req =
  Hmap.find auth_key (Kirin.Request.ctx req)

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

(** Chain multiple auth methods (first success wins).

    Each auth middleware is probed with a lightweight sentinel handler to
    determine whether authentication succeeded (i.e. the sentinel was called
    with an auth-enriched request).  Only when a strategy succeeds is the
    real [handler] called -- exactly once -- preventing double execution of
    side-effectful handlers when a business-logic 4xx was previously
    misinterpreted as auth failure. *)
let any_of auth_middlewares ?(on_error = fun _req ->
    Kirin.Response.json ~status:`Unauthorized
      (`Assoc [("error", `String "Authentication required")])) handler =
  fun req ->
    (* Capture the auth-enriched request from a successful probe.
       Auth middlewares may modify the request (e.g. set_auth_info) before
       calling the inner handler, so we need to forward that modified
       request to the real handler.  The ref is typed via the sentinel
       closure to hold the enriched request type ('a). *)
    let authed_req : _ option ref = ref None in
    let sentinel enriched_req =
      authed_req := Some enriched_req;
      Kirin.Response.text "__auth_probe__"
    in
    let rec try_auth = function
      | [] -> on_error req
      | auth :: rest ->
          authed_req := None;
          let _probe_resp = auth sentinel req in
          match !authed_req with
          | Some req' ->
              (* Auth succeeded: sentinel was called with the enriched request
                 carrying auth_info in its Hmap context. *)
              handler req'
          | None ->
              (* Auth middleware never called sentinel -- auth failed. *)
              try_auth rest
    in
    try_auth auth_middlewares
