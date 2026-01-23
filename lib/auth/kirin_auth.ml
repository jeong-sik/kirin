(** Kirin Auth - Authentication Module

    Complete authentication solution for Kirin web framework.

    {b Features:}
    - JWT (JSON Web Tokens) with HS256/384/512
    - Session management with memory/custom stores
    - Password hashing (PBKDF2-SHA256)
    - OAuth2 (Google, GitHub, Discord, Microsoft)
    - CSRF protection
    - Auth middleware for protected routes

    {b Quick Start:}
    {[
      open Kirin_auth

      (* JWT authentication *)
      let secret = "your-secret-key" in

      (* Generate token *)
      let token = Jwt.encode ~secret
        ~payload:(`Assoc [("role", `String "admin")])
        ~sub:"user-123"
        ~exp:(Unix.time () +. 3600.)
        () in

      (* Protected route *)
      let protected = Auth_middleware.jwt ~secret in
      Router.[
        get "/api/me" (protected (fun req ->
          let user_id = Auth_middleware.get_user_id req in
          Response.json (`Assoc [("user", `String (Option.get user_id))])
        ))
      ]
    ]}
*)

(** JWT token handling *)
module Jwt = Jwt

(** Session management *)
module Session = Session

(** Password hashing *)
module Password = Password

(** OAuth2 client *)
module Oauth2 = Oauth2

(** CSRF protection *)
module Csrf = Csrf

(** Authentication middleware *)
module Middleware = Auth_middleware
