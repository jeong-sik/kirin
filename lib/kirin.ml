(** Kirin - OCaml 5.x Eio-native Web Framework

    Dream의 DX + Axum의 아키텍처 + Eio의 Direct-style

    {[
      let () = Kirin.start ~port:8000
        @@ Kirin.logger
        @@ Kirin.router [
             Kirin.get "/" (fun _ -> Kirin.html "Hello, Kirin!");
             Kirin.get "/hello/:name" (fun req ->
               let name = Kirin.param "name" req in
               Kirin.html ("Hello, " ^ name ^ "!"));
           ]
    ]}
*)

(** {1 Core Types} *)

(** Handler type - Direct style function from request to response *)
type handler = Request.t -> Response.t

(** Middleware type - Handler transformer *)
type middleware = handler -> handler

(** Route type *)
type route = Router.route

(** {1 Request Helpers} *)

(** Get path parameter by name *)
let param name req =
  match Request.param name req with
  | Some v -> v
  | None -> failwith ("Missing parameter: " ^ name)

(** Get optional path parameter *)
let param_opt = Request.param

(** Get query parameter by name *)
let query name req =
  match Request.query name req with
  | Some v -> v
  | None -> failwith ("Missing query parameter: " ^ name)

(** Get optional query parameter *)
let query_opt = Request.query

(** Get request body as string *)
let body = Request.body

(** Parse JSON body *)
let json_body = Request.json_body

(** Parse form body *)
let form_body = Request.form_body

(** Get header value *)
let header = Request.header

(** {1 Response Helpers} *)

(** Plain text response *)
let text = Response.text

(** HTML response *)
let html = Response.html

(** JSON response from Yojson.Safe.t *)
let json = Response.json

(** JSON response from string *)
let json_string = Response.json_string

(** Empty response with status *)
let empty = Response.empty

(** Redirect response *)
let redirect = Response.redirect

(** Permanent redirect *)
let redirect_permanent = Response.redirect_permanent

(** Not found response *)
let not_found = Response.not_found

(** Bad request response *)
let bad_request = Response.bad_request

(** Server error response *)
let server_error = Response.server_error

(** HTMX-specific response *)
let htmx = Response.htmx

(** Add header to response *)
let with_header = Response.with_header

(** Set response status *)
let with_status = Response.with_status

(** {1 Routing} *)

(** Create a router from routes *)
let router = Router.router

(** Dispatch request to router *)
let dispatch = Router.dispatch

(** Route for GET method *)
let get = Router.get

(** Route for POST method *)
let post = Router.post

(** Route for PUT method *)
let put = Router.put

(** Route for PATCH method *)
let patch = Router.patch

(** Route for DELETE method *)
let delete = Router.delete

(** Route for HEAD method *)
let head = Router.head

(** Route for OPTIONS method *)
let options = Router.options

(** Create scoped routes with prefix and middlewares *)
let scope = Router.scope

(** {1 Middleware} *)

(** Logger middleware - logs requests and responses *)
let logger = Middleware.logger

(** Error catching middleware *)
let catch = Middleware.catch

(** CORS middleware *)
let cors = Middleware.cors

(** Timing header middleware *)
let timing = Middleware.timing

(** Compose middlewares into a pipeline *)
let pipeline = Middleware.pipeline

(** {1 Server} *)

(** Start the server *)
let start = Server.start

(** Run with custom config (for advanced use) *)
let run = Server.run

(** {1 Configuration} *)

(** Server configuration *)
type config = Server.config

let default_config = Server.default_config

(** CORS configuration *)
type cors_config = Middleware.cors_config

let default_cors_config = Middleware.default_cors_config
