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

(** {1 Response Inspection (for testing)} *)

(** Get response body *)
let response_body = Response.body

(** Get response status code *)
let response_status = Response.status_code

(** Get response header *)
let response_header = Response.header

(** {1 Static File Serving} *)

(** Static file middleware

    {[
      Kirin.start ~port:3000
      @@ Kirin.logger
      @@ Kirin.static "/public" ~dir:"./static"
      @@ routes
    ]}
*)
let static = Static.static

(** {1 Cookie Handling} *)

(** Get a cookie from request *)
let cookie = Cookie.get

(** Get all cookies from request *)
let cookies = Cookie.get_all

(** Set a cookie on response *)
let set_cookie = Cookie.set

(** Delete a cookie *)
let delete_cookie = Cookie.delete

(** Get a signed cookie (verified) *)
let cookie_signed = Cookie.get_signed

(** Set a signed cookie *)
let set_cookie_signed = Cookie.set_signed

(** Set cookie signing secret (call at startup) *)
let set_cookie_secret = Cookie.set_secret

(** {1 Multipart Form Data (RFC 7578)} *)

(** Parse multipart form data from request *)
let multipart = Multipart.from_request

(** Get field value from multipart data *)
let multipart_field = Multipart.field

(** Get uploaded file from multipart data *)
let multipart_file = Multipart.file

(** Get all uploaded files *)
let multipart_files = Multipart.files

(** Get all form fields *)
let multipart_fields = Multipart.fields

(** {1 Response Compression} *)

(** Compression middleware (gzip/deflate) *)
let compress = Compress.middleware

(** Compress a string using gzip *)
let compress_gzip = Compress.compress_gzip

(** Compress a string using deflate *)
let compress_deflate = Compress.compress_deflate

(** {1 Rate Limiting} *)

(** Rate limiting middleware *)
let rate_limit = Ratelimit.middleware

(** Rate limit configuration *)
type rate_limit_config = Ratelimit.config = {
  requests_per_second : float;
  burst_size : int;
}

(** Default rate limit config (10 req/s, burst 20) *)
let default_rate_limit_config = Ratelimit.default_config

(** {1 ETag Support} *)

(** ETag middleware for automatic caching *)
let etag = Etag.middleware

(** Generate ETag from content *)
let generate_etag = Etag.generate

(** Add ETag header to response *)
let with_etag etag resp =
  Response.with_header "etag" (Etag.to_string etag) resp

(** {1 Internal Modules (for advanced use/testing)} *)

module Request = Request
module Response = Response
module Router = Router
module Middleware = Middleware
module Static = Static
module Cookie = Cookie
module Etag = Etag
module Multipart = Multipart
module Compress = Compress
module Ratelimit = Ratelimit
