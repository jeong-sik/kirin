(** Kirin - OCaml 5.x Eio-native Web Framework

    Dream's DX + Axum's Architecture + Eio's Direct-style

    {b Features:}
    - Direct-style async (no Lwt monads)
    - Type-safe routing with path parameters
    - Middleware composition using [@@]
    - WebSocket support (RFC 6455)
    - Server-Sent Events (SSE)
    - Mustache-like template engine
    - HMAC-signed cookies
    - Multipart form-data parsing
    - Static file serving
    - Rate limiting (token bucket)
    - Gzip/deflate compression
    - ETag caching

    {b Quick Start:}
    {[
      let () = Kirin.start ~port:8000
        @@ Kirin.logger
        @@ Kirin.router [
             Kirin.get "/" (fun _ -> Kirin.html "Hello, Kirin!");
             Kirin.get "/hello/:name" (fun req ->
               let name = Kirin.param "name" req in
               Kirin.html ("Hello, " ^ name ^ "!"));
             Kirin.get "/api/data" (fun _ ->
               Kirin.json (`Assoc [("status", `String "ok")]));
           ]
    ]}

    {b Middleware Stack:}
    {[
      let () = Kirin.start ~port:8000
        @@ Kirin.logger      (* Log requests/responses *)
        @@ Kirin.timing      (* X-Response-Time header *)
        @@ Kirin.cors ()     (* CORS headers *)
        @@ Kirin.compress    (* Gzip/deflate *)
        @@ Kirin.etag        (* ETag caching *)
        @@ Kirin.rate_limit  (* Rate limiting *)
        @@ routes
    ]}

    @see <https://github.com/jeong-sik/kirin> GitHub Repository
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

(** {1 WebSocket Support (RFC 6455)} *)

(** Check if request is a WebSocket upgrade request *)
let is_websocket_upgrade = Websocket.is_upgrade_request

(** Create WebSocket upgrade response *)
let websocket_upgrade = Websocket.upgrade_response

(** WebSocket middleware for handling upgrade requests *)
let websocket = Websocket.middleware

(** Create a text WebSocket frame *)
let ws_text = Websocket.text_frame

(** Create a binary WebSocket frame *)
let ws_binary = Websocket.binary_frame

(** Create a ping frame *)
let ws_ping = Websocket.ping_frame

(** Create a pong frame *)
let ws_pong = Websocket.pong_frame

(** Create a close frame *)
let ws_close = Websocket.close_frame

(** Encode WebSocket frame for sending *)
let ws_encode = Websocket.encode_frame

(** Decode WebSocket frame from received data *)
let ws_decode = Websocket.decode_frame

(** WebSocket opcodes *)
type ws_opcode = Websocket.opcode =
  | Continuation
  | Text
  | Binary
  | Close
  | Ping
  | Pong

(** WebSocket frame *)
type ws_frame = Websocket.frame = {
  fin : bool;
  opcode : ws_opcode;
  payload : string;
}

(** WebSocket close codes *)
type ws_close_code = Websocket.close_code =
  | Normal
  | GoingAway
  | ProtocolError
  | UnsupportedData
  | InvalidPayload
  | PolicyViolation
  | MessageTooBig
  | InternalError

(** Default echo handler *)
let ws_echo_handler = Websocket.echo_handler

(** {1 Server-Sent Events (SSE)} *)

(** SSE event type *)
type sse_event = Sse.event = {
  event : string option;
  data : string;
  id : string option;
  retry : int option;
}

(** Create a simple data-only SSE event *)
let sse_data = Sse.data

(** Create an SSE event with type *)
let sse_event = Sse.event

(** Add ID to SSE event *)
let sse_with_id = Sse.with_id

(** Add retry interval to SSE event *)
let sse_with_retry = Sse.with_retry

(** Encode SSE event to string *)
let sse_encode = Sse.encode

(** Create SSE response with events *)
let sse_response = Sse.response

(** SSE endpoint handler *)
let sse_handler = Sse.handler

(** SSE middleware *)
let sse = Sse.middleware

(** SSE keep-alive ping *)
let sse_ping = Sse.ping

(** Get Last-Event-ID header *)
let sse_last_id = Sse.last_event_id

(** {1 HTML Template Engine} *)

(** Template context type *)
type template_context = Template.context

(** Empty template context *)
let template_context_empty = Template.empty_context

(** Create context from string pairs *)
let template_context = Template.context

(** Create context from Yojson values *)
let template_context_of = Template.context_of

(** Render template with context *)
let template_render = Template.render

(** Render template to HTML response *)
let template_html = Template.html

(** Simple string interpolation *)
let template_interpolate = Template.interpolate

(** HTML escape string *)
let html_escape = Template.html_escape

(** {1 TLS/HTTPS Configuration} *)

(** TLS configuration type *)
type tls_config = Tls_config.t

(** TLS error type *)
type tls_error = Tls_config.error

(** Create TLS config from certificate and key files *)
let tls_config = Tls_config.make

(** Create TLS config from PEM strings *)
let tls_from_pem = Tls_config.from_pem

(** Development TLS config (self-signed, DO NOT USE IN PRODUCTION) *)
let tls_dev = Tls_config.dev_config

(** Validate TLS config *)
let tls_validate = Tls_config.validate

(** TLS error to string *)
let tls_error_string = Tls_config.error_to_string

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
module Websocket = Websocket
module Sse = Sse
module Template = Template
module Tls_config = Tls_config
