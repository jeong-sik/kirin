(** Kirin - OCaml 5.x Eio-native Web Framework

    Direct-style async web framework with type-safe routing,
    middleware composition, and SSR integrations.

    {[
      let () = Kirin.start ~port:8000
        @@ Kirin.logger
        @@ Kirin.router [
             Kirin.get "/" (fun _ -> Kirin.html "Hello, Kirin!");
           ]
    ]}

    For SSR adapters, import directly: [Kirin_react], [Kirin_vue], etc.
    For sub-modules, use qualified paths: [Kirin.Cookie.get], [Kirin.Websocket.middleware], etc.
*)

(** {1 Core Types} *)

type handler = Request.t -> Response.t
type middleware = handler -> handler
type route = Router.route

(** {1 Request Helpers} *)

let param name req =
  match Request.param name req with
  | Some v -> v
  | None -> failwith ("Missing parameter: " ^ name)

let param_opt = Request.param

let query name req =
  match Request.query name req with
  | Some v -> v
  | None -> failwith ("Missing query parameter: " ^ name)

let query_opt = Request.query
let body = Request.body
let json_body = Request.json_body
let form_body = Request.form_body
let header = Request.header

(** {1 Response Helpers} *)

let text = Response.text
let html = Response.html
let json = Response.json
let json_string = Response.json_string
let empty = Response.empty
let redirect = Response.redirect
let redirect_permanent = Response.redirect_permanent
let not_found = Response.not_found
let bad_request = Response.bad_request
let server_error = Response.server_error
let htmx = Response.htmx
let with_header = Response.with_header
let with_status = Response.with_status

(** {1 Response Inspection} *)

let response_body = Response.body
let response_status = Response.status_code
let response_header = Response.header

(** {1 Routing} *)

let router = Router.router
let dispatch = Router.dispatch
let get = Router.get
let post = Router.post
let put = Router.put
let patch = Router.patch
let delete = Router.delete
let head = Router.head
let options = Router.options
let scope = Router.scope

(** {1 Middleware} *)

let logger = Middleware.logger
let catch = Middleware.catch
let cors = Middleware.cors
let timing = Middleware.timing
let pipeline = Middleware.pipeline

(** {1 Server} *)

let start = Server.start
let run = Server.run

(** {1 Configuration} *)

type config = Server.config
let default_config = Server.default_config
type cors_config = Middleware.cors_config
let default_cors_config = Middleware.default_cors_config

(** {1 Static Files} *)

let static = Static.static

(** {1 Cookie Shortcuts} *)

let cookie = Cookie.get
let cookies = Cookie.get_all
let set_cookie = Cookie.set
let delete_cookie = Cookie.delete
let cookie_signed = Cookie.get_signed
let set_cookie_signed = Cookie.set_signed
let set_cookie_secret = Cookie.set_secret

(** {1 Compression} *)

let compress = Compress.middleware
let compress_gzip = Compress.compress_gzip
let compress_deflate = Compress.compress_deflate

(** {1 Rate Limiting} *)

let rate_limit = Ratelimit.middleware

type rate_limit_config = Ratelimit.config = {
  requests_per_second : float;
  burst_size : int;
}

let default_rate_limit_config = Ratelimit.default_config

(** {1 ETag} *)

let etag = Etag.middleware
let generate_etag = Etag.generate
let with_etag etag resp =
  Response.with_header "etag" (Etag.to_string etag) resp

(** {1 WebSocket Shortcuts} *)

let is_websocket_upgrade = Websocket.is_upgrade_request
let websocket_upgrade = Websocket.upgrade_response
let websocket = Websocket.middleware
let ws_text = Websocket.text_frame
let ws_binary = Websocket.binary_frame
let ws_ping = Websocket.ping_frame
let ws_pong = Websocket.pong_frame
let ws_close = Websocket.close_frame
let ws_encode = Websocket.encode_frame
let ws_decode = Websocket.decode_frame
let ws_echo_handler = Websocket.echo_handler

type ws_opcode = Websocket.opcode =
  | Continuation | Text | Binary | Close | Ping | Pong

(** {1 SSE Shortcuts} *)

let sse_data = Sse.data
let sse_event = Sse.event
let sse_with_id = Sse.with_id
let sse_with_retry = Sse.with_retry
let sse_encode = Sse.encode
let sse_response = Sse.response_legacy
let sse_stream_response = Sse.response
let sse_handler = Sse.handler
let sse = Sse.middleware
let sse_ping = Sse.ping
let sse_last_id = Sse.last_event_id

(** {1 Streaming} *)

let stream = Stream.response
let stream_file = Stream.file_response
let stream_file_inline = Stream.file_inline
let save_upload = Stream.save_upload
let read_chunks = Stream.read_chunks
let stream_to_response = Stream.to_response

(** {1 Template Shortcuts} *)

type template_context = Template.context
let template_context_empty = Template.empty_context
let template_context = Template.context
let template_context_of = Template.context_of
let template_render = Template.render
let template_html = Template.html
let template_interpolate = Template.interpolate
let html_escape = Template.html_escape

(** {1 TLS Shortcuts} *)

let tls_config = Tls_config.make
let tls_from_pem = Tls_config.from_pem
let tls_dev = Tls_config.dev_config
let tls_validate = Tls_config.validate
let tls_error_string = Tls_config.error_to_string

(** {1 Validation} *)

let validated = Validation.validated

(** {1 Modules} *)

module Request = Request
module Response = Response
module Router = Router
module Middleware = Middleware
module Server = Server
module Static = Static
module Fs_compat = Fs_compat
module Time_compat = Time_compat
module Cookie = Cookie
module Etag = Etag
module Multipart = Multipart
module Compress = Compress
module Ratelimit = Ratelimit
module Template = Template
module Tls_config = Tls_config
module Trace = Trace
module Stream = Stream
module Pool = Pool
module Backpressure = Backpressure
module Cache = Cache
module Jobs = Jobs
module Parallel = Parallel
module Websocket = Websocket
module Sse = Sse
module Logger = Logger
module Cluster = Cluster
module Health = Health
module Metrics = Metrics
module Shutdown = Shutdown
module Db = Db
module Migrate = Migrate
module Query = Query
module Db_events = Db_events
module Graphql = Graphql_adapter
module Graphql_relay = Graphql_relay
module Openapi = Openapi
module Grpc = Grpc
module Mcp = Mcp_adapter
module WebRTC = Webrtc_adapter

type webrtc_connection_state = Webrtc_adapter.connection_state =
  | New | Connecting | Connected | Disconnected | Failed | Closed
module Sync = Sync
module I18n = I18n
module Validation = Validation
module Testing = Testing
