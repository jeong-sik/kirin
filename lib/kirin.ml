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

(** {1 Streaming I/O (Phase 9)} *)

(** Streaming response for large data
    Memory-efficient chunked transfer encoding.

    {[
      let download _req =
        Kirin.stream_file ~filename:"data.csv" "/path/to/large.csv"

      let generator _req =
        Kirin.stream (fun yield ->
          for i = 1 to 1000000 do
            yield (Printf.sprintf "Line %d\n" i)
          done)
    ]}
*)

(** Create streaming response from a chunk producer *)
let stream = Stream.response

(** Create streaming file download response *)
let stream_file = Stream.file_response

(** Create inline file response (displayed in browser) *)
let stream_file_inline = Stream.file_inline

(** Save uploaded file to disk efficiently *)
let save_upload = Stream.save_upload

(** Read request body in chunks *)
let read_chunks = Stream.read_chunks

(** Progress callback for file operations *)
type progress = Stream.progress_callback = {
  on_progress : bytes_sent:int -> total_bytes:int option -> unit;
  on_complete : unit -> unit;
  on_error : exn -> unit;
}

(** Create stderr progress reporter *)
let progress_stderr = Stream.stderr_progress

(** Silent progress (no-op) *)
let progress_silent = Stream.silent_progress

(** Convert streaming response to regular response (collects all data) *)
let stream_to_response = Stream.to_response

(** Streaming module for advanced use *)
module Stream = Stream

(** {1 Connection Pool (Phase 9)} *)

(** Generic connection pool for managing expensive resources.

    {[
      let db_pool = Kirin.Pool.create
        ~max_size:10
        ~create:(fun () -> Db.connect url)
        ~destroy:Db.close
        ()

      Kirin.Pool.use db_pool (fun conn ->
        Db.query conn "SELECT * FROM users")
    ]}
*)
module Pool = Pool

(** Pool configuration type *)
type pool_config = Pool.config = {
  min_size : int;
  max_size : int;
  idle_timeout : float;
  max_wait_time : float;
  health_check_interval : float;
}

(** Default pool configuration *)
let default_pool_config = Pool.default_config

(** Pool statistics type *)
type pool_stats = Pool.stats = {
  total_connections : int;
  active_connections : int;
  idle_connections : int;
  waiting_requests : int;
  total_acquisitions : int;
  total_timeouts : int;
  total_errors : int;
}

(** {1 Backpressure & Flow Control (Phase 9)} *)

(** Backpressure module for flow control.

    Prevents fast producers from overwhelming slow consumers.

    {[
      (* Rate-limited streaming *)
      let limiter = Kirin.Backpressure.RateLimiter.create ~rate:100.0 () in
      Stream.response (fun yield ->
        for i = 1 to 1000 do
          Kirin.Backpressure.RateLimiter.acquire limiter;
          yield (Printf.sprintf "Item %d\n" i)
        done)

      (* Bounded channel for producer-consumer *)
      let ch = Kirin.Backpressure.Channel.create ~capacity:100 () in
      Kirin.Backpressure.Channel.send ch data;
      let data = Kirin.Backpressure.Channel.recv ch in
    ]}
*)
module Backpressure = Backpressure

(** Backpressure strategy type *)
type backpressure_strategy = Backpressure.strategy =
  | Block
  | Drop_oldest
  | Drop_newest
  | Error

(** {1 In-Memory Cache (Phase 9)} *)

(** LRU cache with TTL support.

    {[
      let cache = Kirin.Cache.create ~max_size:1000 ~default_ttl:300.0 () in
      Kirin.Cache.set cache "key" "value";
      match Kirin.Cache.get cache "key" with
      | Some v -> v
      | None -> "default"
    ]}
*)
module Cache = Cache

(** Cache statistics type *)
type cache_stats = Cache.stats = {
  hits : int;
  misses : int;
  evictions : int;
  expirations : int;
  current_size : int;
  max_size : int;
}

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

(** {1 gRPC Integration (Phase 5)} *)

(** gRPC module for building gRPC services alongside HTTP

    @see <Grpc> for full API documentation
*)
module Grpc = Grpc

(** {1 GraphQL Integration (Phase 6)} *)

(** GraphQL module for building GraphQL APIs

    {b Quick Example:}
    {[
      open Kirin.Graphql

      let user = obj "User" ~fields:(fun _ -> [
        field "id" ~typ:(non_null string) ~args:[] ~resolve:(fun _ u -> u.id);
        field "name" ~typ:(non_null string) ~args:[] ~resolve:(fun _ u -> u.name);
      ])

      let schema = schema [
        field "user" ~typ:user
          ~args:Arg.[arg "id" ~typ:(non_null string)]
          ~resolve:(fun _ () id -> find_user id)
      ]

      let routes = Kirin.router [
        Kirin.post "/graphql" (Kirin.Graphql.handler schema);
        Kirin.get "/graphql" (Kirin.Graphql.playground_handler);
      ]
    ]}

    @see <Graphql_adapter> for full API documentation
*)
module Graphql = Graphql_adapter

(** {1 MCP Integration (Phase 8)} *)

(** MCP module for AI agent integration

    {b Quick Example - Server:}
    {[
      open Kirin.Mcp

      let mcp = Server.create () in
      Server.add_tool mcp
        ~name:"greet"
        ~description:"Greet a person"
        ~schema:(Schema.object_ [
          "name", Schema.string ~description:"Person's name" ()
        ] ~required:["name"])
        ~handler:(fun params ->
          let name = Yojson.Safe.Util.(params |> member "name" |> to_string) in
          `String (Printf.sprintf "Hello, %s!" name));

      let routes = Kirin.router (routes mcp @ [
        Kirin.get "/" (fun _ -> Kirin.html "MCP Server");
      ])
    ]}

    @see <Mcp_adapter> for full API documentation
*)
module Mcp = Mcp_adapter
