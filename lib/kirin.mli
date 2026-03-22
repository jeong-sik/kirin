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

val param : string -> Request.t -> string
val param_opt : string -> Request.t -> string option
val query : string -> Request.t -> string
val query_opt : string -> Request.t -> string option
val body : Request.t -> string
val json_body :
  Request.t ->
  (Yojson.Safe.t, [> `Json_parse_error of string ]) result
val form_body : Request.t -> (string * string list) list
val header : string -> Request.t -> string option

(** {1 Response Helpers} *)

val text : ?status:Http.Status.t -> string -> Response.t
val html :
  ?status:Http.Status.t -> ?doctype:bool -> string -> Response.t
val json : ?status:Http.Status.t -> Yojson.Safe.t -> Response.t
val json_string : ?status:Http.Status.t -> string -> Response.t
val empty : Http.Status.t -> Response.t
val redirect : ?status:Http.Status.t -> string -> Response.t
val redirect_permanent : string -> Response.t
val not_found : ?body:string -> unit -> Response.t
val bad_request : ?body:string -> unit -> Response.t
val server_error : ?body:string -> unit -> Response.t
val htmx :
  ?status:Http.Status.t ->
  ?target:string -> ?swap:string -> string -> Response.t
val with_header : string -> string -> Response.t -> Response.t
val with_status : Http.Status.t -> Response.t -> Response.t

(** {1 Response Inspection} *)

val response_body : Response.t -> Response.body
val response_status : Response.t -> int
val response_header : string -> Response.t -> string option

(** {1 Routing} *)

val router : Router.t -> Router.handler
val dispatch : Router.t -> Request.t -> Response.t
val get : string -> Router.handler -> Router.route
val post : string -> Router.handler -> Router.route
val put : string -> Router.handler -> Router.route
val patch : string -> Router.handler -> Router.route
val delete : string -> Router.handler -> Router.route
val head : string -> Router.handler -> Router.route
val options : string -> Router.handler -> Router.route
val scope :
  string ->
  (Router.handler -> Router.handler) list ->
  Router.route list -> Router.route list

(** {1 Middleware} *)

val logger : Middleware.t
val catch : (exn -> Request.t -> Response.t) -> Middleware.t
val cors : ?config:Middleware.cors_config -> unit -> Middleware.t
val timing : Middleware.t
val pipeline : Middleware.t list -> Middleware.t

(** {1 Server} *)

val start :
  ?port:int ->
  ?request_timeout:float ->
  ?stream_read_timeout:float ->
  ?max_body_size:int ->
  ?domains:int -> Router.handler -> unit
val run :
  ?config:Server.config ->
  sw:Eio.Switch.t ->
  env:< clock : [> float Eio.Time.clock_ty ] Eio.Time.clock;
        net : [> [> `Generic ] Eio.Net.ty ] Eio.Net.t; .. > ->
  Router.handler -> unit

(** {1 Configuration} *)

type config = Server.config
val default_config : Server.config
type cors_config = Middleware.cors_config
val default_cors_config : Middleware.cors_config

(** {1 Static Files} *)

val static :
  string -> dir:string ->
  (Request.t -> Response.t) -> Request.t -> Response.t

(** {1 Cookie Shortcuts} *)

val cookie : string -> Request.t -> string option
val cookies : Request.t -> (string * string) list
val set_cookie :
  ?attrs:Cookie.attributes ->
  string -> string -> Response.t -> Response.t
val delete_cookie :
  ?path:string -> string -> Response.t -> Response.t
val cookie_signed : string -> Request.t -> string option
val set_cookie_signed :
  ?attrs:Cookie.attributes ->
  string -> string -> Response.t -> Response.t
val set_cookie_secret : string -> unit

(** {1 Compression} *)

val compress :
  ?min_size:int ->
  (Request.t -> Response.t) -> Request.t -> Response.t
val compress_gzip : string -> string
val compress_deflate : string -> string

(** {1 Rate Limiting} *)

val rate_limit :
  ?config:Ratelimit.config ->
  ?get_key:(Request.t -> string) ->
  (Request.t -> Response.t) -> Request.t -> Response.t

type rate_limit_config =
  Ratelimit.config = {
  requests_per_second : float;
  burst_size : int;
}

val default_rate_limit_config : Ratelimit.config

(** {1 ETag} *)

val etag : (Request.t -> Response.t) -> Request.t -> Response.t
val generate_etag : ?weak:bool -> string -> Etag.t
val with_etag : Etag.t -> Response.t -> Response.t

(** {1 WebSocket Shortcuts} *)

val is_websocket_upgrade : Request.t -> bool
val websocket_upgrade : Request.t -> (Response.t, string) result
val websocket :
  path:string -> handler:Websocket.handler ->
  (Request.t -> Response.t) -> Request.t -> Response.t
val ws_text : ?fin:bool -> string -> Websocket.frame
val ws_binary : ?fin:bool -> string -> Websocket.frame
val ws_ping : ?payload:string -> unit -> Websocket.frame
val ws_pong : payload:string -> Websocket.frame
val ws_close :
  ?code:Websocket.close_code ->
  ?reason:string -> unit -> Websocket.frame
val ws_encode : Websocket.frame -> string
val ws_decode : string -> (Websocket.frame * int, string) result
val ws_echo_handler : Websocket.handler

type ws_opcode = Websocket.opcode =
  | Continuation | Text | Binary | Close | Ping | Pong

(** {1 SSE Shortcuts} *)

val sse_data : string -> Sse.event
val sse_event : string -> string -> Sse.event
val sse_with_id : string -> Sse.event -> Sse.event
val sse_with_retry : int -> Sse.event -> Sse.event
val sse_encode : Sse.event -> string
val sse_response : Sse.event list -> Response.t
val sse_stream_response : Sse.event Eio.Stream.t -> Response.t
val sse_handler : Sse.Broadcaster.t -> Request.t -> Response.t
val sse : (Request.t -> Response.t) -> Request.t -> Response.t
val sse_ping : string
val sse_last_id : Request.t -> string option

(** {1 Streaming} *)

val stream :
  ?status:Http.Status.t ->
  ?headers:Http.Header.t -> Stream.producer -> Response.t
val stream_file :
  ?filename:string ->
  ?content_type:string -> ?chunk_size:int -> string -> Response.t
val stream_file_inline :
  ?content_type:string -> ?chunk_size:int -> string -> Response.t
val save_upload :
  request:Request.t ->
  dest_path:string -> ?chunk_size:int -> unit -> int
val read_chunks :
  request:Request.t -> chunk_size:int -> (string -> unit) -> unit
val stream_to_response : Response.t -> Response.t

(** {1 Template Shortcuts} *)

type template_context = Template.context
val template_context_empty : [> `Assoc of 'a list ]
val template_context :
  ('a * 'b) list -> [> `Assoc of ('a * [> `String of 'b ]) list ]
val template_context_of : 'a -> [> `Assoc of 'a ]
val template_render :
  ?partials:(string -> string option) ->
  ([< `Assoc of (string * 'a) list
    | `Bool of bool
    | `Float of float
    | `Int of int
    | `List of 'a list
    | `Null
    | `String of string
    > `Assoc `Bool `Int `List `Null `String ]
   as 'a) ->
  string -> string
val template_html :
  ?partials:(string -> string option) ->
  ([< `Assoc of (string * 'a) list
    | `Bool of bool
    | `Float of float
    | `Int of int
    | `List of 'a list
    | `Null
    | `String of string
    > `Assoc `Bool `Int `List `Null `String ]
   as 'a) ->
  string -> Response.t
val template_interpolate : string -> (string * string) list -> string
val html_escape : string -> string

(** {1 TLS Shortcuts} *)

val tls_config :
  cert_file:string ->
  key_file:string ->
  ?ca_file:string ->
  ?alpn:string list -> unit -> Tls_config.t Tls_config.result
val tls_from_pem :
  certificate:string ->
  private_key:string ->
  ?ca_cert:string -> ?alpn:string list -> unit -> Tls_config.t
val tls_dev : unit -> Tls_config.t
val tls_validate : Tls_config.t -> unit Tls_config.result
val tls_error_string : Tls_config.error -> string

(** {1 Validation} *)

val validated :
  'a Validation.typed_schema ->
  ('a -> Request.t -> Response.t) ->
  Request.t -> Response.t

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
