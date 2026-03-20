(** Kirin - OCaml 5.x Eio-native Web Framework

    Direct-style async web framework with type-safe routing,
    middleware composition, WebSocket/SSE support, template engine,
    connection pooling, and SSR framework integrations.

    {b Quick Start:}
    {[
      let () = Kirin.start ~port:8000
        @@ Kirin.logger
        @@ Kirin.router [
             Kirin.get "/" (fun _ -> Kirin.html "Hello, Kirin!");
           ]
    ]}
*)

(** {1 Core Types} *)

type handler = Request.t -> Response.t
type middleware = handler -> handler
type route = Router.route
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
val with_header :
  string -> string -> Response.t -> Response.t
val with_status : Http.Status.t -> Response.t -> Response.t
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
val logger : Middleware.t
val catch :
  (exn -> Request.t -> Response.t) -> Middleware.t
val cors :
  ?config:Middleware.cors_config -> unit -> Middleware.t
val timing : Middleware.t
val pipeline : Middleware.t list -> Middleware.t
val start :
  ?port:int ->
  ?request_timeout:float ->
  ?stream_read_timeout:float ->
  ?domains:int -> Router.handler -> unit
val run :
  ?config:Server.config ->
  sw:Eio.Switch.t ->
  env:< clock : [> float Eio.Time.clock_ty ] Eio.Time.clock;
        net : [> [> `Generic ] Eio.Net.ty ] Eio.Net.t; .. > ->
  Router.handler -> unit
type config = Server.config
val default_config : Server.config
type cors_config = Middleware.cors_config
val default_cors_config : Middleware.cors_config
val response_body : Response.t -> Response.body
val response_status : Response.t -> int
val response_header : string -> Response.t -> string option
val static :
  string ->
  dir:string ->
  (Request.t -> Response.t) ->
  Request.t -> Response.t
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
val multipart : Request.t -> Multipart.t option
val multipart_field : string -> Multipart.t -> string option
val multipart_file :
  string -> Multipart.t -> Multipart.part option
val multipart_files : Multipart.t -> Multipart.part list
val multipart_fields : Multipart.t -> (string * string) list
val compress :
  ?min_size:int ->
  (Request.t -> Response.t) ->
  Request.t -> Response.t
val compress_gzip : string -> string
val compress_deflate : string -> string
val rate_limit :
  ?config:Ratelimit.config ->
  ?get_key:(Request.t -> string) ->
  (Request.t -> Response.t) ->
  Request.t -> Response.t
type rate_limit_config =
  Ratelimit.config = {
  requests_per_second : float;
  burst_size : int;
}
val default_rate_limit_config : Ratelimit.config
val etag :
  (Request.t -> Response.t) ->
  Request.t -> Response.t
val generate_etag : ?weak:bool -> string -> Etag.t
val with_etag : Etag.t -> Response.t -> Response.t
val is_websocket_upgrade : Request.t -> bool
val websocket_upgrade :
  Request.t -> (Response.t, string) result
val websocket :
  path:string ->
  handler:Websocket.handler ->
  (Request.t -> Response.t) ->
  Request.t -> Response.t
val ws_text : ?fin:bool -> string -> Websocket.frame
val ws_binary : ?fin:bool -> string -> Websocket.frame
val ws_ping : ?payload:string -> unit -> Websocket.frame
val ws_pong : payload:string -> Websocket.frame
val ws_close :
  ?code:Websocket.close_code ->
  ?reason:string -> unit -> Websocket.frame
val ws_encode : Websocket.frame -> string
val ws_decode : string -> (Websocket.frame * int, string) result
type ws_opcode =
  Websocket.opcode =
    Continuation
  | Text
  | Binary
  | Close
  | Ping
  | Pong
type ws_frame =
  Websocket.frame = {
  fin : bool;
  opcode : ws_opcode;
  payload : string;
}
type ws_close_code =
  Websocket.close_code =
    Normal
  | GoingAway
  | ProtocolError
  | UnsupportedData
  | InvalidPayload
  | PolicyViolation
  | MessageTooBig
  | InternalError
val ws_echo_handler : Websocket.handler
type sse_event =
  Sse.event = {
  event : string option;
  data : string;
  id : string option;
  retry : int option;
}
val sse_data : string -> Sse.event
val sse_event : string -> string -> Sse.event
val sse_with_id : string -> Sse.event -> Sse.event
val sse_with_retry : int -> Sse.event -> Sse.event
val sse_encode : Sse.event -> string
val sse_response : Sse.event list -> Response.t
val sse_stream_response :
  Sse.event Eio.Stream.t -> Response.t
val sse_handler :
  Sse.Broadcaster.t -> Request.t -> Response.t
val sse :
  (Request.t -> Response.t) ->
  Request.t -> Response.t
val sse_ping : string
val sse_last_id : Request.t -> string option
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
type progress =
  Stream.progress_callback = {
  on_progress : bytes_sent:int -> total_bytes:int option -> unit;
  on_complete : unit -> unit;
  on_error : exn -> unit;
}
val progress_stderr :
  ?prefix:string -> unit -> Stream.progress_callback
val progress_silent : Stream.progress_callback
val stream_to_response : Response.t -> Response.t
module Stream = Stream
module Pool = Pool
type pool_config =
  Pool.config = {
  min_size : int;
  max_size : int;
  idle_timeout : float;
  max_wait_time : float;
  health_check_interval : float;
}
val default_pool_config : Pool.config
type pool_stats =
  Pool.stats = {
  total_connections : int;
  active_connections : int;
  idle_connections : int;
  waiting_requests : int;
  total_acquisitions : int;
  total_timeouts : int;
  total_errors : int;
}
module Backpressure = Backpressure
type backpressure_strategy =
  Backpressure.strategy =
    Block
  | Drop_oldest
  | Drop_newest
  | Error
module Cache = Cache
type cache_stats =
  Cache.stats = {
  hits : int;
  misses : int;
  evictions : int;
  expirations : int;
  current_size : int;
  max_size : int;
}
module Jobs = Jobs
type job_priority = Jobs.priority = Critical | High | Normal | Low
module Parallel = Parallel
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
type tls_config = Tls_config.t
type tls_error = Tls_config.error
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
module Request = Request
module Response = Response
module Router = Router
module Middleware = Middleware
module Trace = Trace
module Static = Static
module Fs_compat = Fs_compat
module Time_compat = Time_compat
module Cookie = Cookie
module Etag = Etag
module Multipart = Multipart
module Compress = Compress
module Ratelimit = Ratelimit
module Websocket = Websocket
module Sse = Sse
module Template = Template
module Tls_config = Tls_config
module Grpc = Grpc
module Graphql = Graphql_adapter
module Graphql_relay = Graphql_relay
module Sync = Sync
module Cluster = Cluster
module Logger = Logger
module Mcp = Mcp_adapter
module Health = Health
type health_status =
  Health.status =
    Healthy
  | Unhealthy of string
  | Degraded of string
module Metrics = Metrics
module Shutdown = Shutdown
type shutdown_state = Shutdown.state = Running | ShuttingDown | Stopped
module WebRTC = Webrtc_adapter
type webrtc_connection_state =
  Webrtc_adapter.connection_state =
    New
  | Connecting
  | Connected
  | Disconnected
  | Failed
  | Closed
module Db = Db
module Migrate = Migrate
module Query = Query
module Db_events = Db_events
module Openapi = Openapi
module I18n = I18n
module Validation = Validation
val validated :
  'a Validation.typed_schema ->
  ('a -> Request.t -> Response.t) ->
  Request.t -> Response.t
module Testing = Testing
