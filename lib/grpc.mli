(** Kirin gRPC Integration

    Provides gRPC support built on grpc-direct.
    Allows unified middleware for HTTP + gRPC in a single application.

    {b Features:}
    - Unified HTTP + gRPC server
    - Middleware-to-interceptor bridge
    - Type-safe service definitions
    - Streaming RPC support (unary, server, client, bidi)
    - Health check and reflection

    @since 1.0.0
    @status needs-work
    Thin wrapper over grpc-direct. API surface may be refined.
    @see <https://github.com/jeong-sik/grpc-direct> grpc-direct library *)

(** {1 Re-exports from grpc-direct} *)

(** gRPC Service definition. *)
module Service = Grpc_eio.Service

(** gRPC Server. *)
module Server = Grpc_eio.Server

(** gRPC Client. *)
module Client = Grpc_eio.Client

(** gRPC Interceptor (like middleware for gRPC). *)
module Interceptor = Grpc_eio.Interceptor

(** gRPC Stream for streaming RPCs. *)
module Stream = Grpc_eio.Stream

(** gRPC Health checking. *)
module Health = Grpc_eio.Health

(** gRPC Metrics (Prometheus format). *)
module Metrics = Grpc_eio.Metrics

(** gRPC Server Reflection. *)
module Reflection = Grpc_eio.Reflection

(** Connection Pool for clients. *)
module Pool = Grpc_eio.Pool

(** Load Balancer for clients. *)
module Balancer = Grpc_eio.Balancer

(** gRPC-Web support. *)
module Grpc_web = Grpc_eio.Grpc_web

(** gRPC Core types. *)
module Core : sig
  include module type of Grpc_core
  module Status = Grpc_core.Status
  module Codec = Grpc_core.Codec
  module Message = Grpc_core.Message
  module Timeout = Grpc_core.Timeout
end

(** {1 Kirin-style Helpers} *)

(** [service name] creates a new gRPC service. *)
val service : string -> Service.t

(** [unary method_name handler svc] adds a unary RPC method to a service. *)
val unary : string -> Service.unary_handler -> Service.t -> Service.t

(** [server_streaming method_name handler svc] adds a server streaming RPC method. *)
val server_streaming : string -> Service.server_streaming_handler -> Service.t -> Service.t

(** [client_streaming method_name handler svc] adds a client streaming RPC method. *)
val client_streaming : string -> Service.client_streaming_handler -> Service.t -> Service.t

(** [bidi_streaming method_name handler svc] adds a bidirectional streaming RPC method. *)
val bidi_streaming : string -> Service.bidi_handler -> Service.t -> Service.t

(** {1 Server Configuration} *)

(** gRPC server configuration. *)
type grpc_config = Server.config = {
  host : string;
  port : int;
  codecs : Grpc_core.Codec.t list;
  max_message_size : int;
  default_timeout : Grpc_core.Timeout.t option;
  tls : Grpc_eio.Tls_config.t option;
}

(** Default gRPC config (localhost:50051). *)
val default_grpc_config : grpc_config

(** [grpc_server ?config ()] creates a gRPC server. *)
val grpc_server : ?config:grpc_config -> unit -> Server.t

(** [add_service svc server] adds a service to the gRPC server. *)
val add_service : Service.t -> Server.t -> Server.t

(** [with_interceptor interceptor server] adds an interceptor to the gRPC server. *)
val with_interceptor : string Interceptor.t -> Server.t -> Server.t

(** {1 Middleware-Interceptor Bridge} *)

(** Context for bridging HTTP and gRPC. *)
type bridge_context = {
  method_name : string;
  metadata : (string * string) list;
  start_time : float;
}

(** [make_interceptor name f] creates a gRPC interceptor from a name and function. *)
val make_interceptor :
  string ->
  (Interceptor.context -> (Interceptor.context -> string Interceptor.response) -> string Interceptor.response) ->
  string Interceptor.t

(** [logging_interceptor ()] creates a logging interceptor. *)
val logging_interceptor : unit -> string Interceptor.t

(** [timing_interceptor ()] creates a timing interceptor that adds timing info. *)
val timing_interceptor : unit -> string Interceptor.t

(** [catch_interceptor ~on_error ()] creates a catch interceptor for error handling. *)
val catch_interceptor : on_error:(exn -> int) -> unit -> string Interceptor.t

(** {1 Unified Server} *)

(** Unified server configuration. *)
type unified_config = {
  http_port : int;
  grpc_port : int;
  host : string;
  grpc_tls : Grpc_eio.Tls_config.t option;
}

(** Default unified config (HTTP 8000, gRPC 50051). *)
val default_unified_config : unified_config

(** [start_unified ?config ~grpc_services ~http_handler ()] starts a unified HTTP + gRPC server.
    Runs both servers concurrently using Eio fibers. *)
val start_unified :
  ?config:unified_config ->
  grpc_services:Service.t list ->
  http_handler:Router.handler ->
  unit ->
  unit

(** {1 gRPC Status Helpers} *)

(** OK status. *)
val status_ok : Core.Status.t

(** [status ~code ~message] creates a status with code and message. *)
val status : code:Core.Status.code -> message:string -> Core.Status.t

(** Common status codes. *)
module StatusCode : sig
  val ok : Core.Status.code
  val cancelled : Core.Status.code
  val unknown : Core.Status.code
  val invalid_argument : Core.Status.code
  val deadline_exceeded : Core.Status.code
  val not_found : Core.Status.code
  val already_exists : Core.Status.code
  val permission_denied : Core.Status.code
  val resource_exhausted : Core.Status.code
  val failed_precondition : Core.Status.code
  val aborted : Core.Status.code
  val out_of_range : Core.Status.code
  val unimplemented : Core.Status.code
  val internal : Core.Status.code
  val unavailable : Core.Status.code
  val data_loss : Core.Status.code
  val unauthenticated : Core.Status.code
end

(** {1 Stream Helpers} *)

(** [stream_create capacity] creates a stream with given capacity. *)
val stream_create : int -> 'a Stream.t

(** [stream_is_closed stream] checks if stream is closed. *)
val stream_is_closed : 'a Stream.t -> bool

(** [stream_add stream value] adds value to stream. *)
val stream_add : 'a Stream.t -> 'a -> unit

(** [stream_close stream] closes a stream. *)
val stream_close : 'a Stream.t -> unit

(** [stream_take stream] takes value from stream (blocking). *)
val stream_take : 'a Stream.t -> 'a

(** [stream_is_empty stream] checks if stream is empty. *)
val stream_is_empty : 'a Stream.t -> bool
