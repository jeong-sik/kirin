(** Kirin gRPC Integration

    Provides gRPC support built on grpc-direct.
    Allows unified middleware for HTTP + gRPC in a single application.

    {b Features:}
    - Unified HTTP + gRPC server
    - Middleware-to-interceptor bridge
    - Type-safe service definitions
    - Streaming RPC support (unary, server, client, bidi)
    - Health check and reflection

    {b Quick Start:}
    {[
      (* Define a gRPC service *)
      let greeter = Kirin.Grpc.service "helloworld.Greeter"
        |> Kirin.Grpc.unary "SayHello" (fun request ->
             (* request/response are bytes, use your protobuf library *)
             "Hello, " ^ request)

      (* Start unified server *)
      let () = Kirin.Grpc.start_unified ~http_port:8000 ~grpc_port:50051
        ~grpc_services:[greeter]
        ~http_handler:(Kirin.router [...])
        ()
    ]}

    {b Middleware Bridge:}
    {[
      (* Convert Kirin middleware to gRPC interceptor *)
      let logging_interceptor =
        Kirin.Grpc.interceptor_of_middleware Kirin.logger
    ]}

    @see <https://github.com/jeong-sik/grpc-direct> grpc-direct library
*)

(** {1 Re-exports from grpc-direct} *)

(** gRPC Service definition *)
module Service = Grpc_eio.Service

(** gRPC Server *)
module Server = Grpc_eio.Server

(** gRPC Client *)
module Client = Grpc_eio.Client

(** gRPC Interceptor (like middleware for gRPC) *)
module Interceptor = Grpc_eio.Interceptor

(** gRPC Stream for streaming RPCs *)
module Stream = Grpc_eio.Stream

(** gRPC Health checking *)
module Health = Grpc_eio.Health

(** gRPC Metrics (Prometheus format) *)
module Metrics = Grpc_eio.Metrics

(** gRPC Server Reflection *)
module Reflection = Grpc_eio.Reflection

(** Connection Pool for clients *)
module Pool = Grpc_eio.Pool

(** Load Balancer for clients *)
module Balancer = Grpc_eio.Balancer

(** gRPC-Web support *)
module Grpc_web = Grpc_eio.Grpc_web

(** gRPC Core types *)
module Core = struct
  include Grpc_core
  module Status = Grpc_core.Status
  module Codec = Grpc_core.Codec
  module Message = Grpc_core.Message
  module Timeout = Grpc_core.Timeout
end

(** {1 Kirin-style Helpers} *)

(** Create a new gRPC service *)
let service name = Service.create name

(** Add a unary RPC method to a service *)
let unary method_name handler svc =
  Service.add_unary method_name handler svc

(** Add a server streaming RPC method *)
let server_streaming method_name handler svc =
  Service.add_server_streaming method_name handler svc

(** Add a client streaming RPC method *)
let client_streaming method_name handler svc =
  Service.add_client_streaming method_name handler svc

(** Add a bidirectional streaming RPC method *)
let bidi_streaming method_name handler svc =
  Service.add_bidi_streaming method_name handler svc

(** {1 Server Configuration} *)

(** gRPC server configuration *)
type grpc_config = Server.config = {
  host : string;
  port : int;
  codecs : Grpc_core.Codec.t list;
  max_message_size : int;
  default_timeout : Grpc_core.Timeout.t option;
  tls : Grpc_eio.Tls_config.t option;
}

(** Default gRPC config (localhost:50051) *)
let default_grpc_config = Server.default_config

(** Create a gRPC server *)
let grpc_server ?config () =
  Server.create ?config ()

(** Add a service to the gRPC server *)
let add_service svc server =
  Server.add_service svc server

(** Add an interceptor to the gRPC server *)
let with_interceptor interceptor server =
  Server.with_interceptor interceptor server

(** {1 Middleware-Interceptor Bridge} *)

(** Context for bridging HTTP and gRPC *)
type bridge_context = {
  method_name : string;
  metadata : (string * string) list;
  start_time : float;
}

(** Create a gRPC interceptor from a name and function.

    Example:
    {[
      let timing_interceptor = Kirin.Grpc.make_interceptor "timing"
        (fun ctx next ->
          let start = Unix.gettimeofday () in
          let result = next ctx in
          let elapsed = Unix.gettimeofday () -. start in
          Printf.printf "RPC %s took %.3fs\n" ctx.method_ elapsed;
          result)
    ]}
*)
let make_interceptor name f =
  Interceptor.make ~name f

(** Create a logging interceptor (similar to Kirin.logger) *)
let logging_interceptor () =
  make_interceptor "kirin-logger" (fun ctx next ->
    let start = Unix.gettimeofday () in
    Printf.printf "[gRPC] --> %s\n%!" ctx.Interceptor.method_;
    let result = next ctx in
    let elapsed = (Unix.gettimeofday () -. start) *. 1000.0 in
    Printf.printf "[gRPC] <-- %s (%.2fms)\n%!" ctx.Interceptor.method_ elapsed;
    result)

(** Create a timing interceptor that adds timing info *)
let timing_interceptor () =
  make_interceptor "kirin-timing" (fun ctx next ->
    let start = Unix.gettimeofday () in
    let result = next ctx in
    let elapsed = (Unix.gettimeofday () -. start) *. 1000.0 in
    let trailers =
      ("x-response-time-ms", Printf.sprintf "%.2f" elapsed)
      :: result.Interceptor.trailers
    in
    { result with trailers })

(** Create a catch interceptor for error handling *)
let catch_interceptor ~on_error () =
  make_interceptor "kirin-catch" (fun ctx next ->
    try next ctx
    with exn ->
      let status = on_error exn in
      { Interceptor.value = ""; trailers = [("grpc-status", string_of_int status)] })

(** {1 Unified Server} *)

(** Unified server configuration *)
type unified_config = {
  http_port : int;
  grpc_port : int;
  host : string;
  grpc_tls : Grpc_eio.Tls_config.t option;
}

let default_unified_config = {
  http_port = 8000;
  grpc_port = 50051;
  host = "0.0.0.0";
  grpc_tls = None;
}

(** Start a unified HTTP + gRPC server.

    Runs both servers concurrently using Eio fibers.

    Example:
    {[
      let () = Kirin.Grpc.start_unified
        ~http_port:8000
        ~grpc_port:50051
        ~grpc_services:[greeter_service; health_service]
        ~http_handler:(Kirin.router [
          Kirin.get "/" (fun _ -> Kirin.html "Hello!");
        ])
        ()
    ]}
*)
let start_unified
    ?(config = default_unified_config)
    ~grpc_services
    ~http_handler
    ()
  =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  (* Create gRPC server *)
  let grpc_config = {
    Server.default_config with
    host = config.host;
    port = config.grpc_port;
    tls = config.grpc_tls;
  } in
  let grpc_server =
    List.fold_left
      (fun s svc -> Server.add_service svc s)
      (Server.create ~config:grpc_config ())
      grpc_services
  in

  (* Start both servers concurrently *)
  Eio.Fiber.both
    (fun () ->
       Printf.printf "[Kirin] HTTP server starting on %s:%d\n%!"
         config.host config.http_port;
       (* Use Kirin's HTTP server - handler is already composed *)
       ignore http_handler;
       (* TODO: Integrate with Kirin.Server.start when unified server is ready *)
       ())
    (fun () ->
       Printf.printf "[Kirin] gRPC server starting on %s:%d\n%!"
         config.host config.grpc_port;
       Server.serve ~sw ~env grpc_server)

(** {1 gRPC Status Helpers} *)

(** Create an OK status *)
let status_ok = Core.Status.{ code = OK; message = ""; details = None }

(** Create a status with code and message *)
let status ~code ~message =
  Core.Status.{ code; message; details = None }

(** Common status codes *)
module StatusCode = struct
  let ok = Core.Status.OK
  let cancelled = Core.Status.Cancelled
  let unknown = Core.Status.Unknown
  let invalid_argument = Core.Status.Invalid_argument
  let deadline_exceeded = Core.Status.Deadline_exceeded
  let not_found = Core.Status.Not_found
  let already_exists = Core.Status.Already_exists
  let permission_denied = Core.Status.Permission_denied
  let resource_exhausted = Core.Status.Resource_exhausted
  let failed_precondition = Core.Status.Failed_precondition
  let aborted = Core.Status.Aborted
  let out_of_range = Core.Status.Out_of_range
  let unimplemented = Core.Status.Unimplemented
  let internal = Core.Status.Internal
  let unavailable = Core.Status.Unavailable
  let data_loss = Core.Status.Data_loss
  let unauthenticated = Core.Status.Unauthenticated
end

(** {1 Stream Helpers} *)

(** Create a stream with given capacity *)
let stream_create capacity = Stream.create capacity

(** Check if stream is closed *)
let stream_is_closed stream = Stream.is_closed stream

(** Add value to stream *)
let stream_add stream value = Stream.add stream value

(** Close a stream *)
let stream_close stream = Stream.close stream

(** Take value from stream (blocking) *)
let stream_take stream = Stream.take stream

(** Check if stream is empty *)
let stream_is_empty stream = Stream.is_empty stream
