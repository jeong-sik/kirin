type stdio_transport = { ic : Eio.Buf_read.t; oc : Eio.Buf_write.t; }
type streamable_http_transport = {
  endpoint : string;
  mutable pending_requests :
    (Jsonrpc.id * Jsonrpc.response Eio.Promise.u)
    list;
  mutable message_queue : Jsonrpc.message Eio.Stream.t;
  mutable session_id : string option;
}
type t =
    Stdio of stdio_transport
  | Streamable_http of streamable_http_transport
exception Transport_error of string
val read_line_stdio : Eio.Buf_read.t -> string
val write_line_stdio : Eio.Buf_write.t -> string -> unit
val read_message_stdio : Eio.Buf_read.t -> Jsonrpc.message
val write_message_stdio :
  Eio.Buf_write.t -> Jsonrpc.message -> unit
val create_streamable_http : ?endpoint:string -> unit -> t
val queue_message : t -> Jsonrpc.message -> unit
val read_message_streamable_http :
  streamable_http_transport -> Jsonrpc.message
val register_pending_request :
  streamable_http_transport ->
  Jsonrpc.id ->
  Jsonrpc.response Eio.Promise.u -> unit
val resolve_pending_request :
  streamable_http_transport ->
  Jsonrpc.id -> Jsonrpc.response -> unit
val session_id : t -> string option
val set_session_id : t -> string -> unit
val read_message : t -> Jsonrpc.message
val write_message : t -> Jsonrpc.message -> unit
val of_stdio : ic:Eio.Buf_read.t -> oc:Eio.Buf_write.t -> t
val of_streamable_http : ?endpoint:string -> unit -> t
val endpoint : t -> string option
val is_stdio : t -> bool
val is_streamable_http : t -> bool
val send_http_request :
  ?clock:[> float Eio.Time.clock_ty ] Eio.Resource.t ->
  ?timeout:float ->
  streamable_http_transport ->
  Jsonrpc.request -> Jsonrpc.response
(** Send an HTTP MCP request and await its response.

    Pass [~clock] and [~timeout] together to bound the await with
    [Eio.Time.with_timeout]; on expiry the pending entry is deregistered and
    [Transport_error "MCP HTTP request timed out after Ns"] is raised.

    Omitting either keeps the historical unbounded await — release relies on
    the enclosing [Eio.Switch] being cancelled. Code paths that may run
    outside a bounded switch should always pass both. *)

val send_request :
  ?clock:[> float Eio.Time.clock_ty ] Eio.Resource.t ->
  ?timeout:float ->
  t -> Jsonrpc.request -> Jsonrpc.response
(** Send a request on either transport. [clock]/[timeout] apply to HTTP only;
    stdio is line-based and ignores them. *)

val send_notification : t -> Jsonrpc.notification -> unit
