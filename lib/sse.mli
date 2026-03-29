(** Server-Sent Events (SSE) - Realtime Broadcaster.

    Implements the SSE protocol for server-to-client event streaming.

    {b Features:}
    - Channel-based broadcasting
    - Automatic keep-alive (ping)
    - Last-Event-ID support

    @since 1.0.0
    @status stable *)

(** {1 Types} *)

(** SSE event. *)
type event =
  { event : string option
  ; data : string
  ; id : string option
  ; retry : int option
  }

(** {1 Event Constructors} *)

(** [data d] creates a data-only SSE event. *)
val data : string -> event

(** [event typ d] creates an SSE event with the given type. *)
val event : string -> string -> event

(** [event_typed ~event_type d] creates an SSE event with a named type (labeled argument). *)
val event_typed : event_type:string -> string -> event

(** [with_id id e] adds an ID field to an event. *)
val with_id : string -> event -> event

(** [with_retry ms e] adds a retry interval (milliseconds) to an event. *)
val with_retry : int -> event -> event

(** {1 Encoding} *)

(** [encode e] serializes an event to SSE wire format. *)
val encode : event -> string

(** {1 Broadcaster} *)

module Broadcaster : sig
  (** Broadcaster manages a set of subscribed clients. *)
  type t

  (** [create ()] creates a new broadcaster. *)
  val create : unit -> t

  (** [subscribe t] subscribes a client. Returns [(id, stream)]. *)
  val subscribe : t -> int * event Eio.Stream.t

  (** [unsubscribe t id] removes the client with the given ID. *)
  val unsubscribe : t -> int -> unit

  (** [broadcast t evt] sends an event to all connected clients. *)
  val broadcast : t -> event -> unit
end

(** {1 Response} *)

(** [response stream] creates an SSE streaming response from an event stream.
    Sets Content-Type to [text/event-stream] with appropriate caching headers. *)
val response : event Eio.Stream.t -> Response.t

(** {1 Utilities} *)

(** Keep-alive ping comment line. *)
val ping : string

(** [last_event_id req] returns the Last-Event-ID header value, if present. *)
val last_event_id : Request.t -> string option

(** [middleware] SSE header middleware (deprecated, use [response] directly). *)
val middleware : (Request.t -> Response.t) -> Request.t -> Response.t

(** [handler broadcaster req] creates an SSE handler that subscribes to the broadcaster. *)
val handler : Broadcaster.t -> Request.t -> Response.t

(** {1 Legacy} *)

(** [response_legacy events] creates a non-streaming SSE response from a list of events. *)
val response_legacy : event list -> Response.t
