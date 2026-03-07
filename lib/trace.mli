(** Lightweight request tracing with W3C Trace Context support.

    Provides span-based tracing without OpenTelemetry SDK dependency.
    Spans are stored in the request context (Hmap) via {!middleware}.

    {b Usage:}
    {[
      let app = Middleware.pipeline [
        Trace.middleware ();
        my_handler
      ]

      let handler req =
        let span = Trace.child_span req ~name:"db_query" in
        let result = do_query () in
        Trace.finish span;
        Response.json result
    ]} *)

(** {1 Types} *)

(** Span identifier (16 hex chars). *)
type span_id = string

(** Trace identifier (32 hex chars). *)
type trace_id = string

(** Trace flags (sampling). *)
type trace_flags = int

(** A single trace span. *)
type span = {
  trace_id : trace_id;
  span_id : span_id;
  parent_id : span_id option;
  name : string;
  start_time : float;
  mutable end_time : float option;
  mutable status : [ `Ok | `Error of string ];
  mutable attributes : (string * string) list;
}

(** {1 ID Generation} *)

(** Generate a new random trace ID (32 hex chars). *)
val new_trace_id : unit -> trace_id

(** Generate a new random span ID (16 hex chars). *)
val new_span_id : unit -> span_id

(** {1 Span Operations} *)

(** [start ~name ?trace_id ?parent_id ()] creates a new span.
    Generates a new trace_id if not provided. *)
val start :
  name:string ->
  ?trace_id:trace_id ->
  ?parent_id:span_id ->
  unit ->
  span

(** [finish span] records the end time on the span. *)
val finish : span -> unit

(** [set_attr span ~key ~value] adds a key-value attribute to the span. *)
val set_attr : span -> key:string -> value:string -> unit

(** [set_error span msg] marks the span as errored with the given message. *)
val set_error : span -> string -> unit

(** [duration span] returns the span duration in seconds.
    Uses current time if the span is still open. *)
val duration : span -> float

(** {1 W3C Trace Context} *)

(** [parse_traceparent header] parses a W3C traceparent header.
    Returns [(trace_id, parent_id)] or [None] if invalid. *)
val parse_traceparent : string -> (trace_id * span_id) option

(** [to_traceparent span] formats a span as a W3C traceparent header value. *)
val to_traceparent : span -> string

(** {1 Context Integration} *)

(** Hmap key for storing the current span in request context. *)
val span_key : span Hmap.key

(** [current_span req] returns the active span from the request context. *)
val current_span : Request.t -> span option

(** [child_span req ~name] creates a child span under the current request span.
    Creates a new root span if no parent exists. *)
val child_span : Request.t -> name:string -> span

(** {1 Exporter Interface} *)

(** Exporter receives finished spans for output. *)
module type EXPORTER = sig
  val export : span -> unit
end

(** Stderr exporter: prints span summaries to stderr. *)
module Stderr_exporter : EXPORTER

(** JSON exporter: outputs spans as JSON lines to stderr. *)
module Json_exporter : EXPORTER

(** {1 Tracing Middleware} *)

(** [middleware ?exporter ()] creates a tracing middleware.
    Automatically creates a root span per request, propagates traceparent,
    and exports the span on completion. *)
val middleware : ?exporter:(module EXPORTER) -> unit -> Middleware.t
