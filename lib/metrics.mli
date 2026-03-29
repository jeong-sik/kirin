(** Kirin Metrics Module

    Prometheus-compatible metrics for monitoring.

    @since 1.0.0
    @status stable

    {b Metric Types:}
    - Counter: Monotonically increasing value
    - Gauge: Value that can go up or down
    - Histogram: Distribution of values in buckets
    - Summary: Quantile-based summary

    {b Example:}
    {[
      let metrics = Metrics.create () in

      (* Register metrics *)
      let requests = Metrics.counter metrics "http_requests_total"
        ~help:"Total HTTP requests" ~labels:["method"; "path"; "status"] () in

      let latency = Metrics.histogram metrics "http_request_duration_seconds"
        ~help:"Request latency"
        ~buckets:[| 0.01; 0.05; 0.1; 0.5; 1.0; 5.0 |] () in

      (* Record values *)
      Metrics.Counter.inc requests
        ~labels:[("method", "GET"); ("path", "/"); ("status", "200")];
      Metrics.Histogram.observe latency 0.042;

      (* Expose endpoint *)
      Kirin.get "/metrics" (Metrics.handler metrics)
    ]}
*)

(** {1 Label Types} *)

(** A label is a key-value pair for metric dimensions. *)
type label = string * string

(** {1 Counter} *)

(** Counter metric - monotonically increasing value. *)
module Counter : sig
  (** Abstract counter type. *)
  type t

  (** [create ~name ~help ?labels ()] creates a new counter.
      [labels] defines the label dimensions for this metric. *)
  val create : name:string -> help:string -> ?labels:string list -> unit -> t

  (** [inc ?by ?labels t] increments the counter by [by] (default 1.0)
      for the given label combination. *)
  val inc : ?by:float -> ?labels:label list -> t -> unit

  (** [get ?labels t] returns the current value for the given labels. *)
  val get : ?labels:label list -> t -> float

  (** [reset t] clears all recorded values. *)
  val reset : t -> unit
end

(** {1 Gauge} *)

(** Gauge metric - value that can go up or down. *)
module Gauge : sig
  (** Abstract gauge type. *)
  type t

  (** [create ~name ~help ?labels ()] creates a new gauge. *)
  val create : name:string -> help:string -> ?labels:string list -> unit -> t

  (** [set ?labels t value] sets the gauge to [value]. *)
  val set : ?labels:label list -> t -> float -> unit

  (** [inc ?by ?labels t] increments the gauge by [by] (default 1.0). *)
  val inc : ?by:float -> ?labels:label list -> t -> unit

  (** [dec ?by ?labels t] decrements the gauge by [by] (default 1.0). *)
  val dec : ?by:float -> ?labels:label list -> t -> unit

  (** [get ?labels t] returns the current value. *)
  val get : ?labels:label list -> t -> float
end

(** {1 Histogram} *)

(** Histogram metric - distribution of values in buckets. *)
module Histogram : sig
  (** Abstract histogram type. *)
  type t

  (** Default bucket boundaries for histograms. *)
  val default_buckets : float array

  (** [create ~name ~help ?labels ?buckets ()] creates a new histogram.
      [buckets] defines the upper bounds for each bucket (default: standard Prometheus buckets). *)
  val create
    :  name:string
    -> help:string
    -> ?labels:string list
    -> ?buckets:float array
    -> unit
    -> t

  (** [observe ?labels t value] records a value in the histogram. *)
  val observe : ?labels:label list -> t -> float -> unit

  (** [time ?labels t f] measures the execution time of [f] and records it.
      Returns the result of [f]. *)
  val time : ?labels:label list -> t -> (unit -> 'a) -> 'a
end

(** {1 Summary} *)

(** Summary metric - quantile-based summary. *)
module Summary : sig
  (** Abstract summary type. *)
  type t

  (** [create ~name ~help ?labels ?max_samples ()] creates a new summary.
      [max_samples] is the maximum number of samples to retain (default: 1000). *)
  val create
    :  name:string
    -> help:string
    -> ?labels:string list
    -> ?max_samples:int
    -> unit
    -> t

  (** [observe t value] records a value. *)
  val observe : t -> float -> unit

  (** [quantile t q] returns the value at quantile [q] (0.0 to 1.0). *)
  val quantile : t -> float -> float
end

(** {1 Metric Variants} *)

(** Metric type discriminator. *)
type metric_type =
  | Counter
  | Gauge
  | Histogram
  | Summary

(** A registered metric (tagged union). *)
type metric =
  | MCounter of Counter.t
  | MGauge of Gauge.t
  | MHistogram of Histogram.t
  | MSummary of Summary.t

(** {1 Registry} *)

(** Metrics registry - holds all registered metrics. *)
type t

(** [create ()] creates a new empty metrics registry. *)
val create : unit -> t

(** [counter t name ~help ?labels ()] registers and returns a new counter. *)
val counter : t -> string -> help:string -> ?labels:string list -> unit -> Counter.t

(** [gauge t name ~help ?labels ()] registers and returns a new gauge. *)
val gauge : t -> string -> help:string -> ?labels:string list -> unit -> Gauge.t

(** [histogram t name ~help ?labels ?buckets ()] registers and returns
    a new histogram. *)
val histogram
  :  t
  -> string
  -> help:string
  -> ?labels:string list
  -> ?buckets:float array
  -> unit
  -> Histogram.t

(** [summary t name ~help ?labels ()] registers and returns a new summary. *)
val summary : t -> string -> help:string -> ?labels:string list -> unit -> Summary.t

(** {1 Prometheus Export} *)

(** [export t] returns all registered metrics in Prometheus text exposition format. *)
val export : t -> string

(** {1 HTTP Handler} *)

(** [handler t] returns a request handler that serves metrics in Prometheus format.
    Responds with [Content-Type: text/plain; version=0.0.4]. *)
val handler : t -> Request.t -> Response.t

(** {1 Built-in HTTP Metrics} *)

(** Standard HTTP metrics bundle for use with {!middleware}. *)
type http_metrics =
  { requests_total : Counter.t
  ; request_duration : Histogram.t
  ; requests_in_flight : Gauge.t
  }

(** [http_metrics t] creates and registers a standard set of HTTP metrics:
    - [http_requests_total] (counter with method/path/status labels)
    - [http_request_duration_seconds] (histogram with method/path labels)
    - [http_requests_in_flight] (gauge) *)
val http_metrics : t -> http_metrics

(** [middleware ?normalize_path http_metrics handler req] wraps a handler with automatic
    HTTP metrics collection. Tracks request count, latency, and in-flight requests.
    [normalize_path] replaces dynamic path segments (UUIDs, numbers) with [:id]
    to prevent Prometheus label cardinality explosion. *)
val middleware
  :  ?normalize_path:(string -> string)
  -> http_metrics
  -> (Request.t -> Response.t)
  -> Request.t
  -> Response.t
