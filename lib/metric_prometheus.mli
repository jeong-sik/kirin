type label = Metric_common.label
type metric_type = Counter | Gauge | Histogram | Summary
type metric =
    MCounter of Metric_counter.t
  | MGauge of Metric_gauge.t
  | MHistogram of Metric_histogram.t
  | MSummary of Metric_summary.t
type t = { metrics : (string, metric) Hashtbl.t; mutex : Eio.Mutex.t; }
val create : unit -> t
val counter :
  t ->
  string ->
  help:string -> ?labels:string list -> unit -> Metric_counter.t
val gauge :
  t ->
  string ->
  help:string -> ?labels:string list -> unit -> Metric_gauge.t
val histogram :
  t ->
  string ->
  help:string ->
  ?labels:string list ->
  ?buckets:float array -> unit -> Metric_histogram.t
val summary :
  t ->
  string ->
  help:string -> ?labels:string list -> unit -> Metric_summary.t
val escape_label_value : string -> string
val format_labels : (string * string) list -> string
val export_counter : Buffer.t -> Metric_counter.t -> unit
val export_gauge : Buffer.t -> Metric_gauge.t -> unit
val export_histogram : Buffer.t -> Metric_histogram.t -> unit
val export_summary : Buffer.t -> Metric_summary.t -> unit
val export : t -> string
val handler : t -> 'a -> Response.t
type http_metrics = {
  requests_total : Metric_counter.t;
  request_duration : Metric_histogram.t;
  requests_in_flight : Metric_gauge.t;
}
val http_metrics : t -> http_metrics
val default_normalize_path : string -> string
val middleware :
  ?normalize_path:(string -> string) ->
  http_metrics ->
  (Request.t -> Response.t) ->
  Request.t -> Response.t
