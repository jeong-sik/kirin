(** Kirin Metrics Module

    Prometheus-compatible metrics for monitoring.

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
        ~help:"Total HTTP requests" ~labels:["method"; "path"; "status"] in

      let latency = Metrics.histogram metrics "http_request_duration_seconds"
        ~help:"Request latency" ~buckets:[0.01; 0.05; 0.1; 0.5; 1.0; 5.0] in

      (* Record values *)
      Metrics.Counter.inc requests ~labels:[("method", "GET"); ("path", "/"); ("status", "200")];
      Metrics.Histogram.observe latency 0.042;

      (* Expose endpoint *)
      Kirin.get "/metrics" (Metrics.handler metrics)
    ]}
*)

(** {1 Sub-modules} *)

(* Sub-module includes *)
module Counter = struct
  include Metric_counter
end

module Gauge = struct
  include Metric_gauge
end

module Histogram = struct
  include Metric_histogram
end

module Summary = struct
  include Metric_summary
end

(* Registry, Prometheus export, handler, middleware *)
include Metric_prometheus
