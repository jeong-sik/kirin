(** Prometheus text format export and HTTP handler *)

type label = string * string

(** Metric type *)
type metric_type =
  | Counter
  | Gauge
  | Histogram
  | Summary

type metric =
  | MCounter of Metric_counter.t
  | MGauge of Metric_gauge.t
  | MHistogram of Metric_histogram.t
  | MSummary of Metric_summary.t

type t = {
  metrics : (string, metric) Hashtbl.t;
  mutex : Eio.Mutex.t;
}

let create () = {
  metrics = Hashtbl.create 64;
  mutex = Eio.Mutex.create ();
}

let with_lock t f =
  Eio.Mutex.use_rw ~protect:true t.mutex f

(** Register a counter *)
let counter t name ~help ?(labels = []) () =
  let c = Metric_counter.create ~name ~help ~labels () in
  with_lock t (fun () -> Hashtbl.replace t.metrics name (MCounter c));
  c

(** Register a gauge *)
let gauge t name ~help ?(labels = []) () =
  let g = Metric_gauge.create ~name ~help ~labels () in
  with_lock t (fun () -> Hashtbl.replace t.metrics name (MGauge g));
  g

(** Register a histogram *)
let histogram t name ~help ?(labels = []) ?(buckets = Metric_histogram.default_buckets) () =
  let h = Metric_histogram.create ~name ~help ~labels ~buckets () in
  with_lock t (fun () -> Hashtbl.replace t.metrics name (MHistogram h));
  h

(** Register a summary *)
let summary t name ~help ?(labels = []) () =
  let s = Metric_summary.create ~name ~help ~labels () in
  with_lock t (fun () -> Hashtbl.replace t.metrics name (MSummary s));
  s

(** {1 Prometheus Format Export} *)

let escape_label_value s =
  let buf = Buffer.create (String.length s) in
  String.iter (fun c ->
    match c with
    | '\\' -> Buffer.add_string buf "\\\\"
    | '"' -> Buffer.add_string buf "\\\""
    | '\n' -> Buffer.add_string buf "\\n"
    | c -> Buffer.add_char buf c
  ) s;
  Buffer.contents buf

let format_labels labels =
  if labels = [] then ""
  else
    let pairs = List.map (fun (k, v) ->
      Printf.sprintf "%s=\"%s\"" k (escape_label_value v)
    ) labels in
    "{" ^ String.concat "," pairs ^ "}"

let export_counter buf (c : Metric_counter.t) =
  Buffer.add_string buf (Printf.sprintf "# HELP %s %s\n" c.name c.help);
  Buffer.add_string buf (Printf.sprintf "# TYPE %s counter\n" c.name);
  Hashtbl.iter (fun labels value ->
    Buffer.add_string buf (Printf.sprintf "%s%s %g\n" c.name (format_labels labels) value)
  ) c.values

let export_gauge buf (g : Metric_gauge.t) =
  Buffer.add_string buf (Printf.sprintf "# HELP %s %s\n" g.name g.help);
  Buffer.add_string buf (Printf.sprintf "# TYPE %s gauge\n" g.name);
  Hashtbl.iter (fun labels value ->
    Buffer.add_string buf (Printf.sprintf "%s%s %g\n" g.name (format_labels labels) value)
  ) g.values

let export_histogram buf (h : Metric_histogram.t) =
  Buffer.add_string buf (Printf.sprintf "# HELP %s %s\n" h.name h.help);
  Buffer.add_string buf (Printf.sprintf "# TYPE %s histogram\n" h.name);
  Hashtbl.iter (fun labels (data : Metric_histogram.data) ->
    let label_str = format_labels labels in
    Array.iter (fun (bucket : Metric_histogram.bucket_data) ->
      let le_labels = if labels = [] then
        Printf.sprintf "{le=\"%g\"}" bucket.upper_bound
      else
        let inner = String.sub label_str 1 (String.length label_str - 2) in
        Printf.sprintf "{%s,le=\"%g\"}" inner bucket.upper_bound
      in
      Buffer.add_string buf (Printf.sprintf "%s_bucket%s %d\n" h.name le_labels bucket.count)
    ) data.buckets;
    let inf_labels = if labels = [] then "{le=\"+Inf\"}"
      else
        let inner = String.sub label_str 1 (String.length label_str - 2) in
        Printf.sprintf "{%s,le=\"+Inf\"}" inner
    in
    Buffer.add_string buf (Printf.sprintf "%s_bucket%s %d\n" h.name inf_labels data.count);
    Buffer.add_string buf (Printf.sprintf "%s_sum%s %g\n" h.name label_str data.sum);
    Buffer.add_string buf (Printf.sprintf "%s_count%s %d\n" h.name label_str data.count)
  ) h.values

let export_summary buf (s : Metric_summary.t) =
  Buffer.add_string buf (Printf.sprintf "# HELP %s %s\n" s.name s.help);
  Buffer.add_string buf (Printf.sprintf "# TYPE %s summary\n" s.name);
  List.iter (fun q ->
    let v = Metric_summary.quantile s q in
    Buffer.add_string buf (Printf.sprintf "%s{quantile=\"%g\"} %g\n" s.name q v)
  ) [0.5; 0.9; 0.99];
  Buffer.add_string buf (Printf.sprintf "%s_sum %g\n" s.name s.sum);
  Buffer.add_string buf (Printf.sprintf "%s_count %d\n" s.name s.count)

(** Export all metrics in Prometheus format *)
let export t =
  let buf = Buffer.create 4096 in
  with_lock t (fun () ->
    Hashtbl.iter (fun _ metric ->
      match metric with
      | MCounter c -> export_counter buf c
      | MGauge g -> export_gauge buf g
      | MHistogram h -> export_histogram buf h
      | MSummary s -> export_summary buf s
    ) t.metrics
  );
  Buffer.contents buf

(** {1 HTTP Handler} *)

(** Metrics endpoint handler *)
let handler t =
  let default_headers = Http.Header.of_list [("Content-Type", "text/plain; version=0.0.4")] in
  fun _req ->
    let body = export t in
    Response.make ~status:`OK ~headers:default_headers (`String body)

(** {1 Built-in HTTP Metrics} *)

(** Standard HTTP metrics *)
type http_metrics = {
  requests_total : Metric_counter.t;
  request_duration : Metric_histogram.t;
  requests_in_flight : Metric_gauge.t;
}

(** Create standard HTTP metrics *)
let http_metrics t =
  let requests_total = counter t "http_requests_total"
    ~help:"Total number of HTTP requests"
    ~labels:["method"; "path"; "status"] () in
  let request_duration = histogram t "http_request_duration_seconds"
    ~help:"HTTP request latency in seconds"
    ~labels:["method"; "path"]
    ~buckets:[| 0.001; 0.005; 0.01; 0.025; 0.05; 0.1; 0.25; 0.5; 1.0; 2.5; 5.0; 10.0 |] () in
  let requests_in_flight = gauge t "http_requests_in_flight"
    ~help:"Number of HTTP requests currently being processed" () in
  { requests_total; request_duration; requests_in_flight }

(** Metrics middleware *)
let middleware http_metrics handler req =
  let meth = Http.Method.to_string (Request.meth req) in
  let path = Request.path req in

  Metric_gauge.inc http_metrics.requests_in_flight;

  let start = Time_compat.now () in
  let resp = try handler req with exn ->
    Metric_gauge.dec http_metrics.requests_in_flight;
    raise exn
  in
  let elapsed = Time_compat.now () -. start in

  Metric_gauge.dec http_metrics.requests_in_flight;

  let status = string_of_int (Http.Status.to_int (Response.status resp)) in
  Metric_counter.inc http_metrics.requests_total
    ~labels:[("method", meth); ("path", path); ("status", status)];
  Metric_histogram.observe http_metrics.request_duration ~labels:[("method", meth); ("path", path)] elapsed;

  resp
