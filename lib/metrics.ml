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

(** {1 Types} *)

(** Label pair *)
type label = string * string

(** Metric type *)
type metric_type =
  | Counter
  | Gauge
  | Histogram
  | Summary

(** Counter metric *)
module Counter = struct
  type t = {
    name : string;
    help : string;
    label_names : string list;
    values : (label list, float) Hashtbl.t;
    mutex : Mutex.t;
  }

  let create ~name ~help ?(labels = []) () = {
    name;
    help;
    label_names = labels;
    values = Hashtbl.create 64;
    mutex = Mutex.create ();
  }

  let with_lock t f =
    Mutex.lock t.mutex;
    Fun.protect ~finally:(fun () -> Mutex.unlock t.mutex) f

  let inc ?(by = 1.0) ?(labels = []) t =
    with_lock t (fun () ->
      let current = Hashtbl.find_opt t.values labels |> Option.value ~default:0.0 in
      Hashtbl.replace t.values labels (current +. by)
    )

  let get ?(labels = []) t =
    with_lock t (fun () ->
      Hashtbl.find_opt t.values labels |> Option.value ~default:0.0
    )

  let reset t =
    with_lock t (fun () -> Hashtbl.clear t.values)
end

(** Gauge metric *)
module Gauge = struct
  type t = {
    name : string;
    help : string;
    label_names : string list;
    values : (label list, float) Hashtbl.t;
    mutex : Mutex.t;
  }

  let create ~name ~help ?(labels = []) () = {
    name;
    help;
    label_names = labels;
    values = Hashtbl.create 64;
    mutex = Mutex.create ();
  }

  let with_lock t f =
    Mutex.lock t.mutex;
    Fun.protect ~finally:(fun () -> Mutex.unlock t.mutex) f

  let set ?(labels = []) t value =
    with_lock t (fun () -> Hashtbl.replace t.values labels value)

  let inc ?(by = 1.0) ?(labels = []) t =
    with_lock t (fun () ->
      let current = Hashtbl.find_opt t.values labels |> Option.value ~default:0.0 in
      Hashtbl.replace t.values labels (current +. by)
    )

  let dec ?(by = 1.0) ?(labels = []) t =
    with_lock t (fun () ->
      let current = Hashtbl.find_opt t.values labels |> Option.value ~default:0.0 in
      Hashtbl.replace t.values labels (current -. by)
    )

  let get ?(labels = []) t =
    with_lock t (fun () ->
      Hashtbl.find_opt t.values labels |> Option.value ~default:0.0
    )
end

(** Histogram metric *)
module Histogram = struct
  type bucket_data = {
    mutable count : int;
    upper_bound : float;
  }

  type data = {
    mutable sum : float;
    mutable count : int;
    buckets : bucket_data array;
  }

  type t = {
    name : string;
    help : string;
    label_names : string list;
    bucket_bounds : float array;
    values : (label list, data) Hashtbl.t;
    mutex : Mutex.t;
  }

  let default_buckets = [| 0.005; 0.01; 0.025; 0.05; 0.1; 0.25; 0.5; 1.0; 2.5; 5.0; 10.0 |]

  let create ~name ~help ?(labels = []) ?(buckets = default_buckets) () = {
    name;
    help;
    label_names = labels;
    bucket_bounds = buckets;
    values = Hashtbl.create 64;
    mutex = Mutex.create ();
  }

  let with_lock t f =
    Mutex.lock t.mutex;
    Fun.protect ~finally:(fun () -> Mutex.unlock t.mutex) f

  let make_data buckets =
    {
      sum = 0.0;
      count = 0;
      buckets = Array.map (fun b -> { count = 0; upper_bound = b }) buckets;
    }

  let observe ?(labels = []) t value =
    with_lock t (fun () ->
      let data = match Hashtbl.find_opt t.values labels with
        | Some d -> d
        | None ->
          let d = make_data t.bucket_bounds in
          Hashtbl.replace t.values labels d;
          d
      in
      data.sum <- data.sum +. value;
      data.count <- data.count + 1;
      Array.iter (fun bucket ->
        if value <= bucket.upper_bound then
          bucket.count <- bucket.count + 1
      ) data.buckets
    )

  let time ?(labels = []) t f =
    let start = Unix.gettimeofday () in
    let result = f () in
    let elapsed = Unix.gettimeofday () -. start in
    observe ~labels t elapsed;
    result
end

(** Summary metric (simplified - fixed quantiles) *)
module Summary = struct
  type t = {
    name : string;
    help : string;
    label_names : string list;
    mutable values : float list;  (* Keep recent values *)
    mutable sum : float;
    mutable count : int;
    max_samples : int;
    mutex : Mutex.t;
  }

  let create ~name ~help ?(labels = []) ?(max_samples = 1000) () = {
    name;
    help;
    label_names = labels;
    values = [];
    sum = 0.0;
    count = 0;
    max_samples;
    mutex = Mutex.create ();
  }

  let with_lock t f =
    Mutex.lock t.mutex;
    Fun.protect ~finally:(fun () -> Mutex.unlock t.mutex) f

  let observe t value =
    with_lock t (fun () ->
      t.sum <- t.sum +. value;
      t.count <- t.count + 1;
      t.values <- value :: (if List.length t.values >= t.max_samples
                           then List.tl t.values else t.values)
    )

  let quantile t q =
    with_lock t (fun () ->
      if t.values = [] then 0.0
      else
        let sorted = List.sort compare t.values in
        let n = List.length sorted in
        let idx = min (n - 1) (int_of_float (float_of_int n *. q)) in
        List.nth sorted idx
    )
end

(** {1 Registry} *)

type metric =
  | MCounter of Counter.t
  | MGauge of Gauge.t
  | MHistogram of Histogram.t
  | MSummary of Summary.t

type t = {
  metrics : (string, metric) Hashtbl.t;
  mutex : Mutex.t;
}

let create () = {
  metrics = Hashtbl.create 64;
  mutex = Mutex.create ();
}

let with_lock t f =
  Mutex.lock t.mutex;
  Fun.protect ~finally:(fun () -> Mutex.unlock t.mutex) f

(** Register a counter *)
let counter t name ~help ?(labels = []) () =
  let c = Counter.create ~name ~help ~labels () in
  with_lock t (fun () -> Hashtbl.replace t.metrics name (MCounter c));
  c

(** Register a gauge *)
let gauge t name ~help ?(labels = []) () =
  let g = Gauge.create ~name ~help ~labels () in
  with_lock t (fun () -> Hashtbl.replace t.metrics name (MGauge g));
  g

(** Register a histogram *)
let histogram t name ~help ?(labels = []) ?(buckets = Histogram.default_buckets) () =
  let h = Histogram.create ~name ~help ~labels ~buckets () in
  with_lock t (fun () -> Hashtbl.replace t.metrics name (MHistogram h));
  h

(** Register a summary *)
let summary t name ~help ?(labels = []) () =
  let s = Summary.create ~name ~help ~labels () in
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

let export_counter buf (c : Counter.t) =
  Buffer.add_string buf (Printf.sprintf "# HELP %s %s\n" c.name c.help);
  Buffer.add_string buf (Printf.sprintf "# TYPE %s counter\n" c.name);
  Hashtbl.iter (fun labels value ->
    Buffer.add_string buf (Printf.sprintf "%s%s %g\n" c.name (format_labels labels) value)
  ) c.values

let export_gauge buf (g : Gauge.t) =
  Buffer.add_string buf (Printf.sprintf "# HELP %s %s\n" g.name g.help);
  Buffer.add_string buf (Printf.sprintf "# TYPE %s gauge\n" g.name);
  Hashtbl.iter (fun labels value ->
    Buffer.add_string buf (Printf.sprintf "%s%s %g\n" g.name (format_labels labels) value)
  ) g.values

let export_histogram buf (h : Histogram.t) =
  Buffer.add_string buf (Printf.sprintf "# HELP %s %s\n" h.name h.help);
  Buffer.add_string buf (Printf.sprintf "# TYPE %s histogram\n" h.name);
  Hashtbl.iter (fun labels (data : Histogram.data) ->
    let label_str = format_labels labels in
    (* Export buckets *)
    Array.iter (fun (bucket : Histogram.bucket_data) ->
      let le_labels = if labels = [] then
        Printf.sprintf "{le=\"%g\"}" bucket.upper_bound
      else
        let inner = String.sub label_str 1 (String.length label_str - 2) in
        Printf.sprintf "{%s,le=\"%g\"}" inner bucket.upper_bound
      in
      Buffer.add_string buf (Printf.sprintf "%s_bucket%s %d\n" h.name le_labels bucket.count)
    ) data.buckets;
    (* +Inf bucket *)
    let inf_labels = if labels = [] then "{le=\"+Inf\"}"
      else
        let inner = String.sub label_str 1 (String.length label_str - 2) in
        Printf.sprintf "{%s,le=\"+Inf\"}" inner
    in
    Buffer.add_string buf (Printf.sprintf "%s_bucket%s %d\n" h.name inf_labels data.count);
    Buffer.add_string buf (Printf.sprintf "%s_sum%s %g\n" h.name label_str data.sum);
    Buffer.add_string buf (Printf.sprintf "%s_count%s %d\n" h.name label_str data.count)
  ) h.values

let export_summary buf (s : Summary.t) =
  Buffer.add_string buf (Printf.sprintf "# HELP %s %s\n" s.name s.help);
  Buffer.add_string buf (Printf.sprintf "# TYPE %s summary\n" s.name);
  (* Fixed quantiles: 0.5, 0.9, 0.99 *)
  List.iter (fun q ->
    let v = Summary.quantile s q in
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
let handler t _req =
  let body = export t in
  Response.make ~status:`OK
    ~headers:(Http.Header.of_list [
      ("content-type", "text/plain; version=0.0.4; charset=utf-8")
    ])
    body

(** {1 Built-in HTTP Metrics} *)

(** Standard HTTP metrics *)
type http_metrics = {
  requests_total : Counter.t;
  request_duration : Histogram.t;
  requests_in_flight : Gauge.t;
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

  (* Increment in-flight *)
  Gauge.inc http_metrics.requests_in_flight;

  let start = Unix.gettimeofday () in
  let resp = try handler req with exn ->
    Gauge.dec http_metrics.requests_in_flight;
    raise exn
  in
  let elapsed = Unix.gettimeofday () -. start in

  (* Decrement in-flight *)
  Gauge.dec http_metrics.requests_in_flight;

  (* Record metrics *)
  let status = string_of_int (Http.Status.to_int (Response.status resp)) in
  Counter.inc http_metrics.requests_total
    ~labels:[("method", meth); ("path", path); ("status", status)];
  Histogram.observe http_metrics.request_duration ~labels:[("method", meth); ("path", path)] elapsed;

  resp
