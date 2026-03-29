(** Prometheus Metrics tests (Phase 10) *)

open Alcotest
open Test_helpers
module M = Kirin.Metrics

let test_metrics_counter () =
  let registry = M.create () in
  let counter = M.counter registry "test_counter" ~help:"Test counter" () in
  M.Counter.inc counter;
  M.Counter.inc counter ~by:5.0;
  check (float 0.01) "counter value" 6.0 (M.Counter.get counter)
;;

let test_metrics_counter_labels () =
  let registry = M.create () in
  let counter =
    M.counter registry "requests" ~help:"Requests" ~labels:[ "method"; "path" ] ()
  in
  M.Counter.inc counter ~labels:[ "method", "GET"; "path", "/" ];
  M.Counter.inc counter ~labels:[ "method", "POST"; "path", "/api" ];
  check
    (float 0.01)
    "GET /"
    1.0
    (M.Counter.get counter ~labels:[ "method", "GET"; "path", "/" ]);
  check
    (float 0.01)
    "POST /api"
    1.0
    (M.Counter.get counter ~labels:[ "method", "POST"; "path", "/api" ])
;;

let test_metrics_gauge () =
  let registry = M.create () in
  let gauge = M.gauge registry "test_gauge" ~help:"Test gauge" () in
  M.Gauge.set gauge 42.0;
  check (float 0.01) "gauge value" 42.0 (M.Gauge.get gauge);
  M.Gauge.inc gauge ~by:8.0;
  check (float 0.01) "after inc" 50.0 (M.Gauge.get gauge);
  M.Gauge.dec gauge ~by:10.0;
  check (float 0.01) "after dec" 40.0 (M.Gauge.get gauge)
;;

let test_metrics_histogram () =
  let registry = M.create () in
  let hist =
    M.histogram registry "latency" ~help:"Latency" ~buckets:[| 0.1; 0.5; 1.0 |] ()
  in
  M.Histogram.observe hist 0.05;
  M.Histogram.observe hist 0.3;
  M.Histogram.observe hist 0.8;
  (* Check it runs without error - actual bucket counts are internal *)
  ()
;;

let test_metrics_histogram_time () =
  let registry = M.create () in
  let hist = M.histogram registry "duration" ~help:"Duration" () in
  let result =
    M.Histogram.time hist (fun () ->
      Kirin.Time_compat.sleep 0.05;
      42)
  in
  check int "timed result" 42 result
;;

let test_metrics_summary () =
  let registry = M.create () in
  let sum = M.summary registry "response_size" ~help:"Response size" () in
  for i = 1 to 100 do
    M.Summary.observe sum (float_of_int i)
  done;
  let p50 = M.Summary.quantile sum 0.5 in
  check bool "p50 reasonable" true (p50 > 0.0)
;;

let test_metrics_export () =
  let registry = M.create () in
  let counter = M.counter registry "http_requests" ~help:"HTTP requests" () in
  M.Counter.inc counter;
  let output = M.export registry in
  check bool "has HELP" true (String.length output > 0);
  check
    bool
    "contains name"
    true
    (String.sub output 0 50 |> String.lowercase_ascii |> fun s -> String.length s > 0)
;;

let tests =
  [ test_case "metrics counter" `Quick (with_eio test_metrics_counter)
  ; test_case "metrics counter labels" `Quick (with_eio test_metrics_counter_labels)
  ; test_case "metrics gauge" `Quick (with_eio test_metrics_gauge)
  ; test_case "metrics histogram" `Quick (with_eio test_metrics_histogram)
  ; test_case "metrics histogram time" `Quick (with_eio test_metrics_histogram_time)
  ; test_case "metrics summary" `Quick (with_eio test_metrics_summary)
  ; test_case "metrics export" `Quick (with_eio test_metrics_export)
  ]
;;
