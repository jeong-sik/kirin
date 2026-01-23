(** Kirin Framework Benchmark Suite

    Measures handler/middleware overhead.
    NOT network benchmarks - just measures pure handler execution.

    Run: dune exec bench/bench_kirin.exe

    Metrics:
    - Throughput: operations per second
    - Latency: p50, p95, p99 in nanoseconds
    - Memory: minor/major allocations per operation
*)

(** ============================================================
    Benchmarking Infrastructure
    ============================================================ *)

module Bench = struct
  type result = {
    name : string;
    _iterations : int; [@warning "-69"]
    _total_time_ns : float; [@warning "-69"]
    throughput_ops : float;
    latency_p50_ns : float;
    latency_p95_ns : float;
    latency_p99_ns : float;
    minor_words : float;
    major_words : float;
  }

  let time_ns () = Unix.gettimeofday () *. 1_000_000_000.0

  (** Run warmup iterations *)
  let warmup ~iterations f =
    for _ = 1 to iterations do
      ignore (f ())
    done

  (** Collect latency samples *)
  let collect_samples ~count f =
    let samples = Array.make count 0.0 in
    for i = 0 to count - 1 do
      let start = time_ns () in
      ignore (f ());
      let stop = time_ns () in
      samples.(i) <- stop -. start
    done;
    Array.sort Float.compare samples;
    samples

  (** Collect allocation stats *)
  let measure_allocations ~iterations f =
    Gc.full_major ();
    let stat_before = Gc.quick_stat () in
    for _ = 1 to iterations do
      ignore (f ())
    done;
    let stat_after = Gc.quick_stat () in
    let minor_words =
      (stat_after.minor_words -. stat_before.minor_words) /. float iterations
    in
    let major_words =
      (stat_after.major_words -. stat_before.major_words) /. float iterations
    in
    (minor_words, major_words)

  (** Get percentile from sorted array *)
  let percentile samples p =
    let n = Array.length samples in
    let idx = int_of_float (float n *. p /. 100.0) in
    let idx = min (n - 1) (max 0 idx) in
    samples.(idx)

  (** Run a benchmark *)
  let run ~name ~warmup_iterations ~sample_count ~alloc_iterations f =
    (* Warmup *)
    warmup ~iterations:warmup_iterations f;

    (* Collect latency samples *)
    let samples = collect_samples ~count:sample_count f in
    let total_time_ns = Array.fold_left ( +. ) 0.0 samples in

    (* Calculate throughput *)
    let throughput_ops =
      if total_time_ns > 0.0 then
        float sample_count /. (total_time_ns /. 1_000_000_000.0)
      else 0.0
    in

    (* Calculate latencies *)
    let latency_p50_ns = percentile samples 50.0 in
    let latency_p95_ns = percentile samples 95.0 in
    let latency_p99_ns = percentile samples 99.0 in

    (* Measure allocations *)
    let (minor_words, major_words) = measure_allocations ~iterations:alloc_iterations f in

    { name; _iterations = sample_count; _total_time_ns = total_time_ns; throughput_ops;
      latency_p50_ns; latency_p95_ns; latency_p99_ns; minor_words; major_words }

  (** Format number with commas *)
  let format_number n =
    let s = Printf.sprintf "%.0f" n in
    let len = String.length s in
    let buf = Buffer.create (len + len / 3) in
    String.iteri (fun i c ->
      if i > 0 && (len - i) mod 3 = 0 then Buffer.add_char buf ',';
      Buffer.add_char buf c
    ) s;
    Buffer.contents buf

  (** Print results as ASCII table *)
  let print_results results =
    let sep = String.make 120 '-' in

    Printf.printf "\n%s\n" sep;
    Printf.printf "| %-30s | %12s | %10s | %10s | %10s | %10s | %10s |\n"
      "Benchmark" "ops/sec" "p50 (ns)" "p95 (ns)" "p99 (ns)" "minor" "major";
    Printf.printf "%s\n" sep;

    List.iter (fun r ->
      Printf.printf "| %-30s | %12s | %10.0f | %10.0f | %10.0f | %10.1f | %10.1f |\n"
        r.name
        (format_number r.throughput_ops)
        r.latency_p50_ns
        r.latency_p95_ns
        r.latency_p99_ns
        r.minor_words
        r.major_words
    ) results;

    Printf.printf "%s\n\n" sep
end

(** ============================================================
    Test Fixtures
    ============================================================ *)

(** Create a mock request for benchmarking *)
let make_request ?(meth = `GET) ?(path = "/") ?(headers = []) ?(body = "") () =
  let raw_headers =
    Http.Header.of_list (("host", "localhost") :: headers)
  in
  let resource = path in
  let raw = Http.Request.make ~meth ~headers:raw_headers resource in
  Kirin.Request.make ~raw ~body

(** Sample JSON data for serialization benchmarks *)
let small_json = `Assoc [
  ("message", `String "Hello, World!");
  ("status", `Int 200);
]

let medium_json = `Assoc [
  ("users", `List [
    `Assoc [("id", `Int 1); ("name", `String "Alice"); ("email", `String "alice@example.com")];
    `Assoc [("id", `Int 2); ("name", `String "Bob"); ("email", `String "bob@example.com")];
    `Assoc [("id", `Int 3); ("name", `String "Charlie"); ("email", `String "charlie@example.com")];
    `Assoc [("id", `Int 4); ("name", `String "David"); ("email", `String "david@example.com")];
    `Assoc [("id", `Int 5); ("name", `String "Eve"); ("email", `String "eve@example.com")];
  ]);
  ("metadata", `Assoc [
    ("page", `Int 1);
    ("per_page", `Int 5);
    ("total", `Int 100);
    ("timestamp", `String "2026-01-23T12:00:00Z");
  ]);
]

let large_json =
  let users = List.init 100 (fun i ->
    `Assoc [
      ("id", `Int i);
      ("name", `String (Printf.sprintf "User_%d" i));
      ("email", `String (Printf.sprintf "user%d@example.com" i));
      ("bio", `String (String.make 200 'x'));
    ]
  ) in
  `Assoc [
    ("users", `List users);
    ("metadata", `Assoc [
      ("page", `Int 1);
      ("per_page", `Int 100);
      ("total", `Int 10000);
    ]);
  ]

(** Sample HTML content *)
let small_html = "<h1>Hello, Kirin!</h1>"
let medium_html = String.concat "" [
  "<!DOCTYPE html><html><head><title>Test</title></head><body>";
  String.make 1000 'x';
  "</body></html>";
]
let large_html = String.concat "" [
  "<!DOCTYPE html><html><head><title>Test</title></head><body>";
  String.make 50000 'x';
  "</body></html>";
]

(** ============================================================
    Handlers
    ============================================================ *)

(** Hello World - minimal handlers *)
let handler_hello_text _req = Kirin.text "Hello, World!"
let handler_hello_html _req = Kirin.html "<h1>Hello, World!</h1>"

(** JSON Serialization handlers *)
let handler_json_small _req = Kirin.json small_json
let handler_json_medium _req = Kirin.json medium_json
let handler_json_large _req = Kirin.json large_json

(** HTML handlers with varying sizes *)
let handler_html_small _req = Kirin.html small_html
let handler_html_medium _req = Kirin.html medium_html
let handler_html_large _req = Kirin.html large_html

(** Parameter extraction handler *)
let handler_with_param req =
  let id = Kirin.param "id" req in
  Kirin.json (`Assoc [("id", `String id)])

(** ============================================================
    Routers
    ============================================================ *)

let simple_routes = [
  Kirin.get "/" handler_hello_text;
  Kirin.get "/hello" handler_hello_html;
  Kirin.get "/json" handler_json_small;
]

let many_routes =
  List.init 100 (fun i ->
    Kirin.get (Printf.sprintf "/route%d" i) handler_hello_text
  ) @ [
    Kirin.get "/users/:id" handler_with_param;
    Kirin.get "/api/v1/users/:id/posts/:post_id" handler_with_param;
  ]

let router_simple = Kirin.router simple_routes
let router_many = Kirin.router many_routes

(** ============================================================
    Middleware
    ============================================================ *)

(** Silent logger (no output, measures timing overhead only) *)
let silent_logger : Kirin.middleware = fun handler req ->
  let _start = Unix.gettimeofday () in
  let response = handler req in
  let _elapsed = Unix.gettimeofday () -. _start in
  response

let timing_middleware = Kirin.timing
let cors_middleware = Kirin.cors ()
let compress_middleware = Kirin.compress

let combined_middleware = Kirin.pipeline [
  silent_logger;
  timing_middleware;
  cors_middleware;
]

(** ============================================================
    Benchmark Runner
    ============================================================ *)

let () =
  print_endline "=== Kirin Framework Benchmark Suite ===";
  print_endline "";
  print_endline "Configuration:";
  print_endline "  - Warmup iterations: 10,000";
  print_endline "  - Sample count: 100,000";
  print_endline "  - Allocation iterations: 10,000";
  print_endline "";
  print_endline "Starting benchmarks...";

  let warmup_iterations = 10_000 in
  let sample_count = 100_000 in
  let alloc_iterations = 10_000 in

  let run name f = Bench.run ~name ~warmup_iterations ~sample_count ~alloc_iterations f in

  (* Requests *)
  let req_root = make_request ~path:"/" () in
  let req_route0 = make_request ~path:"/route0" () in
  let req_route50 = make_request ~path:"/route50" () in
  let req_param = make_request ~path:"/users/123" () in
  let req_with_encoding = make_request
    ~path:"/"
    ~headers:[("accept-encoding", "gzip, deflate")]
    ()
  in

  (* ===== Handler Benchmarks ===== *)
  print_endline "\n[1/5] Running handler benchmarks...";
  let handler_results = [
    run "handler/hello_text" (fun () -> handler_hello_text req_root);
    run "handler/hello_html" (fun () -> handler_hello_html req_root);
    run "handler/json_small" (fun () -> handler_json_small req_root);
    run "handler/json_medium" (fun () -> handler_json_medium req_root);
    run "handler/json_large" (fun () -> handler_json_large req_root);
    run "handler/html_small" (fun () -> handler_html_small req_root);
    run "handler/html_medium" (fun () -> handler_html_medium req_root);
    run "handler/html_large" (fun () -> handler_html_large req_root);
  ] in
  Bench.print_results handler_results;

  (* ===== Router Benchmarks ===== *)
  print_endline "[2/5] Running router benchmarks...";
  let router_results = [
    run "router/simple_3routes" (fun () -> router_simple req_root);
    run "router/100routes_first" (fun () -> router_many req_route0);
    run "router/100routes_middle" (fun () -> router_many req_route50);
    run "router/100routes_last" (fun () -> router_many (make_request ~path:"/route99" ()));
    run "router/param_extraction" (fun () -> router_many req_param);
  ] in
  Bench.print_results router_results;

  (* ===== Middleware Benchmarks ===== *)
  print_endline "[3/5] Running middleware benchmarks...";
  let handler_none = handler_hello_text in
  let handler_logger = silent_logger handler_hello_text in
  let handler_timing = timing_middleware handler_hello_text in
  let handler_cors = cors_middleware handler_hello_text in
  let handler_compress_small = compress_middleware handler_hello_text in
  let handler_compress_large = compress_middleware handler_html_large in
  let handler_combined = combined_middleware handler_hello_text in

  let middleware_results = [
    run "middleware/none" (fun () -> handler_none req_root);
    run "middleware/logger" (fun () -> handler_logger req_root);
    run "middleware/timing" (fun () -> handler_timing req_root);
    run "middleware/cors" (fun () -> handler_cors req_root);
    run "middleware/compress_small" (fun () -> handler_compress_small req_with_encoding);
    run "middleware/compress_large" (fun () -> handler_compress_large req_with_encoding);
    run "middleware/combined" (fun () -> handler_combined req_root);
  ] in
  Bench.print_results middleware_results;

  (* ===== Response Construction Benchmarks ===== *)
  print_endline "[4/5] Running response benchmarks...";
  let base_resp = Kirin.Response.make "Hello" in
  let response_results = [
    run "response/make" (fun () -> Kirin.Response.make "Hello");
    run "response/with_header" (fun () -> Kirin.Response.with_header "X-Custom" "value" base_resp);
    run "response/with_3_headers" (fun () ->
      Kirin.Response.with_headers
        [("X-A", "1"); ("X-B", "2"); ("X-C", "3")]
        base_resp);
    run "response/text" (fun () -> Kirin.text "Hello");
    run "response/html" (fun () -> Kirin.html "<h1>Hello</h1>");
    run "response/json" (fun () -> Kirin.json small_json);
  ] in
  Bench.print_results response_results;

  (* ===== Compression Benchmarks ===== *)
  print_endline "[5/5] Running compression benchmarks...";
  let data_1kb = String.make 1024 'x' in
  let data_10kb = String.make 10240 'x' in
  let data_100kb = String.make 102400 'x' in

  let compression_results = [
    run "compress/gzip_1kb" (fun () -> Kirin.compress_gzip data_1kb);
    run "compress/gzip_10kb" (fun () -> Kirin.compress_gzip data_10kb);
    run "compress/gzip_100kb" (fun () -> Kirin.compress_gzip data_100kb);
    run "compress/deflate_1kb" (fun () -> Kirin.compress_deflate data_1kb);
    run "compress/deflate_10kb" (fun () -> Kirin.compress_deflate data_10kb);
  ] in
  Bench.print_results compression_results;

  (* ===== Summary ===== *)
  print_endline "=== Benchmark Complete ===";
  print_endline "";
  print_endline "Legend:";
  print_endline "  ops/sec    : Operations per second (throughput)";
  print_endline "  p50/p95/p99: Latency percentiles in nanoseconds";
  print_endline "  minor      : Minor heap allocations (words per operation)";
  print_endline "  major      : Major heap allocations (words per operation)";
  print_endline "";
  print_endline "Notes:";
  print_endline "  - These benchmarks measure handler/middleware overhead only";
  print_endline "  - No actual network I/O is performed";
  print_endline "  - For real-world throughput, use tools like wrk or hey";
  ()
