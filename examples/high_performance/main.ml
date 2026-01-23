(** High-Performance Kirin Example

    Demonstrates Phase 9 features:
    - Connection Pool: Database connection management
    - Cache: LRU caching with TTL
    - Jobs: Background task processing
    - Parallel: CPU-bound parallel computation
    - Streaming: Large data handling
    - Backpressure: Flow control

    Run:
      dune exec examples/high_performance/main.exe
*)

(* ============================================================ *)
(* Simulated Database (for demo purposes)                       *)
(* ============================================================ *)

module FakeDb = struct
  type connection = { id : int; created_at : float }

  let next_id = ref 0

  let connect () =
    incr next_id;
    Unix.sleepf 0.01;  (* Simulate connection overhead *)
    { id = !next_id; created_at = Unix.gettimeofday () }

  let close _conn = ()

  let ping conn =
    (* Connection is valid if less than 60 seconds old *)
    Unix.gettimeofday () -. conn.created_at < 60.0

  let query conn sql =
    Printf.printf "[DB:%d] Executing: %s\n%!" conn.id sql;
    Unix.sleepf 0.05;  (* Simulate query time *)
    `Assoc [
      ("connection_id", `Int conn.id);
      ("query", `String sql);
      ("rows", `Int (Random.int 100));
    ]
end

(* ============================================================ *)
(* 1. Connection Pool                                           *)
(* ============================================================ *)

let db_pool = Kirin.Pool.create
  ~max_size:5
  ~create:FakeDb.connect
  ~destroy:FakeDb.close
  ~validate:FakeDb.ping
  ()

let db_handler _req =
  Kirin.Pool.use db_pool (fun conn ->
    let result = FakeDb.query conn "SELECT * FROM users LIMIT 10" in
    Kirin.json result
  )

(* ============================================================ *)
(* 2. LRU Cache with TTL                                        *)
(* ============================================================ *)

let user_cache : (string, Yojson.Safe.t) Kirin.Cache.t =
  Kirin.Cache.create ~max_size:1000 ()

(* Expensive computation (simulated) *)
let fetch_user_from_db id =
  Printf.printf "[Cache MISS] Fetching user %s from database\n%!" id;
  Unix.sleepf 0.1;  (* Simulate slow DB query *)
  `Assoc [
    ("id", `String id);
    ("name", `String ("User " ^ id));
    ("email", `String (id ^ "@example.com"));
    ("fetched_at", `Float (Unix.gettimeofday ()));
  ]

let cached_user_handler req =
  let id = Kirin.param "id" req in
  let user = Kirin.Cache.get_or_set ~ttl:30.0 user_cache id (fun () ->
    fetch_user_from_db id
  ) in
  Kirin.json user

let cache_stats_handler _req =
  let stats = Kirin.Cache.stats user_cache in
  Kirin.json (`Assoc [
    ("hits", `Int stats.hits);
    ("misses", `Int stats.misses);
    ("current_size", `Int stats.current_size);
    ("evictions", `Int stats.evictions);
    ("hit_rate", `Float (
      if stats.hits + stats.misses = 0 then 0.0
      else float_of_int stats.hits /. float_of_int (stats.hits + stats.misses)
    ));
  ])

(* ============================================================ *)
(* 3. Background Jobs                                           *)
(* ============================================================ *)

let job_queue : string Kirin.Jobs.t = Kirin.Jobs.create ~workers:2 ()

(* Simulate email sending *)
let send_email to_addr subject =
  Printf.printf "[EMAIL] Sending to %s: %s\n%!" to_addr subject;
  Unix.sleepf 0.5;  (* Simulate SMTP latency *)
  Printf.sprintf "Email sent to %s at %f" to_addr (Unix.gettimeofday ())

let submit_job_handler req =
  let email = Kirin.query_opt "email" req |> Option.value ~default:"test@example.com" in
  let job_id = Kirin.Jobs.submit ~priority:Kirin.Jobs.Normal job_queue (fun () ->
    send_email email "Welcome to Kirin!"
  ) in
  Kirin.json (`Assoc [
    ("job_id", `String job_id);
    ("status", `String "queued");
  ])

let job_status_handler req =
  let job_id = Kirin.param "id" req in
  let status = try Kirin.Jobs.status job_queue job_id with _ -> Kirin.Jobs.Failed (Failure "Unknown job") in
  let status_str = match status with
    | Kirin.Jobs.Pending -> "pending"
    | Kirin.Jobs.Running -> "running"
    | Kirin.Jobs.Completed result -> Printf.sprintf "completed: %s" result
    | Kirin.Jobs.Failed exn -> Printf.sprintf "failed: %s" (Printexc.to_string exn)
  in
  Kirin.json (`Assoc [
    ("job_id", `String job_id);
    ("status", `String status_str);
  ])

let job_stats_handler _req =
  let stats = Kirin.Jobs.stats job_queue in
  Kirin.json (`Assoc [
    ("total_submitted", `Int stats.total_submitted);
    ("total_completed", `Int stats.total_completed);
    ("total_failed", `Int stats.total_failed);
    ("currently_running", `Int stats.currently_running);
    ("queue_size", `Int stats.queue_size);
  ])

(* ============================================================ *)
(* 4. Parallel Processing                                       *)
(* ============================================================ *)

(* CPU-intensive computation (prime factorization) *)
let factorize n =
  let rec find_factors n d acc =
    if d * d > n then
      if n > 1 then n :: acc else acc
    else if n mod d = 0 then
      find_factors (n / d) d (d :: acc)
    else
      find_factors n (d + 1) acc
  in
  List.rev (find_factors n 2 [])

let parallel_handler req =
  let count = Kirin.query_opt "count" req
              |> Option.map int_of_string
              |> Option.value ~default:100 in

  (* Generate random numbers to factorize *)
  let numbers = List.init count (fun i -> 10000 + i * 7) in

  (* Measure sequential time *)
  let start_seq = Unix.gettimeofday () in
  let _ = List.map factorize numbers in
  let seq_time = Unix.gettimeofday () -. start_seq in

  (* Measure parallel time *)
  let start_par = Unix.gettimeofday () in
  let results = Kirin.Parallel.map ~domains:4 factorize numbers in
  let par_time = Unix.gettimeofday () -. start_par in

  let speedup = seq_time /. par_time in

  Kirin.json (`Assoc [
    ("count", `Int count);
    ("sequential_ms", `Float (seq_time *. 1000.0));
    ("parallel_ms", `Float (par_time *. 1000.0));
    ("speedup", `Float speedup);
    ("domains_used", `Int 4);
    ("sample_result", `List (
      List.map (fun n -> `Int n) (List.hd results)
    ));
  ])

(* Fork-join example *)
let fork_join_handler _req =
  let (a, b, c) = Kirin.Parallel.triple
    (fun () ->
      Unix.sleepf 0.1;
      `String "Task A complete")
    (fun () ->
      Unix.sleepf 0.1;
      `String "Task B complete")
    (fun () ->
      Unix.sleepf 0.1;
      `String "Task C complete")
  in
  Kirin.json (`Assoc [
    ("task_a", a);
    ("task_b", b);
    ("task_c", c);
    ("note", `String "All 3 tasks ran in parallel (~100ms total, not 300ms)");
  ])

(* ============================================================ *)
(* 5. Backpressure                                              *)
(* ============================================================ *)

let rate_limiter = Kirin.Backpressure.RateLimiter.create
  ~rate:5.0     (* 5 requests per second *)
  ~burst:10    (* Allow burst of 10 *)
  ()

let rate_limited_handler _req =
  if Kirin.Backpressure.RateLimiter.try_acquire rate_limiter then
    Kirin.json (`Assoc [
      ("status", `String "ok");
      ("message", `String "Request processed");
      ("timestamp", `Float (Unix.gettimeofday ()));
    ])
  else
    Kirin.Response.make ~status:`Too_many_requests
      {|{"error": "Rate limited. Please slow down."}|}
    |> Kirin.with_header "Content-Type" "application/json"
    |> Kirin.with_header "Retry-After" "1"

(* ============================================================ *)
(* 6. Streaming (Large Data)                                    *)
(* ============================================================ *)

(** Streaming demo - demonstrates chunked transfer encoding

    Note: In a full implementation, Stream.t responses would be handled
    directly by the HTTP server for true streaming. This demo uses
    stream_to_response to convert to a regular response for simplicity.
*)
let streaming_handler req =
  let size = Kirin.query_opt "size" req
             |> Option.map int_of_string
             |> Option.value ~default:100 in  (* Keep small for demo *)

  (* Create streaming response *)
  let stream = Kirin.stream (fun yield ->
    for i = 1 to size do
      yield (Printf.sprintf "Line %d: %s\n" i (String.make 50 'x'))
    done
  ) in

  (* Convert to regular response for demo
     In production, you'd handle Stream.t directly in the server *)
  Kirin.stream_to_response stream

(* ============================================================ *)
(* Home Page (API Overview)                                     *)
(* ============================================================ *)

let home_handler _req =
  Kirin.html ~doctype:true {|
<!DOCTYPE html>
<html>
<head>
  <title>Kirin High-Performance Demo</title>
  <style>
    body { font-family: system-ui, sans-serif; max-width: 800px; margin: 40px auto; padding: 0 20px; }
    h1 { color: #2563eb; }
    h2 { color: #475569; margin-top: 2em; }
    pre { background: #f1f5f9; padding: 15px; border-radius: 8px; overflow-x: auto; }
    a { color: #2563eb; }
    .badge { display: inline-block; background: #dbeafe; color: #1e40af; padding: 2px 8px; border-radius: 4px; font-size: 12px; }
  </style>
</head>
<body>
  <h1>🦌 Kirin High-Performance Demo</h1>
  <p>Demonstrating Phase 9 features: Pool, Cache, Jobs, Parallel, Backpressure, Streaming</p>

  <h2>1. Connection Pool <span class="badge">Pool</span></h2>
  <pre>GET <a href="/db">/db</a></pre>
  <p>Reuses database connections from a pool of 5.</p>

  <h2>2. LRU Cache <span class="badge">Cache</span></h2>
  <pre>GET <a href="/users/alice">/users/:id</a>
GET <a href="/cache/stats">/cache/stats</a></pre>
  <p>First request: cache miss (slow). Subsequent: cache hit (fast).</p>

  <h2>3. Background Jobs <span class="badge">Jobs</span></h2>
  <pre>POST <a href="/jobs?email=test@example.com">/jobs?email=...</a>
GET <a href="/jobs/status/job_1_1234">/jobs/status/:id</a>
GET <a href="/jobs/stats">/jobs/stats</a></pre>
  <p>Submit email jobs to background queue with priority.</p>

  <h2>4. Parallel Processing <span class="badge">Parallel</span></h2>
  <pre>GET <a href="/parallel?count=500">/parallel?count=500</a>
GET <a href="/fork-join">/fork-join</a></pre>
  <p>CPU-bound work distributed across 4 domains.</p>

  <h2>5. Rate Limiting <span class="badge">Backpressure</span></h2>
  <pre>GET <a href="/rate-limited">/rate-limited</a></pre>
  <p>Token bucket: 5 req/sec with burst of 10. Spam it to see 429.</p>

  <h2>6. Streaming <span class="badge">Streaming</span></h2>
  <pre>GET <a href="/stream?size=10000">/stream?size=10000</a></pre>
  <p>Generate large data without loading all into memory.</p>

  <hr style="margin-top: 3em">
  <p><small>Kirin Web Framework - Phase 9 High-Performance Components</small></p>
</body>
</html>
|}

(* ============================================================ *)
(* Main Application                                             *)
(* ============================================================ *)

let () =
  (* Start job queue *)
  Kirin.Jobs.start job_queue;

  Printf.printf "Starting Kirin High-Performance Demo on http://localhost:8000\n%!";

  Kirin.start ~port:8000
  @@ Kirin.logger
  @@ Kirin.timing
  @@ Kirin.router [
       (* Home *)
       Kirin.get "/" home_handler;

       (* Pool *)
       Kirin.get "/db" db_handler;

       (* Cache *)
       Kirin.get "/users/:id" cached_user_handler;
       Kirin.get "/cache/stats" cache_stats_handler;

       (* Jobs *)
       Kirin.post "/jobs" submit_job_handler;
       Kirin.get "/jobs" submit_job_handler;  (* Allow GET for easy testing *)
       Kirin.get "/jobs/status/:id" job_status_handler;
       Kirin.get "/jobs/stats" job_stats_handler;

       (* Parallel *)
       Kirin.get "/parallel" parallel_handler;
       Kirin.get "/fork-join" fork_join_handler;

       (* Backpressure *)
       Kirin.get "/rate-limited" rate_limited_handler;

       (* Streaming *)
       Kirin.get "/stream" streaming_handler;
     ]
