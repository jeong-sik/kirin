(** Kirin Scale-Up Example
    - Multi-domain Parallelism: True OCaml 5 multicore scaling
    - Structured Logging: JSON logs for observability
*)

let cpu_heavy_handler _req =
  let start = Unix.gettimeofday () in
  (* Simulate CPU work (e.g. image processing, heavy math) *)
  let rec fib n = if n <= 1 then n else fib (n - 1) + fib (n - 2) in
  let result = fib 35 in
  let duration = Unix.gettimeofday () -. start in
  
  Kirin.Logger.info ~ctx:[("result", `Int result); ("duration_sec", `Float duration)] 
    "CPU heavy task finished on Domain %d" (Domain.self () :> int);
    
  Kirin.json (`Assoc [
    ("result", `Int result);
    ("duration_sec", `Float duration);
    ("domain_id", `Int (Domain.self () :> int))
  ])

let routes = Kirin.router [
  Kirin.get "/" (fun _ -> Kirin.html "<h1>Kirin Multicore Scaling</h1><p>Check logs for JSON output</p>");
  Kirin.get "/heavy" cpu_heavy_handler;
]

let app =
  Kirin.Logger.middleware
  @@ routes

let () =
  (* Configure JSON logging *)
  Kirin.Logger.configure ~format:`Json ~min_level:Kirin.Logger.Info ();
  
  Printf.printf "Starting Kirin Multicore Server (Cores detected: %d)\n%!" (Domain.recommended_domain_count ());
  
  (* Start with native Multicore support (one Eio loop per core) *)
  Kirin.start ~port:8000 ~domains:4 app