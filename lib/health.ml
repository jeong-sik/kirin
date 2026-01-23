(** Kirin Health Check Module

    Kubernetes-style health check endpoints for production deployments.

    {b Endpoints:}
    - /health - Overall health status
    - /live - Liveness probe (is the process running?)
    - /ready - Readiness probe (can the service accept traffic?)

    {b Example:}
    {[
      let health = Health.create () in

      (* Register custom checks *)
      Health.register health "database" (fun () ->
        if Db.ping () then Health.Healthy else Health.Unhealthy "connection failed");

      (* Add to routes *)
      let routes = Health.routes health @ my_routes
    ]}
*)

(** {1 Types} *)

(** Health status *)
type status =
  | Healthy
  | Unhealthy of string
  | Degraded of string

(** Health check function *)
type check = unit -> status

(** Health check result *)
type check_result = {
  name : string;
  status : status;
  duration_ms : float;
}

(** Overall health response *)
type health_response = {
  status : status;
  checks : check_result list;
  timestamp : float;
  uptime_seconds : float;
}

(** Health checker *)
type t = {
  mutable checks : (string * check) list;
  mutable ready : bool;
  start_time : float;
  mutex : Mutex.t;
}

(** {1 Helpers} *)

let now () = Unix.gettimeofday ()

let with_lock mutex f =
  Mutex.lock mutex;
  Fun.protect ~finally:(fun () -> Mutex.unlock mutex) f

let status_to_string = function
  | Healthy -> "healthy"
  | Unhealthy msg -> "unhealthy: " ^ msg
  | Degraded msg -> "degraded: " ^ msg

let status_to_json = function
  | Healthy -> `String "healthy"
  | Unhealthy msg -> `Assoc [("status", `String "unhealthy"); ("message", `String msg)]
  | Degraded msg -> `Assoc [("status", `String "degraded"); ("message", `String msg)]

let is_healthy = function
  | Healthy -> true
  | Degraded _ -> true  (* Degraded is still "up" *)
  | Unhealthy _ -> false

(** {1 Creation} *)

(** Create a new health checker *)
let create () = {
  checks = [];
  ready = true;
  start_time = now ();
  mutex = Mutex.create ();
}

(** {1 Check Registration} *)

(** Register a health check *)
let register t name check =
  with_lock t.mutex (fun () ->
    t.checks <- (name, check) :: List.filter (fun (n, _) -> n <> name) t.checks
  )

(** Unregister a health check *)
let unregister t name =
  with_lock t.mutex (fun () ->
    t.checks <- List.filter (fun (n, _) -> n <> name) t.checks
  )

(** {1 Readiness Control} *)

(** Set readiness status *)
let set_ready t ready =
  with_lock t.mutex (fun () -> t.ready <- ready)

(** Check if ready *)
let is_ready t =
  with_lock t.mutex (fun () -> t.ready)

(** {1 Check Execution} *)

(** Run a single check with timing *)
let run_check (name, check) =
  let start = now () in
  let status = try check () with exn -> Unhealthy (Printexc.to_string exn) in
  let duration_ms = (now () -. start) *. 1000.0 in
  { name; status; duration_ms }

(** Run all checks *)
let run_all_checks t =
  let checks = with_lock t.mutex (fun () -> t.checks) in
  List.map run_check checks

(** Get overall health response *)
let check t : health_response =
  let results = run_all_checks t in
  let all_healthy = List.for_all (fun (r : check_result) -> is_healthy r.status) results in
  let any_degraded = List.exists (fun (r : check_result) ->
    match r.status with Degraded _ -> true | _ -> false
  ) results in
  let status =
    if not all_healthy then Unhealthy "one or more checks failed"
    else if any_degraded then Degraded "one or more checks degraded"
    else Healthy
  in
  {
    status;
    checks = results;
    timestamp = now ();
    uptime_seconds = now () -. t.start_time;
  }

(** {1 JSON Serialization} *)

let check_result_to_json r =
  `Assoc [
    ("name", `String r.name);
    ("status", status_to_json r.status);
    ("duration_ms", `Float r.duration_ms);
  ]

let health_response_to_json r =
  `Assoc [
    ("status", status_to_json r.status);
    ("checks", `List (List.map check_result_to_json r.checks));
    ("timestamp", `Float r.timestamp);
    ("uptime_seconds", `Float r.uptime_seconds);
  ]

(** {1 HTTP Handlers} *)

(** Health endpoint handler - detailed status *)
let health_handler t _req =
  let response = check t in
  let json = health_response_to_json response in
  let status = if is_healthy response.status then `OK else `Service_unavailable in
  Response.make ~status
    ~headers:(Http.Header.of_list [("content-type", "application/json")])
    (Yojson.Safe.to_string json)

(** Liveness probe - is the process alive? *)
let live_handler _t _req =
  (* If we can respond, we're alive *)
  Response.make ~status:`OK
    ~headers:(Http.Header.of_list [("content-type", "application/json")])
    {|{"status":"alive"}|}

(** Readiness probe - can we accept traffic? *)
let ready_handler t _req =
  if is_ready t then
    Response.make ~status:`OK
      ~headers:(Http.Header.of_list [("content-type", "application/json")])
      {|{"status":"ready"}|}
  else
    Response.make ~status:`Service_unavailable
      ~headers:(Http.Header.of_list [("content-type", "application/json")])
      {|{"status":"not ready"}|}

(** {1 Routes} *)

(** Create health check routes *)
let routes t = [
  Router.route `GET "/health" (health_handler t);
  Router.route `GET "/live" (live_handler t);
  Router.route `GET "/ready" (ready_handler t);
  Router.route `GET "/livez" (live_handler t);   (* Kubernetes style *)
  Router.route `GET "/readyz" (ready_handler t); (* Kubernetes style *)
  Router.route `GET "/healthz" (health_handler t); (* Kubernetes style *)
]

(** {1 Common Checks} *)

(** Create a check that always passes *)
let always_healthy name = (name, fun () -> Healthy)

(** Create a check that calls a function *)
let check_fn name f = (name, fun () ->
  if f () then Healthy else Unhealthy "check failed"
)

(** Create a check with timeout *)
let check_with_timeout name timeout_sec f =
  (name, fun () ->
    let start = now () in
    let result = try f () with _ -> Unhealthy "exception" in
    let elapsed = now () -. start in
    if elapsed > timeout_sec then
      Degraded (Printf.sprintf "slow: %.2fs" elapsed)
    else
      result
  )

(** {1 Middleware} *)

(** Middleware that adds health routes before other routes *)
let middleware t handler req =
  let path = Request.path req in
  if List.mem path ["/health"; "/live"; "/ready"; "/healthz"; "/livez"; "/readyz"] then
    match path with
    | "/health" | "/healthz" -> health_handler t req
    | "/live" | "/livez" -> live_handler t req
    | "/ready" | "/readyz" -> ready_handler t req
    | _ -> handler req
  else
    handler req
