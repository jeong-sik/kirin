(** Kirin Health Check Module

    Kubernetes-style health checks (Liveness/Readiness probes).

    {b Example:}
    {[ 
      let health = Health.create () in 
      Health.register health "db" db_check;
      Health.register health "redis" redis_check;
      
      let routes = Health.routes health 
    ]}
*)

(** {1 Types} *)

type status =
  | Healthy
  | Unhealthy of string
  | Degraded of string

type check = unit -> status

type t = {
  checks : (string, check) Hashtbl.t;
  start_time : float;
  mutable manual_ready : bool;
}

(** {1 Creation} *)

let create () = {
  checks = Hashtbl.create 16;
  start_time = Unix.gettimeofday ();
  manual_ready = true;
}

(** Register a named health check *)
let register t name check =
  Hashtbl.replace t.checks name check

(** Set ready state manually *)
let set_ready t ready =
  t.manual_ready <- ready

(** Check if ready (including manual flag) *)
let is_ready t =
  t.manual_ready
(** {1 Check Logic} *)

(* A registered check is a user-provided closure.  Before this PR
   it was called directly inside [Hashtbl.fold], so a single check
   that raised collapsed the entire aggregation — [/health],
   [/healthz], [/ready] and friends all returned 500 and kubelet
   saw the pod as crashed.  One buggy probe should not gate every
   other probe; map a raised exception to an [Unhealthy] status
   that names the failure and lets the remaining checks contribute.
   [Eio.Cancel.Cancelled] is re-raised because that one *does*
   need to unwind the fiber. *)
let safe_run_check name check =
  try check ()
  with
  | Eio.Cancel.Cancelled _ as e -> raise e
  | exn ->
    Unhealthy
      (Printf.sprintf "check %S raised: %s" name (Printexc.to_string exn))

(** Run all checks and aggregate status *)
let check t =
  let results = Hashtbl.fold (fun name check acc ->
    (name, safe_run_check name check) :: acc
  ) t.checks [] in
  
  let overall = List.fold_left (fun acc (_, status) -> 
    match acc, status with 
    | Unhealthy _, _ -> acc 
    | _, Unhealthy msg -> Unhealthy msg 
    | Degraded _, _ -> acc 
    | _, Degraded msg -> Degraded msg 
    | Healthy, Healthy -> Healthy 
  ) Healthy results in 
  
  let uptime = Unix.gettimeofday () -. t.start_time in 
  
  let details = List.map (fun (name, status) -> 
    let status_str, error = match status with 
      | Healthy -> "healthy", `Null
      | Unhealthy msg -> "unhealthy", `String msg
      | Degraded msg -> "degraded", `String msg
    in 
    (name, `Assoc [("status", `String status_str); ("error", error)]) 
  ) results in 
  
  let json = `Assoc [
    ("status", `String (match overall with 
      | Healthy -> "healthy"
      | Unhealthy _ -> "unhealthy"
      | Degraded _ -> "degraded"));
    ("uptime", `Float uptime);
    ("details", `Assoc details);
  ] in 
  
  (overall, json)

(** {1 HTTP Handlers} *)

let default_headers = Http.Header.of_list [
  ("Content-Type", "application/json");
  ("Cache-Control", "no-cache");
]

(** Full health check handler (for /health) *)
let handler t = 
  fun _req -> 
    let status, json = check t in 
    let http_status = match status with 
      | Healthy -> `OK
      | Degraded _ -> `OK
      | Unhealthy _ -> `Service_unavailable 
    in 
    Response.make ~status:http_status ~headers:default_headers 
      (`String (Yojson.Safe.to_string json))

(** Liveness probe handler (for /live) *)
let live_handler _t = 
  fun _req -> 
    Response.make ~status:`OK ~headers:default_headers 
      (`String "{\"status\":\"alive\"}")

(** Pure: readiness verdict.

    Honours both the manual [is_ready] flag *and* the aggregated
    [check] result.  The manual flag exists for graceful K8s
    rolling updates: an operator calls [set_ready t false] just
    before SIGTERM so the kubelet starts draining traffic while
    in-flight requests finish.  Before this PR the flag was a
    silent no-op in [ready_handler] — [set_ready false] was
    dropped because the handler only consulted [check].  Extracted
    here as a pure verdict so callers can drive K8s probes (or
    a custom transport) without depending on Kirin's Request/Response
    types, and so the regression is unit-testable. *)
let ready_status t : [ `Ready | `Not_ready_drain | `Not_ready_unhealthy ] =
  if not (is_ready t) then `Not_ready_drain
  else
    let status, _ = check t in
    match status with
    | Healthy | Degraded _ -> `Ready
    | Unhealthy _ -> `Not_ready_unhealthy

(** Readiness probe handler (for /ready). *)
let ready_handler t =
  fun _req ->
    match ready_status t with
    | `Ready ->
      Response.make ~status:`OK ~headers:default_headers
        (`String "{\"status\":\"ready\"}")
    | `Not_ready_drain ->
      Response.make ~status:`Service_unavailable ~headers:default_headers
        (`String "{\"status\":\"not_ready\",\"reason\":\"drain\"}")
    | `Not_ready_unhealthy ->
      Response.make ~status:`Service_unavailable ~headers:default_headers
        (`String "{\"status\":\"not_ready\"}")

(** {1 Routes} *)

(** Create standard health routes *)
let routes t = [
  Router.get "/health" (handler t);
  Router.get "/live" (live_handler t);
  Router.get "/ready" (ready_handler t);
  (* Kubernetes standard paths *)
  Router.get "/healthz" (handler t);
  Router.get "/livez" (live_handler t);
  Router.get "/readyz" (ready_handler t);
]