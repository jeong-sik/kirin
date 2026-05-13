(** Health check tests (Phase 10) *)

open Alcotest

module H = Kirin.Health

let test_health_create () =
  let health = H.create () in
  check bool "initially ready" true (H.is_ready health)

let test_health_register () =
  let health = H.create () in
  H.register health "db" (fun () -> H.Healthy);
  H.register health "cache" (fun () -> H.Healthy);
  let _, json = H.check health in
  let details = match json with
    | `Assoc fields ->
        (match List.assoc_opt "details" fields with
         | Some (`Assoc d) -> d
         | _ -> [])
    | _ -> []
  in
  check int "two checks" 2 (List.length details)

let test_health_healthy () =
  let health = H.create () in
  H.register health "service" (fun () -> H.Healthy);
  let status, _ = H.check health in
  match status with
  | H.Healthy -> ()
  | _ -> fail "expected healthy status"

let test_health_unhealthy () =
  let health = H.create () in
  H.register health "failing" (fun () -> H.Unhealthy "connection failed");
  let status, _ = H.check health in
  match status with
  | H.Unhealthy _ -> ()
  | _ -> fail "expected unhealthy status"

let test_health_degraded () =
  let health = H.create () in
  H.register health "slow" (fun () -> H.Degraded "high latency");
  let status, _ = H.check health in
  match status with
  | H.Degraded _ -> ()
  | _ -> fail "expected degraded status"

let test_health_ready_control () =
  let health = H.create () in
  check bool "initially ready" true (H.is_ready health);
  H.set_ready health false;
  check bool "now not ready" false (H.is_ready health);
  H.set_ready health true;
  check bool "ready again" true (H.is_ready health)

let test_health_uptime () =
  Eio_main.run @@ fun env ->
  let health = H.create () in
  Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
  let _, json = H.check health in
  let uptime = match json with
    | `Assoc fields ->
        (match List.assoc_opt "uptime" fields with
         | Some (`Float u) -> u
         | _ -> 0.0)
    | _ -> 0.0
  in
  check bool "uptime > 0" true (uptime > 0.0)

let test_health_exception () =
  let health = H.create () in
  H.register health "throws" (fun () -> failwith "boom");
  try
    let _ = H.check health in
    fail "expected exception"
  with _ -> ()

let tests = [
  test_case "health create" `Quick test_health_create;
  test_case "health register" `Quick test_health_register;
  test_case "health healthy" `Quick test_health_healthy;
  test_case "health unhealthy" `Quick test_health_unhealthy;
  test_case "health degraded" `Quick test_health_degraded;
  test_case "health ready control" `Quick test_health_ready_control;
  test_case "health uptime" `Quick test_health_uptime;
  test_case "health exception" `Quick test_health_exception;
]
