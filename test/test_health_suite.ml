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

(* Before this PR, a raising check escaped through [Hashtbl.fold]
   and collapsed the entire aggregation — every probe (/health,
   /ready, /healthz, /readyz) returned 500 because of a single
   buggy check, even when the other checks were fine.  The old
   [test_health_exception] test pinned that *buggy* behaviour.
   This test pins the new contract: a raised exception becomes a
   named [Unhealthy] status, and the other registered checks still
   contribute their own results. *)
let test_health_exception_is_caught () =
  let health = H.create () in
  H.register health "throws" (fun () -> failwith "boom");
  H.register health "good" (fun () -> H.Healthy);
  let status, json = H.check health in
  (* Aggregate is Unhealthy because one check raised, but it must
     not raise — the call returned cleanly. *)
  (match status with
   | H.Unhealthy _ -> ()
   | _ -> fail "expected Unhealthy aggregate after one check raised");
  (* The good check still produced a result; both names must be
     present in the JSON details. *)
  let details_names = match json with
    | `Assoc fields ->
      (match List.assoc_opt "details" fields with
       | Some (`Assoc d) -> List.map fst d
       | _ -> [])
    | _ -> []
  in
  let names_sorted = List.sort compare details_names in
  check (list string) "both checks present" ["good"; "throws"] names_sorted

let test_health_exception_status_names_the_check () =
  (* The Unhealthy message names the check that raised — operators
     need to know which probe died without grepping logs. *)
  let health = H.create () in
  H.register health "db-primary" (fun () -> failwith "kaboom");
  let status, _ = H.check health in
  match status with
  | H.Unhealthy msg ->
    let has_substring s sub =
      let n = String.length sub in
      let m = String.length s in
      let rec loop i =
        if i + n > m then false
        else if String.sub s i n = sub then true
        else loop (i + 1)
      in
      loop 0
    in
    check bool "names the check" true (has_substring msg "db-primary");
    check bool "names the failure" true (has_substring msg "kaboom")
  | _ -> fail "expected Unhealthy"

(* ready_status pure-verdict tests pin the regression where
   [set_ready false] was a silent no-op in [ready_handler]
   (manual drain dropped for the K8s graceful-shutdown flow). *)
let test_ready_status_healthy () =
  let health = H.create () in
  H.register health "ok" (fun () -> H.Healthy);
  match H.ready_status health with
  | `Ready -> ()
  | _ -> fail "expected `Ready"

let test_ready_status_drain () =
  let health = H.create () in
  H.register health "ok" (fun () -> H.Healthy);
  H.set_ready health false;
  match H.ready_status health with
  | `Not_ready_drain -> ()
  | `Ready -> fail "drain flag was ignored — regression"
  | `Not_ready_unhealthy -> fail "drain should report drain, not unhealthy"

let test_ready_status_unhealthy_overrides_ready_flag () =
  (* Manual ready=true must NOT mask a real unhealthy aggregate;
     the K8s contract is that /ready only succeeds when both gates
     are open. *)
  let health = H.create () in
  H.register health "broken" (fun () -> H.Unhealthy "fail");
  H.set_ready health true;
  match H.ready_status health with
  | `Not_ready_unhealthy -> ()
  | _ -> fail "expected `Not_ready_unhealthy"

let test_ready_status_drain_wins_over_unhealthy () =
  (* When both gates are closed, drain is the better signal for an
     operator: the unhealthy check might be a downstream effect of
     the shutdown.  Pin which signal wins. *)
  let health = H.create () in
  H.register health "broken" (fun () -> H.Unhealthy "fail");
  H.set_ready health false;
  match H.ready_status health with
  | `Not_ready_drain -> ()
  | _ -> fail "drain should win over unhealthy"

let tests = [
  test_case "health create" `Quick test_health_create;
  test_case "health register" `Quick test_health_register;
  test_case "health healthy" `Quick test_health_healthy;
  test_case "health unhealthy" `Quick test_health_unhealthy;
  test_case "health degraded" `Quick test_health_degraded;
  test_case "health ready control" `Quick test_health_ready_control;
  test_case "health uptime" `Quick test_health_uptime;
  test_case "raised check is caught" `Quick test_health_exception_is_caught;
  test_case "raised check is named in status" `Quick test_health_exception_status_names_the_check;
  test_case "ready_status healthy" `Quick test_ready_status_healthy;
  test_case "ready_status drain" `Quick test_ready_status_drain;
  test_case "ready_status unhealthy overrides ready flag" `Quick test_ready_status_unhealthy_overrides_ready_flag;
  test_case "ready_status drain wins over unhealthy" `Quick test_ready_status_drain_wins_over_unhealthy;
]
