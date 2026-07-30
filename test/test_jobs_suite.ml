(** Jobs tests (Phase 9) -- Eio-native *)

open Alcotest

module J = Kirin.Jobs

(** Run test body inside Eio runtime with switch and clock *)
let with_eio_jobs f =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  f ~sw ~clock

let test_jobs_create () =
  with_eio_jobs @@ fun ~sw ~clock ->
  let queue = J.create ~sw ~clock ~workers:2 () in
  check bool "running" true (J.is_running queue);
  check int "pending" 0 (J.pending_count queue);
  J.stop queue

let test_jobs_submit () =
  with_eio_jobs @@ fun ~sw ~clock ->
  let queue = J.create ~sw ~clock ~workers:2 () in
  let job_id = J.submit queue (fun () -> "result") in
  check bool "has job id" true (String.length job_id > 0);
  J.stop queue

let test_jobs_submit_and_wait () =
  with_eio_jobs @@ fun ~sw ~clock ->
  let queue = J.create ~sw ~clock ~workers:2 () in
  let result = J.submit_and_wait queue (fun () -> "hello") in
  (match result with
   | J.Completed v -> check string "completed" "hello" v
   | J.Failed e -> fail ("Failed: " ^ Printexc.to_string e)
   | _ -> fail "Unexpected status");
  J.stop queue

let test_jobs_run_sync () =
  let result = J.run_sync (fun () -> 42) in
  check int "sync result" 42 result

let test_jobs_run_once () =
  with_eio_jobs @@ fun ~sw:_ ~clock ->
  let result = J.run_once ~clock (fun () -> "done") in
  match result with
  | J.Completed v -> check string "completed" "done" v
  | J.Failed e -> fail ("Failed: " ^ Printexc.to_string e)
  | _ -> fail "Unexpected status"

let test_jobs_stats () =
  with_eio_jobs @@ fun ~sw ~clock ->
  let queue = J.create ~sw ~clock ~workers:2 () in
  let _ = J.submit queue (fun () -> 1) in
  let _ = J.submit queue (fun () -> 2) in
  let stats = J.stats queue in
  check int "submitted" 2 stats.total_submitted;
  J.stop queue

let test_jobs_cancel () =
  with_eio_jobs @@ fun ~sw ~clock ->
  let queue = J.create ~sw ~clock ~workers:1 () in
  let job_id = J.submit queue (fun () -> "result") in
  let cancelled = J.cancel queue job_id in
  check bool "cancelled" true cancelled;
  check int "pending after cancel" 0 (J.pending_count queue);
  J.stop queue

let test_jobs_clear () =
  with_eio_jobs @@ fun ~sw ~clock ->
  let queue = J.create ~sw ~clock ~workers:1 () in
  let _ = J.submit queue (fun () -> 1) in
  let _ = J.submit queue (fun () -> 2) in
  let _ = J.submit queue (fun () -> 3) in
  let cleared = J.clear queue in
  check int "cleared count" 3 cleared;
  check int "pending after clear" 0 (J.pending_count queue);
  J.stop queue

let test_jobs_priority () =
  with_eio_jobs @@ fun ~sw ~clock ->
  let queue = J.create ~sw ~clock ~workers:1 () in
  let _ = J.submit ~priority:J.Low queue (fun () -> "low") in
  let _ = J.submit ~priority:J.Critical queue (fun () -> "critical") in
  let _ = J.submit ~priority:J.Normal queue (fun () -> "normal") in
  (* Jobs should be ordered: Critical, Normal, Low *)
  let stats = J.stats queue in
  check int "all queued" 3 stats.queue_size;
  J.stop queue

let test_jobs_try_status_known_and_unknown () =
  with_eio_jobs @@ fun ~sw ~clock ->
  let queue = J.create ~sw ~clock ~workers:2 () in
  let id = J.submit queue (fun () -> 42) in
  (match J.try_status queue id with
   | Ok _ -> ()
   | Error `Unknown_job -> fail "known job_id should not be Unknown_job");
  (match J.try_status queue "bogus_id" with
   | Ok _ -> fail "bogus id should not return Ok"
   | Error `Unknown_job -> ());
  J.stop queue

let test_jobs_try_wait_known_and_unknown () =
  with_eio_jobs @@ fun ~sw ~clock ->
  let queue = J.create ~sw ~clock ~workers:2 () in
  let id = J.submit queue (fun () -> "done") in
  (match J.try_wait queue id with
   | Ok (J.Completed "done") -> ()
   | Ok other ->
       fail (Printf.sprintf "unexpected status: %s"
               (match other with
                | J.Pending -> "Pending"
                | J.Running -> "Running"
                | J.Completed _ -> "Completed(other)"
                | J.Failed _ -> "Failed"))
   | Error `Unknown_job -> fail "known job_id should not be Unknown_job");
  (match J.try_wait queue "bogus_id" with
   | Ok _ -> fail "bogus id should not return Ok"
   | Error `Unknown_job -> ());
  J.stop queue

let tests = [
  test_case "jobs create" `Quick test_jobs_create;
  test_case "jobs submit" `Quick test_jobs_submit;
  test_case "jobs submit and wait" `Quick test_jobs_submit_and_wait;
  test_case "jobs run_sync" `Quick test_jobs_run_sync;
  test_case "jobs run_once" `Quick test_jobs_run_once;
  test_case "jobs stats" `Quick test_jobs_stats;
  test_case "jobs cancel" `Quick test_jobs_cancel;
  test_case "jobs clear" `Quick test_jobs_clear;
  test_case "jobs priority" `Quick test_jobs_priority;
  test_case "jobs try_status known and unknown" `Quick
    test_jobs_try_status_known_and_unknown;
  test_case "jobs try_wait known and unknown" `Quick
    test_jobs_try_wait_known_and_unknown;
]
