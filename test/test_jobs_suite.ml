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

let test_jobs_try_submit_ok () =
  with_eio_jobs @@ fun ~sw ~clock ->
  let queue = J.create ~sw ~clock ~workers:2 () in
  (match J.try_submit queue (fun () -> "x") with
   | Ok id -> check bool "got id" true (String.length id > 0)
   | Error `Queue_full -> fail "fresh queue should not be full");
  J.stop queue

let test_jobs_try_submit_queue_full () =
  with_eio_jobs @@ fun ~sw ~clock ->
  (* Tiny capacity. Workers=0 ensures nothing drains during the test. *)
  let queue = J.create ~sw ~clock ~workers:0 ~max_queue_size:2 () in
  (match J.try_submit queue (fun () -> "a") with
   | Ok _ -> ()
   | Error `Queue_full -> fail "1st submit should succeed");
  (match J.try_submit queue (fun () -> "b") with
   | Ok _ -> ()
   | Error `Queue_full -> fail "2nd submit should succeed");
  (match J.try_submit queue (fun () -> "c") with
   | Ok _ -> fail "3rd submit should be rejected"
   | Error `Queue_full -> ());
  J.stop queue

let test_jobs_submit_still_raises_when_full () =
  with_eio_jobs @@ fun ~sw ~clock ->
  let queue = J.create ~sw ~clock ~workers:0 ~max_queue_size:1 () in
  let _ = J.submit queue (fun () -> "x") in
  (match J.submit queue (fun () -> "y") with
   | _ -> fail "submit should raise on full queue"
   | exception Failure _ -> ());
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
  test_case "jobs try_submit ok" `Quick test_jobs_try_submit_ok;
  test_case "jobs try_submit queue full" `Quick test_jobs_try_submit_queue_full;
  test_case "jobs submit still raises when full" `Quick
    test_jobs_submit_still_raises_when_full;
]
