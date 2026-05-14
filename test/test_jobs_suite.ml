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

(* Before this PR the [retry_delay] in [J.config] was dead config:
   the retry branch in [process_job] re-queued failed jobs
   synchronously, so a task that always raises burned through
   [max_retries] in zero wall-clock time and pinned a worker into a
   tight retry spin.  These tests pin the new behaviour:

   - retry_delay is *actually applied* on each retry
   - the job still eventually fails after [max_retries]
   - other workers in the same queue are not blocked by a retrying
     job (sleep happens outside the mutex) *)

let test_retry_delay_is_applied () =
  with_eio_jobs @@ fun ~sw ~clock ->
  (* Single worker, 0.05s retry delay, max_retries=3.  A task that
     always raises should take *at least* 2 × 0.05s = 0.1s of
     wall-clock time before reaching the Failed state.  Without the
     fix, the same workload completed in microseconds. *)
  let queue = J.create ~sw ~clock ~workers:1 ~retry_delay:0.05 () in
  let start = Eio.Time.now clock in
  let result =
    J.submit_and_wait
      ~max_retries:3
      queue
      (fun () -> failwith "always fails")
  in
  let elapsed = Eio.Time.now clock -. start in
  (match result with
   | J.Failed _ -> ()
   | _ -> fail "expected Failed after retries");
  (* 3 attempts means 2 retries inserted, each preceded by a 0.05s
     sleep.  Allow a small margin for scheduler jitter. *)
  check bool
    (Printf.sprintf "elapsed %.3fs >= 0.08s" elapsed)
    true
    (elapsed >= 0.08);
  J.stop queue

let test_retry_eventually_fails () =
  with_eio_jobs @@ fun ~sw ~clock ->
  let queue = J.create ~sw ~clock ~workers:1 ~retry_delay:0.0 () in
  let attempts = ref 0 in
  let result =
    J.submit_and_wait
      ~max_retries:3
      queue
      (fun () ->
        incr attempts;
        failwith "always fails")
  in
  (match result with
   | J.Failed _ -> ()
   | _ -> fail "expected Failed status");
  check int "ran exactly max_retries attempts" 3 !attempts;
  J.stop queue

let test_retry_does_not_block_other_workers () =
  with_eio_jobs @@ fun ~sw ~clock ->
  (* Two workers, slow retry on the failing job, fast success on the
     other.  If the retry-delay sleep were taken under the mutex the
     succeeding job would have to wait at least one full delay
     period; with the fix it should complete in well under that. *)
  let queue = J.create ~sw ~clock ~workers:2 ~retry_delay:0.5 () in
  let _failing =
    J.submit
      ~priority:J.Low
      ~max_retries:5
      queue
      (fun () -> failwith "always fails")
  in
  (* Give the failing job a head start so it claims one of the two
     workers and enters its retry sleep. *)
  Eio.Time.sleep clock 0.02;
  let succ_start = Eio.Time.now clock in
  let succ_id = J.submit queue (fun () -> "ok") in
  let res = J.wait queue succ_id in
  let succ_elapsed = Eio.Time.now clock -. succ_start in
  (match res with
   | J.Completed v -> check string "got ok" "ok" v
   | _ -> fail "expected Completed");
  check bool
    (Printf.sprintf "succeeding job completed in %.3fs (< 0.3s)" succ_elapsed)
    true
    (succ_elapsed < 0.3);
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
  test_case "retry_delay is applied" `Quick test_retry_delay_is_applied;
  test_case "retry eventually fails after max_retries" `Quick test_retry_eventually_fails;
  test_case "retry does not block other workers" `Quick test_retry_does_not_block_other_workers;
]
