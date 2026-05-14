(** Cluster supervisor decision tests.

    These pin the pure [compute_backoff_delay] / [should_give_up] /
    [next_recent_crashes] helpers that the supervisor loop drives.
    The supervisor loop itself forks subprocesses and is integration-
    territory; pinning the decisions in isolation catches the
    fork-bomb regression — a crash loop that respawns immediately —
    without the test having to fork. *)

open Alcotest

(* -- compute_backoff_delay ---------------------------------------- *)

let test_backoff_zero_when_alive_long_enough () =
  (* Worker lived [min_restart_interval] seconds → no sleep. *)
  let d =
    Kirin.Cluster.compute_backoff_delay
      ~last_spawn:100.0
      ~now:101.5
      ~min_restart_interval:1.0
  in
  check (float 0.0001) "no backoff" 0.0 d

let test_backoff_positive_on_fast_crash () =
  (* Worker crashed 0.3s after spawn under a 1.0s interval →
     supervisor must sleep ~0.7s before the next spawn. *)
  let d =
    Kirin.Cluster.compute_backoff_delay
      ~last_spawn:100.0
      ~now:100.3
      ~min_restart_interval:1.0
  in
  check (float 0.0001) "0.7s delay" 0.7 d

let test_backoff_zero_at_exact_boundary () =
  (* elapsed == min_restart_interval → no sleep (>= in the impl). *)
  let d =
    Kirin.Cluster.compute_backoff_delay
      ~last_spawn:0.0
      ~now:1.0
      ~min_restart_interval:1.0
  in
  check (float 0.0001) "boundary is fast-enough" 0.0 d

(* -- should_give_up ------------------------------------------------ *)

let test_give_up_under_cap () =
  check bool "below cap" false
    (Kirin.Cluster.should_give_up ~recent_crashes:9 ~max_consecutive_crashes:10)

let test_give_up_at_cap () =
  check bool "at cap" true
    (Kirin.Cluster.should_give_up ~recent_crashes:10 ~max_consecutive_crashes:10)

let test_give_up_above_cap () =
  check bool "above cap" true
    (Kirin.Cluster.should_give_up ~recent_crashes:11 ~max_consecutive_crashes:10)

(* -- next_recent_crashes ------------------------------------------ *)

let test_recent_crashes_resets_on_stable_run () =
  (* A spawn that lived through the interval is "stable enough" —
     the fast-crash counter resets to 1 on the next crash, not
     [recent + 1].  This is how the supervisor avoids declaring
     a crash loop after a long-running worker finally crashes once. *)
  let n =
    Kirin.Cluster.next_recent_crashes
      ~last_spawn:0.0
      ~now:5.0
      ~min_restart_interval:1.0
      ~recent_crashes:9
  in
  check int "reset to 1" 1 n

let test_recent_crashes_increments_on_fast_crash () =
  let n =
    Kirin.Cluster.next_recent_crashes
      ~last_spawn:0.0
      ~now:0.2
      ~min_restart_interval:1.0
      ~recent_crashes:3
  in
  check int "incremented" 4 n

(* The end-to-end invariant the supervisor needs: a worker that
   reliably crashes faster than [min_restart_interval] must hit the
   give-up condition within [max_consecutive_crashes] respawns.
   Simulate that without forking. *)
let test_crash_loop_gives_up_within_cap () =
  let min_restart_interval = 1.0 in
  let max_consecutive_crashes = 5 in
  let crashes = ref 0 in
  let last_spawn = ref 0.0 in
  let now = ref 0.1 in (* crashes 0.1s after every spawn *)
  let rec step () =
    crashes :=
      Kirin.Cluster.next_recent_crashes
        ~last_spawn:!last_spawn
        ~now:!now
        ~min_restart_interval
        ~recent_crashes:!crashes;
    if Kirin.Cluster.should_give_up
         ~recent_crashes:!crashes
         ~max_consecutive_crashes
    then !crashes
    else begin
      (* simulate spawn at [now], crash at [now + 0.1] *)
      last_spawn := !now;
      now := !now +. 0.1;
      step ()
    end
  in
  let final = step () in
  check int "give up at cap" max_consecutive_crashes final

let tests = [
  test_case "no backoff after stable run" `Quick test_backoff_zero_when_alive_long_enough;
  test_case "backoff on fast crash" `Quick test_backoff_positive_on_fast_crash;
  test_case "no backoff at exact boundary" `Quick test_backoff_zero_at_exact_boundary;
  test_case "give_up under cap" `Quick test_give_up_under_cap;
  test_case "give_up at cap" `Quick test_give_up_at_cap;
  test_case "give_up above cap" `Quick test_give_up_above_cap;
  test_case "recent_crashes resets after stable run" `Quick test_recent_crashes_resets_on_stable_run;
  test_case "recent_crashes increments on fast crash" `Quick test_recent_crashes_increments_on_fast_crash;
  test_case "crash loop hits cap" `Quick test_crash_loop_gives_up_within_cap;
]
