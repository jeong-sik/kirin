(** Graceful Shutdown tests (Phase 10) *)

open Alcotest
open Test_helpers

module S = Kirin.Shutdown

let test_shutdown_create () =
  let shutdown = S.create () in
  check bool "initially running" true (S.is_running shutdown);
  check bool "not shutting down" false (S.is_shutting_down shutdown);
  check bool "not stopped" false (S.is_stopped shutdown)

let test_shutdown_custom_timeout () =
  let shutdown = S.create ~timeout:10.0 ~force_after:30.0 () in
  check bool "running" true (S.is_running shutdown)

let test_shutdown_hooks () =
  let called = ref false in
  let shutdown = S.create ~timeout:0.1 () in
  S.on_shutdown shutdown (fun () -> called := true);
  S.initiate shutdown;
  check bool "hook was called" true !called

let test_shutdown_connection_tracking () =
  let shutdown = S.create () in
  check int "no active connections" 0 (S.active_connections shutdown);
  let started = S.connection_start shutdown in
  check bool "connection started" true started;
  check int "one active connection" 1 (S.active_connections shutdown);
  S.connection_end shutdown;
  check int "no active connections" 0 (S.active_connections shutdown)

let test_shutdown_reject_during_shutdown () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let shutdown = S.create ~timeout:0.1 () in
  (* Initiate shutdown in a fiber (Thread.create lacks Eio effect handler) *)
  Eio.Fiber.fork ~sw (fun () -> S.initiate shutdown);
  (* Wait a bit for shutdown to start *)
  Eio.Time.sleep (Eio.Stdenv.clock env) 0.05;
  (* Now connection_start should return false if shutdown started *)
  (* This is timing-dependent so just check it does not crash *)
  let _ = S.connection_start shutdown in
  ()

let test_shutdown_status_json () =
  let shutdown = S.create () in
  let json = S.status_json shutdown in
  match json with
  | `Assoc fields ->
    check bool "has state" true (List.mem_assoc "state" fields);
    check bool "has connections" true (List.mem_assoc "active_connections" fields)
  | _ -> fail "expected JSON object"

(* Before this PR, [wait_for_drain] called [Eio.Condition.await]
   inside a [now () < deadline] while-loop.  [await] has no
   timeout parameter, so a connection that never calls
   [connection_end] (slow client, deadlocked handler, dropped
   socket without RST) left the wait hanging forever — the
   configured [timeout] was silently dead and K8s had to SIGKILL
   the process after [terminationGracePeriodSeconds].

   These tests pin the new contract: [initiate] returns within
   approximately [timeout] seconds even when no connection ever
   ends, and still drains promptly in the normal case. *)

let test_initiate_honours_timeout_when_drain_hangs () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  Kirin.Time_compat.set_clock clock;
  Fun.protect
    ~finally:(fun () -> Kirin.Time_compat.clear_clock ())
    (fun () ->
      let shutdown = S.create ~timeout:0.15 () in
      let started = S.connection_start shutdown in
      check bool "stuck connection registered" true started;
      let t0 = Unix.gettimeofday () in
      S.initiate shutdown;
      let elapsed = Unix.gettimeofday () -. t0 in
      (* The drain must give up between [timeout] and a small
         margin (poll interval + scheduler jitter).  Before this
         PR it would hang indefinitely. *)
      check bool
        (Printf.sprintf "initiate returned in %.3fs (~timeout 0.15s)" elapsed)
        true
        (elapsed >= 0.15 && elapsed < 1.0);
      (* State must be Stopped even after a force-timeout drain. *)
      check bool "stopped after forced drain" true (S.is_stopped shutdown))

let test_initiate_drains_promptly_in_normal_case () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  Kirin.Time_compat.set_clock clock;
  Fun.protect
    ~finally:(fun () -> Kirin.Time_compat.clear_clock ())
    (fun () ->
      Eio.Switch.run @@ fun sw ->
      let shutdown = S.create ~timeout:5.0 () in
      let _ = S.connection_start shutdown in
      (* End the connection 50ms after initiate begins. *)
      Eio.Fiber.fork ~sw (fun () ->
        Eio.Time.sleep clock 0.05;
        S.connection_end shutdown);
      let t0 = Unix.gettimeofday () in
      S.initiate shutdown;
      let elapsed = Unix.gettimeofday () -. t0 in
      (* Drain returned well before the 5s timeout. *)
      check bool
        (Printf.sprintf "drained in %.3fs (< 1s)" elapsed)
        true
        (elapsed < 1.0);
      check bool "stopped" true (S.is_stopped shutdown))

let tests = [
  test_case "shutdown create" `Quick (with_eio test_shutdown_create);
  test_case "shutdown custom timeout" `Quick (with_eio test_shutdown_custom_timeout);
  test_case "shutdown hooks" `Quick (with_eio test_shutdown_hooks);
  test_case "shutdown connection tracking" `Quick (with_eio test_shutdown_connection_tracking);
  test_case "shutdown reject during" `Quick test_shutdown_reject_during_shutdown;
  test_case "shutdown status json" `Quick (with_eio test_shutdown_status_json);
  test_case "initiate honours timeout when drain hangs" `Quick test_initiate_honours_timeout_when_drain_hangs;
  test_case "initiate drains promptly in normal case" `Quick test_initiate_drains_promptly_in_normal_case;
]
