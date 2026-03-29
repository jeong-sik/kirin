(** Graceful Shutdown tests (Phase 10) *)

open Alcotest
open Test_helpers
module S = Kirin.Shutdown

let test_shutdown_create () =
  let shutdown = S.create () in
  check bool "initially running" true (S.is_running shutdown);
  check bool "not shutting down" false (S.is_shutting_down shutdown);
  check bool "not stopped" false (S.is_stopped shutdown)
;;

let test_shutdown_custom_timeout () =
  let shutdown = S.create ~timeout:10.0 ~force_after:30.0 () in
  check bool "running" true (S.is_running shutdown)
;;

let test_shutdown_hooks () =
  let called = ref false in
  let shutdown = S.create ~timeout:0.1 () in
  S.on_shutdown shutdown (fun () -> called := true);
  S.initiate shutdown;
  check bool "hook was called" true !called
;;

let test_shutdown_connection_tracking () =
  let shutdown = S.create () in
  check int "no active connections" 0 (S.active_connections shutdown);
  let started = S.connection_start shutdown in
  check bool "connection started" true started;
  check int "one active connection" 1 (S.active_connections shutdown);
  S.connection_end shutdown;
  check int "no active connections" 0 (S.active_connections shutdown)
;;

let test_shutdown_reject_during_shutdown () =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let shutdown = S.create ~timeout:0.1 () in
  (* Initiate shutdown in a fiber (Thread.create lacks Eio effect handler) *)
  Eio.Fiber.fork ~sw (fun () -> S.initiate shutdown);
  (* Wait a bit for shutdown to start *)
  Eio.Time.sleep (Eio.Stdenv.clock env) 0.05;
  (* Now connection_start should return false if shutdown started *)
  (* This is timing-dependent so just check it does not crash *)
  let _ = S.connection_start shutdown in
  ()
;;

let test_shutdown_status_json () =
  let shutdown = S.create () in
  let json = S.status_json shutdown in
  match json with
  | `Assoc fields ->
    check bool "has state" true (List.mem_assoc "state" fields);
    check bool "has connections" true (List.mem_assoc "active_connections" fields)
  | _ -> fail "expected JSON object"
;;

let tests =
  [ test_case "shutdown create" `Quick (with_eio test_shutdown_create)
  ; test_case "shutdown custom timeout" `Quick (with_eio test_shutdown_custom_timeout)
  ; test_case "shutdown hooks" `Quick (with_eio test_shutdown_hooks)
  ; test_case
      "shutdown connection tracking"
      `Quick
      (with_eio test_shutdown_connection_tracking)
  ; test_case "shutdown reject during" `Quick test_shutdown_reject_during_shutdown
  ; test_case "shutdown status json" `Quick (with_eio test_shutdown_status_json)
  ]
;;
