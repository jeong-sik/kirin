(** Connection Pool tests (Phase 9) *)

open Alcotest
open Test_helpers

let test_pool_create () =
  let counter = ref 0 in
  let pool =
    Kirin.Pool.create
      ~min_size:1
      ~max_size:5
      ~create:(fun () ->
        incr counter;
        !counter)
      ~destroy:(fun _ -> ())
      ()
  in
  check int "initial in_use" 0 (Kirin.Pool.active_count pool);
  check bool "has available" true (Kirin.Pool.has_available pool)
;;

let test_pool_acquire_release () =
  let pool =
    Kirin.Pool.create
      ~max_size:3
      ~create:(fun () -> "connection")
      ~destroy:(fun _ -> ())
      ()
  in
  let conn = Kirin.Pool.acquire pool in
  check string "got connection" "connection" conn.Kirin.Pool.conn;
  check int "active count" 1 (Kirin.Pool.active_count pool);
  Kirin.Pool.release pool conn;
  check int "after release" 0 (Kirin.Pool.active_count pool);
  check int "idle count" 1 (Kirin.Pool.idle_count pool)
;;

let test_pool_use () =
  let pool =
    Kirin.Pool.create ~max_size:2 ~create:(fun () -> 42) ~destroy:(fun _ -> ()) ()
  in
  let result = Kirin.Pool.use pool (fun conn -> conn * 2) in
  check int "use result" 84 result;
  check int "released" 0 (Kirin.Pool.active_count pool)
;;

let test_pool_stats () =
  let pool =
    Kirin.Pool.create ~max_size:5 ~create:(fun () -> ()) ~destroy:(fun _ -> ()) ()
  in
  let _ = Kirin.Pool.use pool (fun _ -> ()) in
  let _ = Kirin.Pool.use pool (fun _ -> ()) in
  let stats = Kirin.Pool.stats pool in
  check int "total acquisitions" 2 stats.total_acquisitions
;;

let test_pool_validate () =
  let valid = ref true in
  let pool =
    Kirin.Pool.create
      ~max_size:2
      ~create:(fun () -> "conn")
      ~destroy:(fun _ -> ())
      ~validate:(fun _ -> !valid)
      ()
  in
  (* First acquire creates connection *)
  let _ = Kirin.Pool.use pool (fun _ -> ()) in
  check int "idle after use" 1 (Kirin.Pool.idle_count pool);
  (* Invalidate connections *)
  valid := false;
  let removed = Kirin.Pool.validate_all pool in
  check int "removed invalid" 1 removed;
  check int "idle after validate" 0 (Kirin.Pool.idle_count pool)
;;

let test_pool_shutdown () =
  let destroyed = ref 0 in
  let pool =
    Kirin.Pool.create
      ~max_size:3
      ~create:(fun () -> "conn")
      ~destroy:(fun _ -> incr destroyed)
      ()
  in
  (* Create some connections *)
  let _ = Kirin.Pool.use pool (fun _ -> ()) in
  let _ = Kirin.Pool.use pool (fun _ -> ()) in
  Kirin.Pool.shutdown pool;
  check int "idle after shutdown" 0 (Kirin.Pool.idle_count pool)
;;

let test_pool_error_to_string () =
  check
    string
    "timeout error"
    "Connection pool timeout"
    (Kirin.Pool.error_to_string Kirin.Pool.Timeout);
  check
    string
    "exhausted error"
    "Connection pool exhausted"
    (Kirin.Pool.error_to_string Kirin.Pool.Pool_exhausted)
;;

let tests =
  [ test_case "pool create" `Quick (with_eio test_pool_create)
  ; test_case "acquire release" `Quick (with_eio test_pool_acquire_release)
  ; test_case "pool use" `Quick (with_eio test_pool_use)
  ; test_case "pool stats" `Quick (with_eio test_pool_stats)
  ; test_case "pool validate" `Quick (with_eio test_pool_validate)
  ; test_case "pool shutdown" `Quick (with_eio test_pool_shutdown)
  ; test_case "error to string" `Quick (with_eio test_pool_error_to_string)
  ]
;;
