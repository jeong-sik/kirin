(** Connection Pool module tests.
    Uses Eio_main.run because Pool relies on Eio.Mutex and Eio.Condition. *)

open Alcotest

let next_id = ref 0
let mock_create () = incr next_id; !next_id
let mock_destroy _conn = ()

let make_pool ?min_size ?max_size ?validate () =
  Kirin.Pool.create
    ?min_size
    ?max_size
    ~idle_timeout:300.0
    ~max_wait_time:1.0
    ~create:mock_create
    ~destroy:mock_destroy
    ?validate
    ()

(* -- creation and defaults ----------------------------------------- *)

let test_default_config () =
  let c = Kirin.Pool.default_config in
  check int "min_size" 1 c.Kirin.Pool.min_size;
  check int "max_size" 10 c.Kirin.Pool.max_size

let test_create_pool () =
  Eio_main.run @@ fun _env ->
  let pool = make_pool ~max_size:5 () in
  let s = Kirin.Pool.stats pool in
  check int "total 0" 0 s.Kirin.Pool.total_connections;
  check int "active 0" 0 s.Kirin.Pool.active_connections;
  check int "idle 0" 0 s.Kirin.Pool.idle_connections

let creation_tests = [
  test_case "default config" `Quick test_default_config;
  test_case "create pool" `Quick test_create_pool;
]

(* -- acquire / release --------------------------------------------- *)

let test_acquire_creates_connection () =
  Eio_main.run @@ fun _env ->
  let pool = make_pool ~max_size:5 () in
  let pooled = Kirin.Pool.acquire pool in
  check bool "conn > 0" true (pooled.Kirin.Pool.conn > 0);
  check int "use_count" 1 pooled.Kirin.Pool.use_count;
  let s = Kirin.Pool.stats pool in
  check int "active 1" 1 s.Kirin.Pool.active_connections;
  Kirin.Pool.release pool pooled

let test_release_returns_to_pool () =
  Eio_main.run @@ fun _env ->
  let pool = make_pool ~max_size:5 () in
  let pooled = Kirin.Pool.acquire pool in
  Kirin.Pool.release pool pooled;
  let s = Kirin.Pool.stats pool in
  check int "active 0 after release" 0 s.Kirin.Pool.active_connections;
  check int "idle 1" 1 s.Kirin.Pool.idle_connections

let test_acquire_reuses_idle () =
  Eio_main.run @@ fun _env ->
  let pool = make_pool ~max_size:5 () in
  let p1 = Kirin.Pool.acquire pool in
  let conn_id = p1.Kirin.Pool.conn in
  Kirin.Pool.release pool p1;
  let p2 = Kirin.Pool.acquire pool in
  check int "same connection reused" conn_id p2.Kirin.Pool.conn;
  Kirin.Pool.release pool p2

let test_multiple_acquire () =
  Eio_main.run @@ fun _env ->
  let pool = make_pool ~max_size:5 () in
  let p1 = Kirin.Pool.acquire pool in
  let p2 = Kirin.Pool.acquire pool in
  check bool "different connections" true (p1.Kirin.Pool.conn <> p2.Kirin.Pool.conn);
  let s = Kirin.Pool.stats pool in
  check int "active 2" 2 s.Kirin.Pool.active_connections;
  Kirin.Pool.release pool p1;
  Kirin.Pool.release pool p2

let acquire_tests = [
  test_case "acquire creates" `Quick test_acquire_creates_connection;
  test_case "release returns" `Quick test_release_returns_to_pool;
  test_case "reuses idle" `Quick test_acquire_reuses_idle;
  test_case "multiple acquire" `Quick test_multiple_acquire;
]

(* -- use ----------------------------------------------------------- *)

let test_use_auto_release () =
  Eio_main.run @@ fun _env ->
  let pool = make_pool ~max_size:5 () in
  let result = Kirin.Pool.use pool (fun conn -> conn * 10) in
  check bool "result > 0" true (result > 0);
  let s = Kirin.Pool.stats pool in
  check int "active 0 after use" 0 s.Kirin.Pool.active_connections

let test_use_releases_on_exception () =
  Eio_main.run @@ fun _env ->
  let pool = make_pool ~max_size:5 () in
  (try
     Kirin.Pool.use pool (fun _conn -> failwith "boom")
   with Failure _ -> ());
  let s = Kirin.Pool.stats pool in
  check int "active 0 after exception" 0 s.Kirin.Pool.active_connections

let test_use_pooled () =
  Eio_main.run @@ fun _env ->
  let pool = make_pool ~max_size:5 () in
  let info = Kirin.Pool.use_pooled pool (fun p ->
    (p.Kirin.Pool.conn, p.Kirin.Pool.use_count)
  ) in
  check bool "conn > 0" true (fst info > 0);
  check int "use_count 1" 1 (snd info)

let use_tests = [
  test_case "use auto-release" `Quick test_use_auto_release;
  test_case "use releases on exception" `Quick test_use_releases_on_exception;
  test_case "use_pooled metadata" `Quick test_use_pooled;
]

(* -- pool size / stats --------------------------------------------- *)

let test_size () =
  Eio_main.run @@ fun _env ->
  let pool = make_pool ~max_size:5 () in
  let p1 = Kirin.Pool.acquire pool in
  let (active, idle, max_sz) = Kirin.Pool.size pool in
  check int "active" 1 active;
  check int "idle" 0 idle;
  check int "max" 5 max_sz;
  Kirin.Pool.release pool p1

let test_has_available () =
  Eio_main.run @@ fun _env ->
  let pool = make_pool ~max_size:1 () in
  check bool "available initially" true (Kirin.Pool.has_available pool);
  let p1 = Kirin.Pool.acquire pool in
  check bool "not available at max" false (Kirin.Pool.has_available pool);
  Kirin.Pool.release pool p1;
  check bool "available after release" true (Kirin.Pool.has_available pool)

let test_idle_active_count () =
  Eio_main.run @@ fun _env ->
  let pool = make_pool ~max_size:5 () in
  check int "idle 0" 0 (Kirin.Pool.idle_count pool);
  check int "active 0" 0 (Kirin.Pool.active_count pool);
  let p = Kirin.Pool.acquire pool in
  check int "active 1" 1 (Kirin.Pool.active_count pool);
  Kirin.Pool.release pool p;
  check int "idle 1" 1 (Kirin.Pool.idle_count pool);
  check int "active 0" 0 (Kirin.Pool.active_count pool)

let size_tests = [
  test_case "size tuple" `Quick test_size;
  test_case "has_available" `Quick test_has_available;
  test_case "idle/active count" `Quick test_idle_active_count;
]

(* -- init / shutdown ----------------------------------------------- *)

let test_init_creates_minimum () =
  Eio_main.run @@ fun _env ->
  let pool = make_pool ~min_size:3 ~max_size:5 () in
  Kirin.Pool.init pool;
  check int "idle 3" 3 (Kirin.Pool.idle_count pool)

let test_shutdown () =
  Eio_main.run @@ fun _env ->
  let pool = make_pool ~min_size:2 ~max_size:5 () in
  Kirin.Pool.init pool;
  Kirin.Pool.shutdown pool;
  check int "idle 0 after shutdown" 0 (Kirin.Pool.idle_count pool)

let lifecycle_tests = [
  test_case "init creates minimum" `Quick test_init_creates_minimum;
  test_case "shutdown clears all" `Quick test_shutdown;
]

(* -- maintenance --------------------------------------------------- *)

let test_ensure_minimum () =
  Eio_main.run @@ fun _env ->
  let pool = make_pool ~min_size:3 ~max_size:5 () in
  let created = Kirin.Pool.ensure_minimum pool in
  check int "created 3" 3 created;
  check int "idle 3" 3 (Kirin.Pool.idle_count pool)

let test_validate_all () =
  Eio_main.run @@ fun _env ->
  (* Use a mutable set of valid connections. Start all valid, then invalidate
     one after release (so pool.release keeps it in the idle list). *)
  let invalid_conns = Hashtbl.create 10 in
  let pool = Kirin.Pool.create
    ~min_size:1 ~max_size:5
    ~idle_timeout:300.0 ~max_wait_time:1.0
    ~create:mock_create
    ~destroy:mock_destroy
    ~validate:(fun conn -> not (Hashtbl.mem invalid_conns conn))
    ()
  in
  let p1 = Kirin.Pool.acquire pool in
  let p2 = Kirin.Pool.acquire pool in
  let p3 = Kirin.Pool.acquire pool in
  (* Release all -- all are valid during release so all go to idle *)
  Kirin.Pool.release pool p1;
  Kirin.Pool.release pool p2;
  Kirin.Pool.release pool p3;
  check int "3 idle before invalidation" 3 (Kirin.Pool.idle_count pool);
  (* Now mark p2 as invalid *)
  Hashtbl.replace invalid_conns p2.Kirin.Pool.conn true;
  let removed = Kirin.Pool.validate_all pool in
  check int "1 invalid removed" 1 removed;
  check int "2 idle remain" 2 (Kirin.Pool.idle_count pool)

let maintenance_tests = [
  test_case "ensure_minimum" `Quick test_ensure_minimum;
  test_case "validate_all" `Quick test_validate_all;
]

(* -- error_to_string ----------------------------------------------- *)

let test_error_strings () =
  check string "timeout" "Connection pool timeout"
    (Kirin.Pool.error_to_string Kirin.Pool.Timeout);
  check string "exhausted" "Connection pool exhausted"
    (Kirin.Pool.error_to_string Kirin.Pool.Pool_exhausted);
  check string "failed" "Connection failed: db down"
    (Kirin.Pool.error_to_string (Kirin.Pool.Connection_failed "db down"));
  check string "validation" "Connection validation failed"
    (Kirin.Pool.error_to_string Kirin.Pool.Validation_failed)

let error_tests = [
  test_case "error_to_string" `Quick test_error_strings;
]

(* -- connection_info ----------------------------------------------- *)

let test_connection_info () =
  Eio_main.run @@ fun _env ->
  let pool = make_pool ~max_size:5 () in
  Kirin.Pool.use_pooled pool (fun p ->
    let (age, idle, use_count) = Kirin.Pool.connection_info p in
    check bool "age >= 0" true (age >= 0.0);
    check bool "idle >= 0" true (idle >= 0.0);
    check int "use_count" 1 use_count
  )

let info_tests = [
  test_case "connection_info" `Quick test_connection_info;
]

(* -- run ----------------------------------------------------------- *)

let () =
  Alcotest.run "Pool" [
    ("creation", creation_tests);
    ("acquire/release", acquire_tests);
    ("use", use_tests);
    ("size", size_tests);
    ("lifecycle", lifecycle_tests);
    ("maintenance", maintenance_tests);
    ("error", error_tests);
    ("connection_info", info_tests);
  ]
