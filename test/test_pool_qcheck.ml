(** Property-based tests for Pool module using QCheck.

    Tests invariants that unit tests may miss by generating random
    acquire/release sequences: capacity bounds, release correctness,
    and use/stats consistency. *)

(* -- Mock connections --------------------------------------------- *)

let next_id = ref 0

let mock_create () =
  incr next_id;
  !next_id

let mock_destroy _conn = ()

let make_pool ?(max_size = 5) () =
  Kirin.Pool.create
    ~min_size:0
    ~max_size
    ~idle_timeout:300.0
    ~max_wait_time:1.0
    ~create:mock_create
    ~destroy:mock_destroy
    ()

(* -- Pool operations for sequence testing ------------------------- *)

type pool_op =
  | Acquire
  | ReleaseOldest

let pool_op_gen =
  QCheck.Gen.(
    oneof_weighted [
      (3, return Acquire);
      (2, return ReleaseOldest);
    ]
  )

let show_pool_op = function
  | Acquire -> "Acquire"
  | ReleaseOldest -> "ReleaseOldest"

(* -- Properties --------------------------------------------------- *)

(** P1: Active connections never exceed max_size.
    Run a sequence of acquire/release operations; after each step
    the number of active connections must be <= max_size. *)
let test_active_never_exceeds_max =
  QCheck.Test.make ~count:500 ~name:"pool: active never exceeds max_size"
    QCheck.(make
      Gen.(let* max_sz = int_range 1 8 in
           let* ops = list_size (int_range 1 30) pool_op_gen in
           return (max_sz, ops))
      ~shrink:QCheck.Shrink.(pair int list)
      ~print:(fun (max_sz, ops) ->
        Printf.sprintf "max_size=%d, ops=[%s]"
          max_sz (String.concat "; " (List.map show_pool_op ops))))
    (fun (max_sz, ops) ->
       Eio_main.run @@ fun _env ->
       next_id := 0;
       let pool = make_pool ~max_size:max_sz () in
       let held = ref [] in
       let invariant_ok = ref true in
       List.iter (fun op ->
         (match op with
          | Acquire ->
            (* Only acquire if under max_size to avoid blocking *)
            if List.length !held < max_sz then begin
              let pooled = Kirin.Pool.acquire pool in
              held := pooled :: !held
            end
          | ReleaseOldest ->
            (match List.rev !held with
             | [] -> ()
             | oldest :: rest ->
               Kirin.Pool.release pool oldest;
               held := List.rev rest));
         let active = Kirin.Pool.active_count pool in
         if active > max_sz then invariant_ok := false
       ) ops;
       (* Release all remaining *)
       List.iter (Kirin.Pool.release pool) !held;
       !invariant_ok)

(** P2: After releasing all acquired connections, active_count = 0. *)
let test_release_all_zeroes_active =
  QCheck.Test.make ~count:300 ~name:"pool: release all then active = 0"
    QCheck.(make
      Gen.(let* max_sz = int_range 1 8 in
           let* n_acq = int_range 1 max_sz in
           return (max_sz, n_acq))
      ~shrink:QCheck.Shrink.(pair int int)
      ~print:(fun (max_sz, n) ->
        Printf.sprintf "max_size=%d, n_acquire=%d" max_sz n))
    (fun (max_sz, n_acq) ->
       Eio_main.run @@ fun _env ->
       next_id := 0;
       let pool = make_pool ~max_size:max_sz () in
       let held = List.init n_acq (fun _ -> Kirin.Pool.acquire pool) in
       List.iter (Kirin.Pool.release pool) held;
       Kirin.Pool.active_count pool = 0)

(** P3: acquire then release increases idle count.
    After acquiring N connections and releasing them all, idle
    connections should be >= 0 and <= max_size. *)
let test_release_increases_idle =
  QCheck.Test.make ~count:300 ~name:"pool: release adds to idle pool"
    QCheck.(make
      Gen.(let* max_sz = int_range 1 8 in
           let* n_acq = int_range 1 max_sz in
           return (max_sz, n_acq))
      ~shrink:QCheck.Shrink.(pair int int)
      ~print:(fun (max_sz, n) ->
        Printf.sprintf "max_size=%d, n_acquire=%d" max_sz n))
    (fun (max_sz, n_acq) ->
       Eio_main.run @@ fun _env ->
       next_id := 0;
       let pool = make_pool ~max_size:max_sz () in
       let held = List.init n_acq (fun _ -> Kirin.Pool.acquire pool) in
       let idle_before = Kirin.Pool.idle_count pool in
       List.iter (Kirin.Pool.release pool) held;
       let idle_after = Kirin.Pool.idle_count pool in
       idle_after >= idle_before && idle_after <= max_sz)

(** P4: Pool.use acquires and releases automatically.
    After use completes, active count returns to pre-use level. *)
let test_use_auto_releases =
  QCheck.Test.make ~count:300 ~name:"pool: use auto-releases on completion"
    QCheck.(make
      Gen.(let* max_sz = int_range 1 8 in
           return max_sz)
      ~shrink:QCheck.Shrink.int
      ~print:string_of_int)
    (fun max_sz ->
       Eio_main.run @@ fun _env ->
       next_id := 0;
       let pool = make_pool ~max_size:max_sz () in
       let active_before = Kirin.Pool.active_count pool in
       Kirin.Pool.use pool (fun _conn -> ());
       let active_after = Kirin.Pool.active_count pool in
       active_after = active_before)

(** P5: Pool.use auto-releases even on exception. *)
let test_use_releases_on_exception =
  QCheck.Test.make ~count:200 ~name:"pool: use releases on exception"
    QCheck.(make
      Gen.(let* max_sz = int_range 1 8 in
           return max_sz)
      ~shrink:QCheck.Shrink.int
      ~print:string_of_int)
    (fun max_sz ->
       Eio_main.run @@ fun _env ->
       next_id := 0;
       let pool = make_pool ~max_size:max_sz () in
       let active_before = Kirin.Pool.active_count pool in
       (try Kirin.Pool.use pool (fun _conn -> failwith "boom")
        with Failure _ -> ());
       let active_after = Kirin.Pool.active_count pool in
       active_after = active_before)

(** P6: Shutdown clears all connections. *)
let test_shutdown_clears =
  QCheck.Test.make ~count:200 ~name:"pool: shutdown clears all connections"
    QCheck.(make
      Gen.(let* max_sz = int_range 1 8 in
           let* n_acq = int_range 1 max_sz in
           return (max_sz, n_acq))
      ~shrink:QCheck.Shrink.(pair int int)
      ~print:(fun (max_sz, n) ->
        Printf.sprintf "max_size=%d, n_acquire=%d" max_sz n))
    (fun (max_sz, n_acq) ->
       Eio_main.run @@ fun _env ->
       next_id := 0;
       let pool = make_pool ~max_size:max_sz () in
       let held = List.init n_acq (fun _ -> Kirin.Pool.acquire pool) in
       List.iter (Kirin.Pool.release pool) held;
       Kirin.Pool.shutdown pool;
       Kirin.Pool.idle_count pool = 0)

(** P7: has_available is true when active < max_size. *)
let test_has_available_consistent =
  QCheck.Test.make ~count:300 ~name:"pool: has_available when active < max_size"
    QCheck.(make
      Gen.(let* max_sz = int_range 2 8 in
           let* n_acq = int_range 0 (max_sz - 1) in
           return (max_sz, n_acq))
      ~shrink:QCheck.Shrink.(pair int int)
      ~print:(fun (max_sz, n) ->
        Printf.sprintf "max_size=%d, n_acquire=%d" max_sz n))
    (fun (max_sz, n_acq) ->
       Eio_main.run @@ fun _env ->
       next_id := 0;
       let pool = make_pool ~max_size:max_sz () in
       let held = List.init n_acq (fun _ -> Kirin.Pool.acquire pool) in
       let available = Kirin.Pool.has_available pool in
       List.iter (Kirin.Pool.release pool) held;
       available)

(** P8: stats.total_acquisitions matches number of successful acquires. *)
let test_stats_acquisitions_count =
  QCheck.Test.make ~count:300 ~name:"pool: stats.total_acquisitions matches acquires"
    QCheck.(make
      Gen.(let* max_sz = int_range 1 8 in
           let* n_acq = int_range 1 max_sz in
           return (max_sz, n_acq))
      ~shrink:QCheck.Shrink.(pair int int)
      ~print:(fun (max_sz, n) ->
        Printf.sprintf "max_size=%d, n_acquire=%d" max_sz n))
    (fun (max_sz, n_acq) ->
       Eio_main.run @@ fun _env ->
       next_id := 0;
       let pool = make_pool ~max_size:max_sz () in
       let held = List.init n_acq (fun _ -> Kirin.Pool.acquire pool) in
       let s = Kirin.Pool.stats pool in
       List.iter (Kirin.Pool.release pool) held;
       s.Kirin.Pool.total_acquisitions = n_acq)

(* -- Test Runner -------------------------------------------------- *)

let () =
  let suite =
    List.map QCheck_alcotest.to_alcotest [
      test_active_never_exceeds_max;
      test_release_all_zeroes_active;
      test_release_increases_idle;
      test_use_auto_releases;
      test_use_releases_on_exception;
      test_shutdown_clears;
      test_has_available_consistent;
      test_stats_acquisitions_count;
    ]
  in
  Alcotest.run "Pool QCheck" [ ("properties", suite) ]
