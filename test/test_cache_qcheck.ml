(** Property-based tests for Cache module using QCheck.

    Tests invariants that unit tests may miss by generating random
    operation sequences: capacity bounds, LRU eviction order,
    roundtrip consistency, and expiry behavior. *)

(* -- Generators --------------------------------------------------- *)

(** Generate a cache key from a small alphabet to increase collision rate. *)
let key_gen =
  QCheck.Gen.(
    let* len = int_range 1 5 in
    let* chars = list_size (return len) (char_range 'a' 'e') in
    return (String.init len (List.nth chars))
  )

(** Generate a string value. *)
let value_gen = QCheck.Gen.string_size (QCheck.Gen.int_range 1 20)

(** Cache operations for sequence-based testing. *)
type cache_op =
  | Set of string * string
  | Get of string
  | Remove of string

let cache_op_gen =
  QCheck.Gen.(
    oneof [
      (let* k = key_gen in let* v = value_gen in return (Set (k, v)));
      (let* k = key_gen in return (Get k));
      (let* k = key_gen in return (Remove k));
    ]
  )

let show_cache_op = function
  | Set (k, v) -> Printf.sprintf "Set(%s, %s)" k v
  | Get k -> Printf.sprintf "Get(%s)" k
  | Remove k -> Printf.sprintf "Remove(%s)" k

(* -- Properties --------------------------------------------------- *)

(** P1: After any sequence of operations, size <= max_size. *)
let test_capacity_invariant =
  QCheck.Test.make ~count:500 ~name:"cache: size never exceeds max_size"
    QCheck.(make
      Gen.(let* max_sz = int_range 1 10 in
           let* ops = list_size (int_range 0 50) cache_op_gen in
           return (max_sz, ops))
      ~print:(fun (max_sz, ops) ->
        Printf.sprintf "max_size=%d, ops=[%s]"
          max_sz (String.concat "; " (List.map show_cache_op ops))))
    (fun (max_sz, ops) ->
       Eio_main.run @@ fun _env ->
       let cache = Kirin.Cache.create ~max_size:max_sz () in
       List.iter (fun op ->
         match op with
         | Set (k, v) -> Kirin.Cache.set cache k v
         | Get k -> ignore (Kirin.Cache.get cache k)
         | Remove k -> ignore (Kirin.Cache.remove cache k)
       ) ops;
       Kirin.Cache.size cache <= max_sz)

(** P2: set k v then get k returns Some v (when no eviction occurs). *)
let test_roundtrip_no_eviction =
  QCheck.Test.make ~count:500 ~name:"cache: set then get returns value (no eviction)"
    QCheck.(make
      Gen.(let* k = key_gen in
           let* v = value_gen in
           return (k, v))
      ~print:(fun (k, v) -> Printf.sprintf "key=%s, value=%s" k v))
    (fun (k, v) ->
       Eio_main.run @@ fun _env ->
       let cache = Kirin.Cache.create ~max_size:100 () in
       Kirin.Cache.set cache k v;
       Kirin.Cache.get cache k = Some v)

(** P3: Overwrite preserves the latest value. *)
let test_overwrite_latest =
  QCheck.Test.make ~count:300 ~name:"cache: overwrite returns latest value"
    QCheck.(make
      Gen.(let* k = key_gen in
           let* v1 = value_gen in
           let* v2 = value_gen in
           return (k, v1, v2))
      ~print:(fun (k, v1, v2) ->
        Printf.sprintf "key=%s, v1=%s, v2=%s" k v1 v2))
    (fun (k, _v1, v2) ->
       Eio_main.run @@ fun _env ->
       let cache = Kirin.Cache.create ~max_size:100 () in
       Kirin.Cache.set cache k "first";
       Kirin.Cache.set cache k v2;
       Kirin.Cache.get cache k = Some v2)

(** P4: clear always brings size to 0. *)
let test_clear_empties =
  QCheck.Test.make ~count:300 ~name:"cache: clear then size = 0"
    QCheck.(make
      Gen.(list_size (int_range 0 30) cache_op_gen)
      ~print:(fun ops ->
        Printf.sprintf "ops=[%s]"
          (String.concat "; " (List.map show_cache_op ops))))
    (fun ops ->
       Eio_main.run @@ fun _env ->
       let cache = Kirin.Cache.create ~max_size:10 () in
       List.iter (fun op ->
         match op with
         | Set (k, v) -> Kirin.Cache.set cache k v
         | Get k -> ignore (Kirin.Cache.get cache k)
         | Remove k -> ignore (Kirin.Cache.remove cache k)
       ) ops;
       Kirin.Cache.clear cache;
       Kirin.Cache.size cache = 0)

(** P5: remove k then get k returns None. *)
let test_remove_then_gone =
  QCheck.Test.make ~count:300 ~name:"cache: remove then get returns None"
    QCheck.(make
      Gen.(let* k = key_gen in
           let* v = value_gen in
           return (k, v))
      ~print:(fun (k, v) -> Printf.sprintf "key=%s, value=%s" k v))
    (fun (k, v) ->
       Eio_main.run @@ fun _env ->
       let cache = Kirin.Cache.create ~max_size:100 () in
       Kirin.Cache.set cache k v;
       ignore (Kirin.Cache.remove cache k);
       Kirin.Cache.get cache k = None)

(** P6: LRU eviction removes the least recently accessed key.
    Insert max_size distinct keys, access all except the first,
    insert one more key to trigger eviction, check the first is gone. *)
let test_lru_eviction_order =
  QCheck.Test.make ~count:200 ~name:"cache: LRU eviction removes least recently used"
    QCheck.(make
      Gen.(let* max_sz = int_range 2 8 in
           return max_sz)
      ~print:string_of_int)
    (fun max_sz ->
       Eio_main.run @@ fun _env ->
       let cache = Kirin.Cache.create ~max_size:max_sz () in
       (* Insert max_sz distinct keys: "k0", "k1", ..., "k{max_sz-1}" *)
       for i = 0 to max_sz - 1 do
         let k = Printf.sprintf "k%d" i in
         Kirin.Cache.set cache k (Printf.sprintf "v%d" i)
       done;
       (* Access all keys except "k0" to make them more recently used *)
       for i = 1 to max_sz - 1 do
         let k = Printf.sprintf "k%d" i in
         ignore (Kirin.Cache.get cache k)
       done;
       (* Insert one more to trigger eviction *)
       Kirin.Cache.set cache "trigger" "evict";
       (* "k0" should be evicted (least recently used) *)
       let k0_gone = Kirin.Cache.get cache "k0" = None in
       (* "trigger" should be present *)
       let trigger_present = Kirin.Cache.get cache "trigger" = Some "evict" in
       (* Size should still be max_sz *)
       let size_ok = Kirin.Cache.size cache <= max_sz in
       k0_gone && trigger_present && size_ok)

(** P7: Expired entries (TTL=0) are not returned by get. *)
let test_expired_not_returned =
  QCheck.Test.make ~count:100 ~name:"cache: expired entries return None"
    QCheck.(make
      Gen.(let* k = key_gen in
           let* v = value_gen in
           return (k, v))
      ~print:(fun (k, v) -> Printf.sprintf "key=%s, value=%s" k v))
    (fun (k, v) ->
       Eio_main.run @@ fun _env ->
       let cache = Kirin.Cache.create ~max_size:100 () in
       (* Set with TTL that expires immediately *)
       Kirin.Cache.set ~ttl:0.0 cache k v;
       (* Small busy-wait to ensure expiry (gettimeofday resolution) *)
       Unix.sleepf 0.001;
       Kirin.Cache.get cache k = None)

(** P8: Stats hits + misses = total get operations. *)
let test_stats_consistency =
  QCheck.Test.make ~count:300 ~name:"cache: hits + misses = total gets"
    QCheck.(make
      Gen.(list_size (int_range 1 40) cache_op_gen)
      ~print:(fun ops ->
        Printf.sprintf "ops=[%s]"
          (String.concat "; " (List.map show_cache_op ops))))
    (fun ops ->
       Eio_main.run @@ fun _env ->
       let cache = Kirin.Cache.create ~max_size:10 () in
       let get_count = ref 0 in
       List.iter (fun op ->
         match op with
         | Set (k, v) -> Kirin.Cache.set cache k v
         | Get k ->
           ignore (Kirin.Cache.get cache k);
           incr get_count
         | Remove k -> ignore (Kirin.Cache.remove cache k)
       ) ops;
       let s = Kirin.Cache.stats cache in
       s.Kirin.Cache.hits + s.Kirin.Cache.misses = !get_count)

(** P9: mem is consistent with get for non-expired entries. *)
let test_mem_consistent_with_get =
  QCheck.Test.make ~count:300 ~name:"cache: mem consistent with get"
    QCheck.(make
      Gen.(let* k = key_gen in
           let* v = value_gen in
           let* ops = list_size (int_range 0 20) cache_op_gen in
           return (k, v, ops))
      ~print:(fun (k, v, ops) ->
        Printf.sprintf "key=%s, value=%s, ops=[%s]"
          k v (String.concat "; " (List.map show_cache_op ops))))
    (fun (k, v, ops) ->
       Eio_main.run @@ fun _env ->
       let cache = Kirin.Cache.create ~max_size:50 () in
       Kirin.Cache.set cache k v;
       List.iter (fun op ->
         match op with
         | Set (k, v) -> Kirin.Cache.set cache k v
         | Get k -> ignore (Kirin.Cache.get cache k)
         | Remove k -> ignore (Kirin.Cache.remove cache k)
       ) ops;
       (* mem should return true iff get returns Some *)
       let got = Kirin.Cache.get cache k in
       let has = Kirin.Cache.mem cache k in
       (Option.is_some got) = has)

(* -- Test Runner -------------------------------------------------- *)

let () =
  let suite =
    List.map QCheck_alcotest.to_alcotest [
      test_capacity_invariant;
      test_roundtrip_no_eviction;
      test_overwrite_latest;
      test_clear_empties;
      test_remove_then_gone;
      test_lru_eviction_order;
      test_expired_not_returned;
      test_stats_consistency;
      test_mem_consistent_with_get;
    ]
  in
  Alcotest.run "Cache QCheck" [ ("properties", suite) ]
