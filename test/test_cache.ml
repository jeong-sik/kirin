(** Cache module tests.
    Uses Eio_main.run because Cache relies on Eio.Mutex. *)

open Alcotest

(* -- creation and basic ops ---------------------------------------- *)

let test_create_default () =
  Eio_main.run @@ fun _env ->
  let cache = Kirin.Cache.create () in
  check int "size 0" 0 (Kirin.Cache.size cache);
  let s = Kirin.Cache.stats cache in
  check int "max_size" 1000 s.Kirin.Cache.max_size;
  check int "hits" 0 s.Kirin.Cache.hits;
  check int "misses" 0 s.Kirin.Cache.misses

let test_create_custom () =
  Eio_main.run @@ fun _env ->
  let cache = Kirin.Cache.create ~max_size:5 ~default_ttl:10.0 ~cleanup_interval:30.0 () in
  let s = Kirin.Cache.stats cache in
  check int "custom max_size" 5 s.Kirin.Cache.max_size

let test_set_and_get () =
  Eio_main.run @@ fun _env ->
  let cache = Kirin.Cache.create ~max_size:10 () in
  Kirin.Cache.set cache "key1" "value1";
  let result = Kirin.Cache.get cache "key1" in
  check (option string) "found" (Some "value1") result

let test_get_missing () =
  Eio_main.run @@ fun _env ->
  let cache = Kirin.Cache.create ~max_size:10 () in
  let result = Kirin.Cache.get cache "missing" in
  check (option string) "not found" None result

let test_set_overwrites () =
  Eio_main.run @@ fun _env ->
  let cache = Kirin.Cache.create ~max_size:10 () in
  Kirin.Cache.set cache "k" "v1";
  Kirin.Cache.set cache "k" "v2";
  check (option string) "overwritten" (Some "v2") (Kirin.Cache.get cache "k");
  check int "size stays 1" 1 (Kirin.Cache.size cache)

let test_remove () =
  Eio_main.run @@ fun _env ->
  let cache = Kirin.Cache.create ~max_size:10 () in
  Kirin.Cache.set cache "k" "v";
  let removed = Kirin.Cache.remove cache "k" in
  check bool "removed true" true removed;
  check (option string) "gone" None (Kirin.Cache.get cache "k");
  let not_found = Kirin.Cache.remove cache "k" in
  check bool "remove nonexistent" false not_found

let test_mem () =
  Eio_main.run @@ fun _env ->
  let cache = Kirin.Cache.create ~max_size:10 () in
  Kirin.Cache.set cache "k" "v";
  check bool "mem existing" true (Kirin.Cache.mem cache "k");
  check bool "mem missing" false (Kirin.Cache.mem cache "other")

let basic_tests = [
  test_case "create default" `Quick test_create_default;
  test_case "create custom" `Quick test_create_custom;
  test_case "set and get" `Quick test_set_and_get;
  test_case "get missing" `Quick test_get_missing;
  test_case "set overwrites" `Quick test_set_overwrites;
  test_case "remove" `Quick test_remove;
  test_case "mem" `Quick test_mem;
]

(* -- TTL expiry ---------------------------------------------------- *)

let test_ttl_expiry () =
  Eio_main.run @@ fun env ->
  let cache = Kirin.Cache.create ~max_size:10 () in
  Kirin.Cache.set ~ttl:0.0 cache "short" "val";
  Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
  let result = Kirin.Cache.get cache "short" in
  check (option string) "expired" None result

let test_default_ttl () =
  Eio_main.run @@ fun env ->
  let cache = Kirin.Cache.create ~max_size:10 ~default_ttl:0.0 () in
  Kirin.Cache.set cache "k" "v";
  Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
  let result = Kirin.Cache.get cache "k" in
  check (option string) "default ttl expired" None result

let test_no_ttl_never_expires () =
  Eio_main.run @@ fun _env ->
  let cache = Kirin.Cache.create ~max_size:10 () in
  Kirin.Cache.set cache "forever" "here";
  check (option string) "still here" (Some "here") (Kirin.Cache.get cache "forever")

let test_per_entry_ttl_overrides_default () =
  Eio_main.run @@ fun env ->
  let cache = Kirin.Cache.create ~max_size:10 ~default_ttl:3600.0 () in
  Kirin.Cache.set ~ttl:0.0 cache "short" "val";
  Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
  check (option string) "per-entry ttl expired" None (Kirin.Cache.get cache "short")

let ttl_tests = [
  test_case "ttl expiry" `Quick test_ttl_expiry;
  test_case "default ttl" `Quick test_default_ttl;
  test_case "no ttl never expires" `Quick test_no_ttl_never_expires;
  test_case "per-entry ttl overrides default" `Quick test_per_entry_ttl_overrides_default;
]

(* -- LRU eviction -------------------------------------------------- *)

let test_lru_eviction () =
  Eio_main.run @@ fun _env ->
  let cache = Kirin.Cache.create ~max_size:3 () in
  Kirin.Cache.set cache "a" "1";
  Kirin.Cache.set cache "b" "2";
  Kirin.Cache.set cache "c" "3";
  ignore (Kirin.Cache.get cache "a");
  Kirin.Cache.set cache "d" "4";
  check int "size still 3" 3 (Kirin.Cache.size cache);
  check (option string) "a survived" (Some "1") (Kirin.Cache.get cache "a");
  check (option string) "b evicted" None (Kirin.Cache.get cache "b");
  check (option string) "c survived" (Some "3") (Kirin.Cache.get cache "c");
  check (option string) "d present" (Some "4") (Kirin.Cache.get cache "d")

let test_eviction_stats () =
  Eio_main.run @@ fun _env ->
  let cache = Kirin.Cache.create ~max_size:2 () in
  Kirin.Cache.set cache "a" "1";
  Kirin.Cache.set cache "b" "2";
  Kirin.Cache.set cache "c" "3";
  let s = Kirin.Cache.stats cache in
  check int "evictions" 1 s.Kirin.Cache.evictions

let lru_tests = [
  test_case "LRU eviction" `Quick test_lru_eviction;
  test_case "eviction stats" `Quick test_eviction_stats;
]

(* -- statistics ---------------------------------------------------- *)

let test_hit_miss_stats () =
  Eio_main.run @@ fun _env ->
  let cache = Kirin.Cache.create ~max_size:10 () in
  Kirin.Cache.set cache "k" "v";
  ignore (Kirin.Cache.get cache "k");
  ignore (Kirin.Cache.get cache "k");
  ignore (Kirin.Cache.get cache "nope");
  let s = Kirin.Cache.stats cache in
  check int "hits" 2 s.Kirin.Cache.hits;
  check int "misses" 1 s.Kirin.Cache.misses

let test_hit_rate () =
  Eio_main.run @@ fun _env ->
  let cache = Kirin.Cache.create ~max_size:10 () in
  check (float 0.001) "no ops = 0.0" 0.0 (Kirin.Cache.hit_rate cache);
  Kirin.Cache.set cache "k" "v";
  ignore (Kirin.Cache.get cache "k");
  ignore (Kirin.Cache.get cache "miss");
  let rate = Kirin.Cache.hit_rate cache in
  check (float 0.001) "hit rate 0.5" 0.5 rate

let stats_tests = [
  test_case "hit/miss counters" `Quick test_hit_miss_stats;
  test_case "hit_rate" `Quick test_hit_rate;
]

(* -- clear / cleanup / keys / iteration ---------------------------- *)

let test_clear () =
  Eio_main.run @@ fun _env ->
  let cache = Kirin.Cache.create ~max_size:10 () in
  Kirin.Cache.set cache "a" "1";
  Kirin.Cache.set cache "b" "2";
  Kirin.Cache.clear cache;
  check int "cleared size" 0 (Kirin.Cache.size cache);
  check (option string) "a gone" None (Kirin.Cache.get cache "a")

let test_cleanup () =
  Eio_main.run @@ fun env ->
  let cache = Kirin.Cache.create ~max_size:10 () in
  Kirin.Cache.set ~ttl:0.0 cache "expired1" "v";
  Kirin.Cache.set ~ttl:0.0 cache "expired2" "v";
  Kirin.Cache.set cache "alive" "v";
  Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
  let removed = Kirin.Cache.cleanup cache in
  check int "2 cleaned" 2 removed;
  check int "1 remaining" 1 (Kirin.Cache.size cache)

let test_keys () =
  Eio_main.run @@ fun _env ->
  let cache = Kirin.Cache.create ~max_size:10 () in
  Kirin.Cache.set cache "x" "1";
  Kirin.Cache.set cache "y" "2";
  let ks = Kirin.Cache.keys cache |> List.sort String.compare in
  check (list string) "keys" ["x"; "y"] ks

let test_iter () =
  Eio_main.run @@ fun _env ->
  let cache = Kirin.Cache.create ~max_size:10 () in
  Kirin.Cache.set cache "a" 1;
  Kirin.Cache.set cache "b" 2;
  let sum = ref 0 in
  Kirin.Cache.iter (fun _k v -> sum := !sum + v) cache;
  check int "sum" 3 !sum

let test_fold () =
  Eio_main.run @@ fun _env ->
  let cache = Kirin.Cache.create ~max_size:10 () in
  Kirin.Cache.set cache "a" 10;
  Kirin.Cache.set cache "b" 20;
  let total = Kirin.Cache.fold (fun _k v acc -> acc + v) cache 0 in
  check int "fold sum" 30 total

let bulk_tests = [
  test_case "clear" `Quick test_clear;
  test_case "cleanup expired" `Quick test_cleanup;
  test_case "keys" `Quick test_keys;
  test_case "iter" `Quick test_iter;
  test_case "fold" `Quick test_fold;
]

(* -- get_or_set ---------------------------------------------------- *)

let test_get_or_set_miss () =
  Eio_main.run @@ fun _env ->
  let cache = Kirin.Cache.create ~max_size:10 () in
  let called = ref false in
  let v = Kirin.Cache.get_or_set cache "k" (fun () -> called := true; "computed") in
  check string "computed" "computed" v;
  check bool "compute called" true !called

let test_get_or_set_hit () =
  Eio_main.run @@ fun _env ->
  let cache = Kirin.Cache.create ~max_size:10 () in
  Kirin.Cache.set cache "k" "cached";
  let called = ref false in
  let v = Kirin.Cache.get_or_set cache "k" (fun () -> called := true; "new") in
  check string "cached" "cached" v;
  check bool "compute not called" false !called

let get_or_set_tests = [
  test_case "miss computes" `Quick test_get_or_set_miss;
  test_case "hit skips compute" `Quick test_get_or_set_hit;
]

(* -- memoize / make_key -------------------------------------------- *)

let test_memoize () =
  Eio_main.run @@ fun _env ->
  let cache = Kirin.Cache.create ~max_size:10 () in
  let call_count = ref 0 in
  let expensive x =
    incr call_count;
    x * 2
  in
  let memo = Kirin.Cache.memoize cache ~key_fn:string_of_int expensive in
  let r1 = memo 5 in
  let r2 = memo 5 in
  check int "first call" 10 r1;
  check int "second call cached" 10 r2;
  check int "compute called once" 1 !call_count

let test_make_key () =
  check string "joined" "a:b:c" (Kirin.Cache.make_key ["a"; "b"; "c"]);
  check string "single" "x" (Kirin.Cache.make_key ["x"]);
  check string "empty" "" (Kirin.Cache.make_key [])

let utility_tests = [
  test_case "memoize" `Quick test_memoize;
  test_case "make_key" `Quick test_make_key;
]

(* -- StringCache module -------------------------------------------- *)

let test_string_cache () =
  Eio_main.run @@ fun _env ->
  let cache = Kirin.Cache.StringCache.create ~max_size:5 () in
  Kirin.Cache.StringCache.set cache "hello" 42;
  check (option int) "found" (Some 42) (Kirin.Cache.StringCache.get cache "hello");
  check int "size" 1 (Kirin.Cache.StringCache.size cache);
  check bool "mem" true (Kirin.Cache.StringCache.mem cache "hello");
  ignore (Kirin.Cache.StringCache.remove cache "hello");
  check bool "removed" false (Kirin.Cache.StringCache.mem cache "hello");
  Kirin.Cache.StringCache.set cache "a" 1;
  Kirin.Cache.StringCache.clear cache;
  check int "cleared" 0 (Kirin.Cache.StringCache.size cache)

(* -- IntCache module ----------------------------------------------- *)

let test_int_cache () =
  Eio_main.run @@ fun _env ->
  let cache = Kirin.Cache.IntCache.create ~max_size:5 () in
  Kirin.Cache.IntCache.set cache 1 "one";
  Kirin.Cache.IntCache.set cache 2 "two";
  check (option string) "int key" (Some "one") (Kirin.Cache.IntCache.get cache 1);
  check int "size" 2 (Kirin.Cache.IntCache.size cache)

let specialized_tests = [
  test_case "StringCache" `Quick test_string_cache;
  test_case "IntCache" `Quick test_int_cache;
]

(* -- run ----------------------------------------------------------- *)

let () =
  Alcotest.run "Cache" [
    ("basic", basic_tests);
    ("ttl", ttl_tests);
    ("lru", lru_tests);
    ("statistics", stats_tests);
    ("bulk", bulk_tests);
    ("get_or_set", get_or_set_tests);
    ("utility", utility_tests);
    ("specialized", specialized_tests);
  ]
