(** Cache tests (Phase 9) *)

open Alcotest
open Test_helpers
module C = Kirin.Cache

let test_cache_create () =
  let cache = C.create ~max_size:100 () in
  check int "initial size" 0 (C.size cache);
  let stats = C.stats cache in
  check int "max size" 100 stats.max_size
;;

let test_cache_set_get () =
  let cache = C.create ~max_size:10 () in
  C.set cache "key1" "value1";
  C.set cache "key2" "value2";
  check (option string) "get key1" (Some "value1") (C.get cache "key1");
  check (option string) "get key2" (Some "value2") (C.get cache "key2");
  check (option string) "get missing" None (C.get cache "key3")
;;

let test_cache_remove () =
  let cache = C.create ~max_size:10 () in
  C.set cache "key" "value";
  check bool "mem before" true (C.mem cache "key");
  let removed = C.remove cache "key" in
  check bool "removed" true removed;
  check bool "mem after" false (C.mem cache "key");
  let removed_again = C.remove cache "key" in
  check bool "remove again" false removed_again
;;

let test_cache_lru_eviction () =
  let cache = C.create ~max_size:3 () in
  C.set cache "a" "1";
  C.set cache "b" "2";
  C.set cache "c" "3";
  (* Access "a" to make it most recent *)
  ignore (C.get cache "a");
  (* Add "d", should evict "b" (least recently used) *)
  C.set cache "d" "4";
  check int "size capped" 3 (C.size cache);
  check (option string) "a still exists" (Some "1") (C.get cache "a");
  check (option string) "b evicted" None (C.get cache "b");
  check (option string) "c exists" (Some "3") (C.get cache "c");
  check (option string) "d exists" (Some "4") (C.get cache "d")
;;

let test_cache_ttl () =
  let cache = C.create ~max_size:10 ~default_ttl:0.05 () in
  C.set cache "key" "value";
  check (option string) "get before expire" (Some "value") (C.get cache "key");
  Kirin.Time_compat.sleep 0.06;
  check (option string) "get after expire" None (C.get cache "key")
;;

let test_cache_stats () =
  let cache = C.create ~max_size:10 () in
  C.set cache "key" "value";
  ignore (C.get cache "key");
  (* hit *)
  ignore (C.get cache "missing");
  (* miss *)
  let stats = C.stats cache in
  check int "hits" 1 stats.hits;
  check int "misses" 1 stats.misses
;;

let test_cache_hit_rate () =
  let cache = C.create ~max_size:10 () in
  C.set cache "key" "value";
  ignore (C.get cache "key");
  ignore (C.get cache "key");
  ignore (C.get cache "missing");
  let rate = C.hit_rate cache in
  (* 2 hits, 1 miss = 2/3 = 0.666... *)
  check bool "hit rate ~0.67" true (rate > 0.6 && rate < 0.7)
;;

let test_cache_get_or_set () =
  let cache = C.create ~max_size:10 () in
  let count = ref 0 in
  let compute () =
    incr count;
    "computed"
  in
  let v1 = C.get_or_set cache "key" compute in
  check string "first call" "computed" v1;
  check int "computed once" 1 !count;
  let v2 = C.get_or_set cache "key" compute in
  check string "second call" "computed" v2;
  check int "still computed once" 1 !count
;;

let test_cache_clear () =
  let cache = C.create ~max_size:10 () in
  C.set cache "a" "1";
  C.set cache "b" "2";
  check int "size before" 2 (C.size cache);
  C.clear cache;
  check int "size after" 0 (C.size cache)
;;

let test_cache_cleanup () =
  let cache = C.create ~max_size:10 ~default_ttl:0.03 () in
  C.set cache "a" "1";
  C.set cache "b" "2";
  Kirin.Time_compat.sleep 0.05;
  let expired = C.cleanup cache in
  check int "expired count" 2 expired;
  check int "size after cleanup" 0 (C.size cache)
;;

let test_cache_make_key () =
  let key = C.make_key [ "user"; "123"; "profile" ] in
  check string "make_key" "user:123:profile" key
;;

let tests =
  [ test_case "cache create" `Quick (with_eio test_cache_create)
  ; test_case "cache set get" `Quick (with_eio test_cache_set_get)
  ; test_case "cache remove" `Quick (with_eio test_cache_remove)
  ; test_case "cache lru eviction" `Quick (with_eio test_cache_lru_eviction)
  ; test_case "cache ttl" `Quick (with_eio test_cache_ttl)
  ; test_case "cache stats" `Quick (with_eio test_cache_stats)
  ; test_case "cache hit rate" `Quick (with_eio test_cache_hit_rate)
  ; test_case "cache get_or_set" `Quick (with_eio test_cache_get_or_set)
  ; test_case "cache clear" `Quick (with_eio test_cache_clear)
  ; test_case "cache cleanup" `Quick (with_eio test_cache_cleanup)
  ; test_case "cache make_key" `Quick test_cache_make_key (* no Eio needed *)
  ]
;;
