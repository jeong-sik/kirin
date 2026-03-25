(** Parallel tests (Phase 9) *)

open Alcotest

module P = Kirin.Parallel

let test_parallel_map () =
  let items = [1; 2; 3; 4; 5] in
  let results = P.map ~domains:2 (fun x -> x * 2) items in
  check (list int) "parallel map" [2; 4; 6; 8; 10] results

let test_parallel_iter () =
  let sum = ref 0 in
  let mutex = Mutex.create () in
  P.iter ~domains:2 (fun x ->
    Mutex.lock mutex;
    sum := !sum + x;
    Mutex.unlock mutex
  ) [1; 2; 3; 4; 5];
  check int "parallel iter sum" 15 !sum

let test_parallel_filter () =
  let items = [1; 2; 3; 4; 5; 6; 7; 8] in
  let evens = P.filter ~domains:2 (fun x -> x mod 2 = 0) items in
  check (list int) "parallel filter" [2; 4; 6; 8] evens

let test_parallel_reduce () =
  let items = [1; 2; 3; 4; 5] in
  let sum = P.reduce ~domains:2 ( + ) 0 items in
  check int "parallel reduce" 15 sum

let test_parallel_both () =
  let (a, b) = P.both
    (fun () -> 1 + 1)
    (fun () -> 2 * 2) in
  check int "both a" 2 a;
  check int "both b" 4 b

let test_parallel_all () =
  let results = P.all [
    (fun () -> 1);
    (fun () -> 2);
    (fun () -> 3);
  ] in
  check (list int) "all results" [1; 2; 3] results

let test_parallel_pool () =
  let pool = P.Pool.create ~size:2 () in
  check bool "pool active" true (P.Pool.is_active pool);
  check int "pool size" 2 (P.Pool.size pool);
  let results = P.Pool.map pool (fun x -> x + 1) [1; 2; 3] in
  check (list int) "pool map" [2; 3; 4] results;
  P.Pool.shutdown pool;
  check bool "pool inactive" false (P.Pool.is_active pool)

let test_parallel_chunk_list () =
  let items = [1; 2; 3; 4; 5; 6; 7] in
  let chunks = P.chunk_list 3 items in
  check int "chunk count" 3 (List.length chunks);
  check (list int) "chunk 1" [1; 2; 3] (List.nth chunks 0);
  check (list int) "chunk 2" [4; 5; 6] (List.nth chunks 1);
  check (list int) "chunk 3" [7] (List.nth chunks 2)

let test_parallel_recommended () =
  let n = P.recommended_domains () in
  check bool "at least 1 domain" true (n >= 1)

let tests = [
  test_case "parallel map" `Quick test_parallel_map;
  test_case "parallel iter" `Quick test_parallel_iter;
  test_case "parallel filter" `Quick test_parallel_filter;
  test_case "parallel reduce" `Quick test_parallel_reduce;
  test_case "parallel both" `Quick test_parallel_both;
  test_case "parallel all" `Quick test_parallel_all;
  test_case "parallel pool" `Quick test_parallel_pool;
  test_case "parallel chunk_list" `Quick test_parallel_chunk_list;
  test_case "parallel recommended" `Quick test_parallel_recommended;
]
