(** Kirin Parallel Processing

    OCaml 5 Domain-based parallel computation for CPU-bound tasks.
    True parallelism without GIL limitations.

    {b Features:}
    - Parallel map/iter over collections
    - Configurable domain pool
    - Fork-join parallelism
    - Parallel reduce operations

    {b Example - Parallel Map:}
    {[
      let results = Parallel.map ~domains:4 expensive_computation items
    ]}

    {b Example - Parallel Reduce:}
    {[
      let sum = Parallel.reduce ~domains:4 ( + ) 0 numbers
    ]}

    {b Example - Domain Pool:}
    {[
      let pool = Parallel.Pool.create ~size:8 () in
      let results = Parallel.Pool.map pool process items in
      Parallel.Pool.shutdown pool
    ]}
*)

(** {1 Types} *)

(** Parallel computation result *)
type 'a result =
  | Ok of 'a
  | Error of exn

(** {1 Basic Parallel Operations} *)

(** Get recommended domain count based on available CPUs *)
let recommended_domains () =
  max 1 (Domain.recommended_domain_count () - 1)

(** Parallel map over a list.

    Divides work across multiple domains for true parallelism.

    @param domains Number of domains to use (default: CPU count - 1)
*)
let map ?(domains = recommended_domains ()) f items =
  if domains <= 1 || List.length items <= 1 then
    List.map f items
  else
    let items_array = Array.of_list items in
    let n = Array.length items_array in
    let results = Array.make n None in
    let chunk_size = max 1 ((n + domains - 1) / domains) in

    let process_chunk start_idx =
      let end_idx = min n (start_idx + chunk_size) in
      for i = start_idx to end_idx - 1 do
        results.(i) <- Some (f items_array.(i))
      done
    in

    (* Spawn domains for parallel processing *)
    let spawned = Array.init (min domains ((n + chunk_size - 1) / chunk_size)) (fun i ->
      Domain.spawn (fun () -> process_chunk (i * chunk_size))
    ) in

    (* Wait for all domains to complete *)
    Array.iter Domain.join spawned;

    (* Collect results *)
    Array.to_list (Array.map (fun r ->
      match r with
      | Some v -> v
      | None -> failwith "Parallel map: missing result"
    ) results)

(** Parallel iter over a list.

    Executes function on each element in parallel.
*)
let iter ?(domains = recommended_domains ()) f items =
  if domains <= 1 || List.length items <= 1 then
    List.iter f items
  else
    let items_array = Array.of_list items in
    let n = Array.length items_array in
    let chunk_size = max 1 ((n + domains - 1) / domains) in

    let process_chunk start_idx =
      let end_idx = min n (start_idx + chunk_size) in
      for i = start_idx to end_idx - 1 do
        f items_array.(i)
      done
    in

    let spawned = Array.init (min domains ((n + chunk_size - 1) / chunk_size)) (fun i ->
      Domain.spawn (fun () -> process_chunk (i * chunk_size))
    ) in

    Array.iter Domain.join spawned

(** Parallel filter.

    Filters elements in parallel, preserving order.
*)
let filter ?(domains = recommended_domains ()) pred items =
  let flags = map ~domains pred items in
  List.filter_map (fun (item, keep) ->
    if keep then Some item else None
  ) (List.combine items flags)

(** Parallel reduce (fold).

    @param combine Associative combining function
    @param init Initial value
*)
let reduce ?(domains = recommended_domains ()) combine init items =
  if domains <= 1 || List.length items <= 1 then
    List.fold_left combine init items
  else
    let items_array = Array.of_list items in
    let n = Array.length items_array in
    let chunk_size = max 1 ((n + domains - 1) / domains) in
    let num_chunks = (n + chunk_size - 1) / chunk_size in
    let partial_results = Array.make num_chunks init in

    let process_chunk chunk_idx =
      let start_idx = chunk_idx * chunk_size in
      let end_idx = min n (start_idx + chunk_size) in
      let acc = ref init in
      for i = start_idx to end_idx - 1 do
        acc := combine !acc items_array.(i)
      done;
      partial_results.(chunk_idx) <- !acc
    in

    let spawned = Array.init num_chunks (fun i ->
      Domain.spawn (fun () -> process_chunk i)
    ) in

    Array.iter Domain.join spawned;

    (* Combine partial results *)
    Array.fold_left combine init partial_results

(** Parallel for-each with index.

    Like iter but includes index.
*)
let iteri ?(domains = recommended_domains ()) f items =
  let indexed = List.mapi (fun i x -> (i, x)) items in
  iter ~domains (fun (i, x) -> f i x) indexed

(** Parallel map with index *)
let mapi ?(domains = recommended_domains ()) f items =
  let indexed = List.mapi (fun i x -> (i, x)) items in
  map ~domains (fun (i, x) -> f i x) indexed

(** {1 Fork-Join Parallelism} *)

(** Run two computations in parallel and return both results *)
let both f g =
  let d = Domain.spawn g in
  let result_f = f () in
  let result_g = Domain.join d in
  (result_f, result_g)

(** Run three computations in parallel *)
let triple f g h =
  let d1 = Domain.spawn g in
  let d2 = Domain.spawn h in
  let result_f = f () in
  let result_g = Domain.join d1 in
  let result_h = Domain.join d2 in
  (result_f, result_g, result_h)

(** Run a list of computations in parallel *)
let all tasks =
  match tasks with
  | [] -> []
  | [t] -> [t ()]
  | tasks ->
    let spawned = List.map (fun t -> Domain.spawn t) (List.tl tasks) in
    let first_result = (List.hd tasks) () in
    first_result :: List.map Domain.join spawned

(** Run computations and return first to complete (racing) *)
let race tasks =
  (* Note: This is a simplified implementation.
     A full implementation would need cancellation support. *)
  match all tasks with
  | [] -> failwith "Parallel.race: empty task list"
  | results -> List.hd results

(** {1 Domain Pool} *)

module Pool = struct
  (** Domain pool for reusable parallel workers *)
  type 'a task = unit -> 'a

  type t = {
    size : int;
    mutable active : bool;
  }

  (** Create a domain pool *)
  let create ?(size = recommended_domains ()) () = {
    size;
    active = true;
  }

  (** Map using domain pool *)
  let map pool f items =
    if not pool.active then
      failwith "Domain pool is shut down"
    else
      map ~domains:pool.size f items

  (** Iter using domain pool *)
  let iter pool f items =
    if not pool.active then
      failwith "Domain pool is shut down"
    else
      iter ~domains:pool.size f items

  (** Reduce using domain pool *)
  let reduce pool combine init items =
    if not pool.active then
      failwith "Domain pool is shut down"
    else
      reduce ~domains:pool.size combine init items

  (** Get pool size *)
  let size pool = pool.size

  (** Check if pool is active *)
  let is_active pool = pool.active

  (** Shutdown the pool *)
  let shutdown pool =
    pool.active <- false
end

(** {1 Utilities} *)

(** Time a parallel operation *)
let timed f =
  let start = Unix.gettimeofday () in
  let result = f () in
  let elapsed = Unix.gettimeofday () -. start in
  (result, elapsed)

(** Run with specified domain count *)
let with_domains n f =
  map ~domains:n f

(** Chunk a list into sublists *)
let chunk_list n items =
  let rec aux acc current count = function
    | [] ->
      if current = [] then List.rev acc
      else List.rev (List.rev current :: acc)
    | x :: xs ->
      if count >= n then
        aux (List.rev current :: acc) [x] 1 xs
      else
        aux acc (x :: current) (count + 1) xs
  in
  aux [] [] 0 items

(** Parallel map over chunks (for very fine-grained work) *)
let map_chunked ?(domains = recommended_domains ()) ?(chunk_size = 100) f items =
  let chunks = chunk_list chunk_size items in
  let processed_chunks = map ~domains (List.map f) chunks in
  List.flatten processed_chunks
