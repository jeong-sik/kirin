(** Summary metric - quantile-based summary *)

type t =
  { name : string
  ; help : string
  ; label_names : string list
  ; samples : float array (* circular buffer *)
  ; mutable write_pos : int (* next write index *)
  ; mutable length : int (* current number of samples, <= max_samples *)
  ; mutable sum : float
  ; mutable count : int
  ; max_samples : int
  ; mutable sorted_cache : float array option (* invalidated on mutation *)
  ; mutex : Eio.Mutex.t
  }

let create ~name ~help ?(labels = []) ?(max_samples = 1000) () =
  { name
  ; help
  ; label_names = labels
  ; samples = Array.make max_samples 0.0
  ; write_pos = 0
  ; length = 0
  ; sum = 0.0
  ; count = 0
  ; max_samples
  ; sorted_cache = None
  ; mutex = Eio.Mutex.create ()
  }
;;

let observe t value =
  Metric_common.with_lock t.mutex (fun () ->
    t.sum <- t.sum +. value;
    t.count <- t.count + 1;
    t.samples.(t.write_pos) <- value;
    t.write_pos <- (t.write_pos + 1) mod t.max_samples;
    if t.length < t.max_samples then t.length <- t.length + 1;
    t.sorted_cache <- None)
;;

let get_sorted t =
  match t.sorted_cache with
  | Some s -> s
  | None ->
    let arr = Array.sub t.samples 0 t.length in
    Array.sort compare arr;
    t.sorted_cache <- Some arr;
    arr
;;

let quantile t q =
  Metric_common.with_lock t.mutex (fun () ->
    if t.length = 0
    then 0.0
    else (
      let sorted = get_sorted t in
      let n = Array.length sorted in
      let idx = min (n - 1) (int_of_float (float_of_int n *. q)) in
      sorted.(idx)))
;;

let name t = t.name
let help t = t.help
let label_names t = t.label_names
let sum t = Metric_common.with_lock t.mutex (fun () -> t.sum)
let count t = Metric_common.with_lock t.mutex (fun () -> t.count)
