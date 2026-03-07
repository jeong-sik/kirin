(** Histogram metric - distribution of values in buckets *)

type label = string * string

type bucket_data = {
  mutable count : int;
  upper_bound : float;
}

type data = {
  mutable sum : float;
  mutable count : int;
  buckets : bucket_data array;
}

type t = {
  name : string;
  help : string;
  label_names : string list;
  bucket_bounds : float array;
  values : (label list, data) Hashtbl.t;
  mutex : Eio.Mutex.t;
}

let default_buckets = [| 0.005; 0.01; 0.025; 0.05; 0.1; 0.25; 0.5; 1.0; 2.5; 5.0; 10.0 |]

let create ~name ~help ?(labels = []) ?(buckets = default_buckets) () = {
  name;
  help;
  label_names = labels;
  bucket_bounds = buckets;
  values = Hashtbl.create 64;
  mutex = Eio.Mutex.create ();
}

let with_lock t f =
  Eio.Mutex.use_rw ~protect:true t.mutex f

let make_data buckets =
  {
    sum = 0.0;
    count = 0;
    buckets = Array.map (fun b -> { count = 0; upper_bound = b }) buckets;
  }

let observe ?(labels = []) t value =
  with_lock t (fun () ->
    let data = match Hashtbl.find_opt t.values labels with
      | Some d -> d
      | None ->
        let d = make_data t.bucket_bounds in
        Hashtbl.replace t.values labels d;
        d
    in
    data.sum <- data.sum +. value;
    data.count <- data.count + 1;
    Array.iter (fun bucket ->
      if value <= bucket.upper_bound then
        bucket.count <- bucket.count + 1
    ) data.buckets
  )

let time ?(labels = []) t f =
  let start = Time_compat.now () in
  let result = f () in
  let elapsed = Time_compat.now () -. start in
  observe ~labels t elapsed;
  result
