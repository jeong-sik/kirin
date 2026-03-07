(** Summary metric - quantile-based summary *)

type t = {
  name : string;
  help : string;
  label_names : string list;
  mutable values : float list;
  mutable sum : float;
  mutable count : int;
  max_samples : int;
  mutex : Eio.Mutex.t;
}

let create ~name ~help ?(labels = []) ?(max_samples = 1000) () = {
  name;
  help;
  label_names = labels;
  values = [];
  sum = 0.0;
  count = 0;
  max_samples;
  mutex = Eio.Mutex.create ();
}

let with_lock t f =
  Eio.Mutex.use_rw ~protect:true t.mutex f

let observe t value =
  with_lock t (fun () ->
    t.sum <- t.sum +. value;
    t.count <- t.count + 1;
    t.values <- value :: (if List.length t.values >= t.max_samples
                         then List.tl t.values else t.values)
  )

let quantile t q =
  with_lock t (fun () ->
    if t.values = [] then 0.0
    else
      let sorted = List.sort compare t.values in
      let n = List.length sorted in
      let idx = min (n - 1) (int_of_float (float_of_int n *. q)) in
      List.nth sorted idx
  )
