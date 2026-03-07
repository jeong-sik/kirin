(** Gauge metric - value that can go up or down *)

type label = string * string

type t = {
  name : string;
  help : string;
  label_names : string list;
  values : (label list, float) Hashtbl.t;
  mutex : Eio.Mutex.t;
}

let create ~name ~help ?(labels = []) () = {
  name;
  help;
  label_names = labels;
  values = Hashtbl.create 64;
  mutex = Eio.Mutex.create ();
}

let with_lock t f =
  Eio.Mutex.use_rw ~protect:true t.mutex f

let set ?(labels = []) t value =
  with_lock t (fun () -> Hashtbl.replace t.values labels value)

let inc ?(by = 1.0) ?(labels = []) t =
  with_lock t (fun () ->
    let current = Hashtbl.find_opt t.values labels |> Option.value ~default:0.0 in
    Hashtbl.replace t.values labels (current +. by)
  )

let dec ?(by = 1.0) ?(labels = []) t =
  with_lock t (fun () ->
    let current = Hashtbl.find_opt t.values labels |> Option.value ~default:0.0 in
    Hashtbl.replace t.values labels (current -. by)
  )

let get ?(labels = []) t =
  with_lock t (fun () ->
    Hashtbl.find_opt t.values labels |> Option.value ~default:0.0
  )
