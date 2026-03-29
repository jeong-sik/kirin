(** Gauge metric - value that can go up or down *)

type label = Metric_common.label

type t =
  { name : string
  ; help : string
  ; label_names : string list
  ; values : (label list, float) Hashtbl.t
  ; mutex : Eio.Mutex.t
  }

let create ~name ~help ?(labels = []) () =
  { name
  ; help
  ; label_names = labels
  ; values = Hashtbl.create 64
  ; mutex = Eio.Mutex.create ()
  }
;;

let set ?(labels = []) t value =
  Metric_common.with_lock t.mutex (fun () -> Hashtbl.replace t.values labels value)
;;

let inc ?(by = 1.0) ?(labels = []) t =
  Metric_common.with_lock t.mutex (fun () ->
    let current = Hashtbl.find_opt t.values labels |> Option.value ~default:0.0 in
    Hashtbl.replace t.values labels (current +. by))
;;

let dec ?(by = 1.0) ?(labels = []) t =
  Metric_common.with_lock t.mutex (fun () ->
    let current = Hashtbl.find_opt t.values labels |> Option.value ~default:0.0 in
    Hashtbl.replace t.values labels (current -. by))
;;

let get ?(labels = []) t =
  Metric_common.with_lock t.mutex (fun () ->
    Hashtbl.find_opt t.values labels |> Option.value ~default:0.0)
;;

let name t = t.name
let help t = t.help
let label_names t = t.label_names

let iter_values f t =
  Metric_common.with_lock t.mutex (fun () ->
    Hashtbl.iter (fun labels value -> f labels value) t.values)
;;
