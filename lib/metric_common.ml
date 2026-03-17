(** Shared types and utilities for metric sub-modules. *)

type label = string * string

let with_lock mutex f =
  Eio.Mutex.use_rw ~protect:true mutex (fun () -> f ())
