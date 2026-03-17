(** Shared types and utilities for metric sub-modules. *)

(** A label is a key-value pair for metric dimensions. *)
type label = string * string

(** [with_lock mutex f] runs [f ()] while holding [mutex].
    Uses [Eio.Mutex.use_rw ~protect:true]. *)
val with_lock : Eio.Mutex.t -> (unit -> 'a) -> 'a
