(** Summary metric - quantile-based summary. *)

(** Abstract summary type. *)
type t

val create
  :  name:string
  -> help:string
  -> ?labels:string list
  -> ?max_samples:int
  -> unit
  -> t

(** [observe t value] records a value. *)
val observe : t -> float -> unit

(** [quantile t q] returns the value at quantile [q] (0.0 to 1.0). *)
val quantile : t -> float -> float

(** {1 Internal accessors (for Prometheus export)} *)

val name : t -> string
val help : t -> string
val label_names : t -> string list
val sum : t -> float
val count : t -> int
