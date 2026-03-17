(** Counter metric - monotonically increasing value. *)

type label = Metric_common.label

(** Abstract counter type. *)
type t

val create : name:string -> help:string -> ?labels:string list -> unit -> t

(** [inc ?by ?labels t] increments the counter by [by] (default 1.0)
    for the given label combination. *)
val inc : ?by:float -> ?labels:label list -> t -> unit

(** [get ?labels t] returns the current value for the given labels. *)
val get : ?labels:label list -> t -> float

(** [reset t] clears all recorded values. *)
val reset : t -> unit

(** {1 Internal accessors (for Prometheus export)} *)

val name : t -> string
val help : t -> string
val label_names : t -> string list
val iter_values : (label list -> float -> unit) -> t -> unit
