(** Gauge metric - value that can go up or down. *)

type label = Metric_common.label

(** Abstract gauge type. *)
type t

val create : name:string -> help:string -> ?labels:string list -> unit -> t

(** [set ?labels t value] sets the gauge to [value]. *)
val set : ?labels:label list -> t -> float -> unit

(** [inc ?by ?labels t] increments the gauge by [by] (default 1.0). *)
val inc : ?by:float -> ?labels:label list -> t -> unit

(** [dec ?by ?labels t] decrements the gauge by [by] (default 1.0). *)
val dec : ?by:float -> ?labels:label list -> t -> unit

(** [get ?labels t] returns the current value. *)
val get : ?labels:label list -> t -> float

(** {1 Internal accessors (for Prometheus export)} *)

val name : t -> string
val help : t -> string
val label_names : t -> string list
val iter_values : (label list -> float -> unit) -> t -> unit
