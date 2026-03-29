(** Histogram metric - distribution of values in buckets. *)

type label = Metric_common.label

(** Bucket data for a single bucket boundary. *)
type bucket_data =
  { mutable count : int
  ; upper_bound : float
  }

(** Aggregated data for one label combination. *)
type data =
  { mutable sum : float
  ; mutable count : int
  ; buckets : bucket_data array
  }

(** Abstract histogram type. *)
type t

(** Default bucket boundaries for histograms. *)
val default_buckets : float array

val create
  :  name:string
  -> help:string
  -> ?labels:string list
  -> ?buckets:float array
  -> unit
  -> t

(** [observe ?labels t value] records a value in the histogram. *)
val observe : ?labels:label list -> t -> float -> unit

(** [time ?labels t f] measures execution time of [f] and records it.
    Returns the result of [f]. *)
val time : ?labels:label list -> t -> (unit -> 'a) -> 'a

(** {1 Internal accessors (for Prometheus export)} *)

val name : t -> string
val help : t -> string
val label_names : t -> string list
val iter_values : (label list -> data -> unit) -> t -> unit
