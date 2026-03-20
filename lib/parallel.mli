(** Kirin Parallel Processing

    OCaml 5 Domain-based parallel computation for CPU-bound tasks.
    True parallelism without GIL limitations.

    {b Features:}
    - Parallel map/iter over collections
    - Configurable domain pool
    - Fork-join parallelism
    - Parallel reduce operations *)

(** {1 Types} *)

(** Parallel computation result. *)
type 'a result =
  | Ok of 'a
  | Error of exn

(** {1 Basic Parallel Operations} *)

(** [recommended_domains ()] returns recommended domain count based on available CPUs. *)
val recommended_domains : unit -> int

(** [map ?domains f items] maps [f] over [items] in parallel using multiple domains.
    @param domains Number of domains to use (default: CPU count - 1) *)
val map : ?domains:int -> ('a -> 'b) -> 'a list -> 'b list

(** [iter ?domains f items] executes [f] on each element in parallel. *)
val iter : ?domains:int -> ('a -> unit) -> 'a list -> unit

(** [filter ?domains pred items] filters elements in parallel, preserving order. *)
val filter : ?domains:int -> ('a -> bool) -> 'a list -> 'a list

(** [reduce ?domains combine init items] parallel fold over items.
    @param combine Associative combining function
    @param init Initial value *)
val reduce : ?domains:int -> ('a -> 'a -> 'a) -> 'a -> 'a list -> 'a

(** [iteri ?domains f items] parallel for-each with index. *)
val iteri : ?domains:int -> (int -> 'a -> unit) -> 'a list -> unit

(** [mapi ?domains f items] parallel map with index. *)
val mapi : ?domains:int -> (int -> 'a -> 'b) -> 'a list -> 'b list

(** {1 Fork-Join Parallelism} *)

(** [both f g] runs two computations in parallel and returns both results. *)
val both : (unit -> 'a) -> (unit -> 'b) -> 'a * 'b

(** [triple f g h] runs three computations in parallel. *)
val triple : (unit -> 'a) -> (unit -> 'b) -> (unit -> 'c) -> 'a * 'b * 'c

(** [all tasks] runs a list of computations in parallel. *)
val all : (unit -> 'a) list -> 'a list

(** [race tasks] runs computations and returns the first result. *)
val race : (unit -> 'a) list -> 'a

(** {1 Domain Pool} *)

module Pool : sig
  (** Domain pool for reusable parallel workers. *)

  (** Task type alias. *)
  type 'a task = unit -> 'a

  (** Pool handle. *)
  type t

  (** [create ?size ()] creates a domain pool. *)
  val create : ?size:int -> unit -> t

  (** [map pool f items] maps using the domain pool. *)
  val map : t -> ('a -> 'b) -> 'a list -> 'b list

  (** [iter pool f items] iterates using the domain pool. *)
  val iter : t -> ('a -> unit) -> 'a list -> unit

  (** [reduce pool combine init items] reduces using the domain pool. *)
  val reduce : t -> ('a -> 'a -> 'a) -> 'a -> 'a list -> 'a

  (** [size pool] returns the pool size. *)
  val size : t -> int

  (** [is_active pool] returns [true] if the pool is active. *)
  val is_active : t -> bool

  (** [shutdown pool] shuts down the pool. *)
  val shutdown : t -> unit
end

(** {1 Utilities} *)

(** [timed f] times a computation. Returns [(result, elapsed_seconds)]. *)
val timed : (unit -> 'a) -> 'a * float

(** [with_domains n f] runs [f] with specified domain count. *)
val with_domains : int -> ('a -> 'b) -> 'a list -> 'b list

(** [chunk_list n items] splits a list into sublists of size [n]. *)
val chunk_list : int -> 'a list -> 'a list list

(** [map_chunked ?domains ?chunk_size f items] parallel map over chunks.
    Useful for very fine-grained work items. *)
val map_chunked : ?domains:int -> ?chunk_size:int -> ('a -> 'b) -> 'a list -> 'b list
