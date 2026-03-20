(** Kirin Cluster - Multi-process Scale Up (PM2 style)

    Enables scaling across multiple CPU cores using operating system processes.
    Uses SO_REUSEPORT to allow multiple processes to bind to the same port.

    {b Benefits:}
    - Resilience: If one worker crashes, others keep running.
    - Isolation: Independent GC heaps per worker.
    - Performance: True parallelism without GC pauses affecting peers.

    @since 1.0.0
    @status needs-work
    Functional but lacks configuration options and signal handling refinements. *)

(** {1 Types} *)

(** Worker process info. *)
type worker = {
  pid : int;
  id : int;
}

(** Cluster state. *)
type t = {
  workers : (int, worker) Hashtbl.t;
  master_pid : int;
}

(** {1 Internal} *)

(** [create_socket port] creates a socket with SO_REUSEADDR. *)
val create_socket : int -> Unix.file_descr

(** [spawn_worker id entry_point] forks a worker process. *)
val spawn_worker : int -> (unit -> unit) -> worker

(** {1 Cluster Operations} *)

(** [start ?workers entry_point] starts the cluster.

    Forks [workers] child processes, each running [entry_point].
    The master process supervises and respawns crashed workers.

    This function does not return (runs a supervisor loop).

    @param workers Number of worker processes (default: CPU count) *)
val start : ?workers:int -> (unit -> unit) -> unit
