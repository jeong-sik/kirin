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

(** Minimum spacing between two spawns of the same worker id (default
    1.0s).  Used as the [min_restart_interval] default for [start]. *)
val default_min_restart_interval : float

(** Hard cap on consecutive fast crashes for a single worker id
    before the supervisor gives up (default 10).  Used as the
    [max_consecutive_crashes] default for [start]. *)
val default_max_consecutive_crashes : int

(** [compute_backoff_delay ~last_spawn ~now ~min_restart_interval]
    returns how long the supervisor should sleep before respawning.
    Returns [<= 0.0] when the worker has been alive at least
    [min_restart_interval] (spawn immediately). *)
val compute_backoff_delay :
  last_spawn:float -> now:float -> min_restart_interval:float -> float

(** [should_give_up ~recent_crashes ~max_consecutive_crashes] returns
    [true] once a slot has crashed faster than [min_restart_interval]
    that many times in a row.  The supervisor then exits the master
    rather than continue fork-bombing the host. *)
val should_give_up :
  recent_crashes:int -> max_consecutive_crashes:int -> bool

(** [next_recent_crashes ~last_spawn ~now ~min_restart_interval
    ~recent_crashes] returns the updated fast-crash counter after a
    crash.  A crash that took longer than [min_restart_interval]
    resets the counter to [1]; a faster crash increments. *)
val next_recent_crashes :
  last_spawn:float ->
  now:float ->
  min_restart_interval:float ->
  recent_crashes:int ->
  int

(** [start ?workers ?min_restart_interval ?max_consecutive_crashes
    entry_point] starts the cluster.

    Forks [workers] child processes, each running [entry_point].
    The master process supervises and respawns crashed workers,
    inserting a sleep when the same slot crashes within
    [min_restart_interval] seconds and giving up after
    [max_consecutive_crashes] consecutive fast crashes — without
    these guards a worker that reliably raises on startup (port
    already bound, config invalid) would let the supervisor
    fork-bomb the host.

    Returns when the cluster gives up; the caller's deployment
    supervisor (systemd, k8s) decides what to do next.

    @param workers Number of worker processes (default: CPU count)
    @param min_restart_interval Minimum gap before respawn (default 1.0s)
    @param max_consecutive_crashes Cap on fast-crash loop (default 10) *)
val start :
  ?workers:int ->
  ?min_restart_interval:float ->
  ?max_consecutive_crashes:int ->
  (unit -> unit) ->
  unit
