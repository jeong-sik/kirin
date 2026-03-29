(** Kirin Background Jobs -- Eio-native

    Job queue using Eio fibers for concurrent task processing.
    Workers run as lightweight fibers within a shared Eio switch.

    @since 1.0.0
    @status stable

    {b Features:}
    - Fiber-based worker pool (no OS threads)
    - Eio.Mutex for fiber-safe state access
    - Priority queues
    - Retry with exponential backoff *)

(** {1 Types} *)

(** Job priority. *)
type priority =
  | Critical
  | High
  | Normal
  | Low

(** Job status. *)
type 'a status =
  | Pending
  | Running
  | Completed of 'a
  | Failed of exn

(** Job identifier. *)
type job_id = string

(** Job metadata. *)
type 'a job =
  { id : job_id
  ; task : unit -> 'a
  ; priority : priority
  ; mutable status : 'a status
  ; mutable retries : int
  ; max_retries : int
  ; created_at : float
  ; mutable started_at : float option
  ; mutable completed_at : float option
  }

(** Job configuration. *)
type config =
  { workers : int
  ; max_queue_size : int
  ; default_max_retries : int
  ; retry_delay : float
  }

(** Queue statistics. *)
type stats =
  { total_submitted : int
  ; total_completed : int
  ; total_failed : int
  ; currently_running : int
  ; queue_size : int
  }

(** Job queue -- carries Eio resources for fiber-safe operation. *)
type 'a t =
  { config : config
  ; clock : float Eio.Time.clock_ty Eio.Resource.t
  ; mutable jobs : 'a job list
  ; mutable results : (job_id, 'a status) Hashtbl.t
  ; mutable running : bool
  ; mutable stats : stats
  ; mutable next_id : int
  ; mutex : Eio.Mutex.t
  ; job_available : Eio.Condition.t
  ; job_completed : Eio.Condition.t
  }

(** {1 Configuration} *)

(** Default configuration (4 workers, 10000 queue, 3 retries, 1.0s delay). *)
val default_config : config

(** {1 Queue Creation} *)

(** [create ~sw ~clock ?workers ?max_queue_size ?default_max_retries ?retry_delay ()]
    creates and starts a job queue.

    Workers are forked as Eio fibers under [sw].
    The queue stops when the switch is cancelled or {!stop} is called.

    @param sw Eio switch that owns worker fibers
    @param clock Eio clock for timestamps and sleep
    @param workers Number of worker fibers (default: 4)
    @param max_queue_size Maximum pending jobs (default: 10000)
    @param default_max_retries Default retry count (default: 3)
    @param retry_delay Base delay between retries in seconds (default: 1.0) *)
val create
  :  sw:Eio.Switch.t
  -> clock:float Eio.Time.clock_ty Eio.Resource.t
  -> ?workers:int
  -> ?max_queue_size:int
  -> ?default_max_retries:int
  -> ?retry_delay:float
  -> unit
  -> 'a t

(** {1 Job Submission} *)

(** [submit ?priority ?max_retries t task] submits a job to the queue.
    @return Job ID for tracking
    @raise Failure if the queue is full *)
val submit : ?priority:priority -> ?max_retries:int -> 'a t -> (unit -> 'a) -> job_id

(** [submit_all ?priority ?max_retries t tasks] submits multiple jobs. *)
val submit_all
  :  ?priority:priority
  -> ?max_retries:int
  -> 'a t
  -> (unit -> 'a) list
  -> job_id list

(** {1 Job Status} *)

(** [status t job_id] returns the current status of a job.
    @raise Failure if job_id is unknown *)
val status : 'a t -> job_id -> 'a status

(** [is_complete t job_id] returns [true] if the job has completed or failed. *)
val is_complete : 'a t -> job_id -> bool

(** [wait t job_id] waits for a job to complete.
    Suspends the current fiber without blocking OS threads.
    @raise Failure if job_id is unknown *)
val wait : 'a t -> job_id -> 'a status

(** {1 Queue Control} *)

(** [stop t] stops the queue and wakes all waiting workers/callers. *)
val stop : 'a t -> unit

(** [is_running t] returns [true] if the queue is still running. *)
val is_running : 'a t -> bool

(** {1 Statistics} *)

(** [stats t] returns current queue statistics. *)
val stats : 'a t -> stats

(** [pending_count t] returns the number of pending jobs. *)
val pending_count : 'a t -> int

(** [running_count t] returns the number of currently running jobs. *)
val running_count : 'a t -> int

(** {1 Utilities} *)

(** [cancel t job_id] cancels a pending job.
    Returns [true] if the job was pending and removed, [false] if already running/done. *)
val cancel : 'a t -> job_id -> bool

(** [clear t] removes all pending jobs. Returns the number of jobs removed. *)
val clear : 'a t -> int

(** [run_sync task] runs a task synchronously (no queue involved). *)
val run_sync : (unit -> 'a) -> 'a

(** {1 Convenience} *)

(** [submit_and_wait ?priority ?max_retries t task] submits a job and waits for completion. *)
val submit_and_wait
  :  ?priority:priority
  -> ?max_retries:int
  -> 'a t
  -> (unit -> 'a)
  -> 'a status

(** [run_once ~clock task] creates a one-shot queue, runs a single job,
    and returns the result. *)
val run_once : clock:float Eio.Time.clock_ty Eio.Resource.t -> (unit -> 'a) -> 'a status
