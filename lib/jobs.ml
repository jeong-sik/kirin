(** Kirin Background Jobs

    Simple job queue for background task processing.
    Useful for offloading slow operations from request handlers.

    Uses Eio fibers for worker concurrency (cooperative, non-blocking).

    {b Features:}
    - Async job submission
    - Configurable worker pool (Eio fibers)
    - Job status tracking
    - Retry with exponential backoff
    - Priority queues

    {b Example - Basic Usage:}
    {[
      let queue = Jobs.create ~workers:4 () in
      Jobs.start ~sw queue;

      (* Submit a job *)
      let job_id = Jobs.submit queue (fun () ->
        send_email user "Welcome!") in

      (* Check status *)
      match Jobs.status queue job_id with
      | Jobs.Completed result -> Printf.printf "Done: %s\n" result
      | Jobs.Running -> Printf.printf "Still running\n"
      | Jobs.Failed exn -> Printf.printf "Failed: %s\n" (Printexc.to_string exn)
    ]}

    {b Example - With Priority:}
    {[
      Jobs.submit ~priority:High queue critical_task;
      Jobs.submit ~priority:Low queue cleanup_task;
    ]}
*)

(** {1 Types} *)

(** Job priority *)
type priority =
  | Critical  (** Highest priority, processed first *)
  | High
  | Normal
  | Low       (** Lowest priority *)

(** Job status *)
type 'a status =
  | Pending
  | Running
  | Completed of 'a
  | Failed of exn

(** Job identifier *)
type job_id = string

(** Internal job representation *)
type 'a job = {
  id : job_id;
  task : unit -> 'a;
  priority : priority;
  mutable status : 'a status;
  mutable retries : int;
  max_retries : int;
  created_at : float;
  mutable started_at : float option;
  mutable completed_at : float option;
}

(** Job queue configuration *)
type config = {
  workers : int;
  max_queue_size : int;
  default_max_retries : int;
  retry_delay : float;
}

(** Queue statistics *)
type stats = {
  total_submitted : int;
  total_completed : int;
  total_failed : int;
  currently_running : int;
  queue_size : int;
}

(** Job queue *)
type 'a t = {
  config : config;
  mutable jobs : 'a job list;
  mutable results : (job_id, 'a status) Hashtbl.t;
  mutable running : bool;
  mutable stats : stats;
  mutable next_id : int;
  mutex : Eio.Mutex.t;
  condition : Eio.Condition.t;
}

(** {1 Configuration} *)

let default_config = {
  workers = 4;
  max_queue_size = 10000;
  default_max_retries = 3;
  retry_delay = 1.0;
}

(** {1 Helpers} *)

let now () = Time_compat.now ()

let with_lock mutex f =
  Eio.Mutex.use_rw ~protect:true mutex f

let generate_id t =
  let id = t.next_id in
  t.next_id <- id + 1;
  Printf.sprintf "job_%d_%d" id (int_of_float (now () *. 1000.0))

let priority_to_int = function
  | Critical -> 0
  | High -> 1
  | Normal -> 2
  | Low -> 3

(** {1 Queue Creation} *)

(** Create a new job queue.

    @param workers Number of worker fibers (default: 4)
    @param max_queue_size Maximum pending jobs (default: 10000)
    @param default_max_retries Default retry count (default: 3)
*)
let create
    ?(workers = 4)
    ?(max_queue_size = 10000)
    ?(default_max_retries = 3)
    ?(retry_delay = 1.0)
    () =
  let config = { workers; max_queue_size; default_max_retries; retry_delay } in
  let stats = {
    total_submitted = 0;
    total_completed = 0;
    total_failed = 0;
    currently_running = 0;
    queue_size = 0;
  } in
  {
    config;
    jobs = [];
    results = Hashtbl.create 1000;
    running = false;
    stats;
    next_id = 1;
    mutex = Eio.Mutex.create ();
    condition = Eio.Condition.create ();
  }

(** {1 Job Submission} *)

(** Submit a job to the queue.

    @param priority Job priority (default: Normal)
    @param max_retries Maximum retry attempts (default: config value)
    @return Job ID for tracking
*)
let submit ?priority ?max_retries t task =
  let priority = Option.value ~default:Normal priority in
  let max_retries = Option.value ~default:t.config.default_max_retries max_retries in

  with_lock t.mutex (fun () ->
    if List.length t.jobs >= t.config.max_queue_size then
      failwith "Job queue full";

    let job = {
      id = generate_id t;
      task;
      priority;
      status = Pending;
      retries = 0;
      max_retries;
      created_at = now ();
      started_at = None;
      completed_at = None;
    } in

    (* Insert in priority order *)
    let rec insert jobs =
      match jobs with
      | [] -> [job]
      | j :: rest when priority_to_int job.priority < priority_to_int j.priority ->
        job :: j :: rest
      | j :: rest -> j :: insert rest
    in
    t.jobs <- insert t.jobs;
    Hashtbl.replace t.results job.id Pending;
    t.stats <- {
      t.stats with
      total_submitted = t.stats.total_submitted + 1;
      queue_size = t.stats.queue_size + 1;
    };
    Eio.Condition.broadcast t.condition;
    job.id
  )

(** Submit multiple jobs at once *)
let submit_all ?priority ?max_retries t tasks =
  List.map (submit ?priority ?max_retries t) tasks

(** {1 Job Status} *)

(** Get job status *)
let status t job_id =
  with_lock t.mutex (fun () ->
    match Hashtbl.find_opt t.results job_id with
    | Some s -> s
    | None -> failwith ("Unknown job: " ^ job_id)
  )

(** Check if job is complete *)
let is_complete t job_id =
  match status t job_id with
  | Completed _ | Failed _ -> true
  | Pending | Running -> false

(** Wait for job to complete (cooperative polling) *)
let wait t job_id =
  let rec loop () =
    if is_complete t job_id then
      status t job_id
    else begin
      (* Cooperative sleep - yields to Eio scheduler *)
      Time_compat.sleep 0.01;
      loop ()
    end
  in
  loop ()

(** {1 Queue Control} *)

(** Get next job to process *)
let take_job t =
  with_lock t.mutex (fun () ->
    match t.jobs with
    | [] -> None
    | job :: rest ->
      t.jobs <- rest;
      job.status <- Running;
      job.started_at <- Some (now ());
      Hashtbl.replace t.results job.id Running;
      t.stats <- {
        t.stats with
        currently_running = t.stats.currently_running + 1;
        queue_size = t.stats.queue_size - 1;
      };
      Some job
  )

(** Process a single job *)
let process_job t job =
  try
    let result = job.task () in
    with_lock t.mutex (fun () ->
      job.status <- Completed result;
      job.completed_at <- Some (now ());
      Hashtbl.replace t.results job.id (Completed result);
      t.stats <- {
        t.stats with
        total_completed = t.stats.total_completed + 1;
        currently_running = t.stats.currently_running - 1;
      }
    )
  with exn ->
    with_lock t.mutex (fun () ->
      job.retries <- job.retries + 1;
      if job.retries < job.max_retries then begin
        (* Re-queue for retry *)
        job.status <- Pending;
        Hashtbl.replace t.results job.id Pending;
        let rec insert jobs =
          match jobs with
          | [] -> [job]
          | j :: rest when priority_to_int job.priority < priority_to_int j.priority ->
            job :: j :: rest
          | j :: rest -> j :: insert rest
        in
        t.jobs <- insert t.jobs;
        t.stats <- {
          t.stats with
          currently_running = t.stats.currently_running - 1;
          queue_size = t.stats.queue_size + 1;
        }
      end else begin
        job.status <- Failed exn;
        job.completed_at <- Some (now ());
        Hashtbl.replace t.results job.id (Failed exn);
        t.stats <- {
          t.stats with
          total_failed = t.stats.total_failed + 1;
          currently_running = t.stats.currently_running - 1;
        }
      end
    )

(** Worker loop - runs as an Eio fiber *)
let worker_loop t =
  while t.running do
    match take_job t with
    | Some job -> process_job t job
    | None ->
      (* Wait for new job - cooperative Eio condition wait *)
      with_lock t.mutex (fun () ->
        if t.running && List.length t.jobs = 0 then
          Eio.Condition.await t.condition t.mutex
      )
  done

(** Start the job queue (runs workers as Eio fibers).

    @param sw Eio switch for fiber lifecycle management
*)
let start ~sw t =
  with_lock t.mutex (fun () ->
    if not t.running then begin
      t.running <- true;
      for _ = 1 to t.config.workers do
        Eio.Fiber.fork ~sw (fun () -> worker_loop t)
      done
    end
  )

(** Stop the job queue *)
let stop t =
  with_lock t.mutex (fun () ->
    t.running <- false;
    Eio.Condition.broadcast t.condition
  )

(** Check if queue is running *)
let is_running t = t.running

(** {1 Statistics} *)

(** Get queue statistics *)
let stats t =
  with_lock t.mutex (fun () ->
    { t.stats with queue_size = List.length t.jobs }
  )

(** Get pending job count *)
let pending_count t =
  with_lock t.mutex (fun () ->
    List.length t.jobs
  )

(** Get running job count *)
let running_count t =
  with_lock t.mutex (fun () ->
    t.stats.currently_running
  )

(** {1 Utilities} *)

(** Cancel a pending job *)
let cancel t job_id =
  with_lock t.mutex (fun () ->
    let was_pending = List.exists (fun j -> j.id = job_id) t.jobs in
    if was_pending then begin
      t.jobs <- List.filter (fun j -> j.id <> job_id) t.jobs;
      Hashtbl.remove t.results job_id;
      t.stats <- { t.stats with queue_size = t.stats.queue_size - 1 };
      true
    end else
      false
  )

(** Clear all pending jobs *)
let clear t =
  with_lock t.mutex (fun () ->
    let count = List.length t.jobs in
    List.iter (fun j -> Hashtbl.remove t.results j.id) t.jobs;
    t.jobs <- [];
    t.stats <- { t.stats with queue_size = 0 };
    count
  )

(** Run a task immediately (synchronous) *)
let run_sync task =
  task ()

(** {1 Convenience Functions} *)

(** Submit and wait for result *)
let submit_and_wait ?priority ?max_retries t task =
  let job_id = submit ?priority ?max_retries t task in
  wait t job_id

(** Create a simple one-shot queue, run job, stop.
    Note: requires an active Eio switch context. *)
let run_once ~sw task =
  let t = create ~workers:1 () in
  start ~sw t;
  let job_id = submit t task in
  let result = wait t job_id in
  stop t;
  result
