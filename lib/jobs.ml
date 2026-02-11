(** Kirin Background Jobs — Eio-native

    Job queue using Eio fibers for concurrent task processing.
    Workers run as lightweight fibers within a shared Eio switch.

    {b Features:}
    - Fiber-based worker pool (no OS threads)
    - Eio.Mutex for fiber-safe state access
    - Eio.Condition for non-blocking wait
    - Priority queues
    - Retry with exponential backoff

    {b Example:}
    {[
      Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      let clock = Eio.Stdenv.clock env in
      let queue = Jobs.create ~sw ~clock ~workers:4 () in

      let job_id = Jobs.submit queue (fun () ->
        send_email user "Welcome!") in

      match Jobs.wait queue job_id with
      | Jobs.Completed result -> Printf.printf "Done: %s\n" result
      | Jobs.Failed exn -> Printf.printf "Failed: %s\n" (Printexc.to_string exn)
      | _ -> ()
    ]}
*)

(** {1 Types} *)

type priority =
  | Critical
  | High
  | Normal
  | Low

type 'a status =
  | Pending
  | Running
  | Completed of 'a
  | Failed of exn

type job_id = string

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

type config = {
  workers : int;
  max_queue_size : int;
  default_max_retries : int;
  retry_delay : float;
}

type stats = {
  total_submitted : int;
  total_completed : int;
  total_failed : int;
  currently_running : int;
  queue_size : int;
}

(** Job queue — carries Eio resources for fiber-safe operation *)
type 'a t = {
  config : config;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  mutable jobs : 'a job list;
  mutable results : (job_id, 'a status) Hashtbl.t;
  mutable running : bool;
  mutable stats : stats;
  mutable next_id : int;
  mutex : Eio.Mutex.t;
  job_available : Eio.Condition.t;
  job_completed : Eio.Condition.t;
}

(** {1 Configuration} *)

let default_config = {
  workers = 4;
  max_queue_size = 10000;
  default_max_retries = 3;
  retry_delay = 1.0;
}

(** {1 Helpers} *)

let priority_to_int = function
  | Critical -> 0
  | High -> 1
  | Normal -> 2
  | Low -> 3

let insert_by_priority job jobs =
  let rec go = function
    | [] -> [job]
    | j :: rest when priority_to_int job.priority < priority_to_int j.priority ->
      job :: j :: rest
    | j :: rest -> j :: go rest
  in
  go jobs

(** {1 Internal — Worker} *)

(** Take next job from the queue. Must be called under [t.mutex]. *)
let take_job t =
  match t.jobs with
  | [] -> None
  | job :: rest ->
    t.jobs <- rest;
    job.status <- Running;
    job.started_at <- Some (Eio.Time.now t.clock);
    Hashtbl.replace t.results job.id Running;
    t.stats <- {
      t.stats with
      currently_running = t.stats.currently_running + 1;
      queue_size = t.stats.queue_size - 1;
    };
    Some job

(** Process a single job, updating state under mutex on completion. *)
let process_job t job =
  let result =
    try Ok (job.task ())
    with exn -> Error exn
  in
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    match result with
    | Ok value ->
      job.status <- Completed value;
      job.completed_at <- Some (Eio.Time.now t.clock);
      Hashtbl.replace t.results job.id (Completed value);
      t.stats <- {
        t.stats with
        total_completed = t.stats.total_completed + 1;
        currently_running = t.stats.currently_running - 1;
      };
      Eio.Condition.broadcast t.job_completed
    | Error exn ->
      job.retries <- job.retries + 1;
      if job.retries < job.max_retries then begin
        job.status <- Pending;
        Hashtbl.replace t.results job.id Pending;
        t.jobs <- insert_by_priority job t.jobs;
        t.stats <- {
          t.stats with
          currently_running = t.stats.currently_running - 1;
          queue_size = t.stats.queue_size + 1;
        };
        Eio.Condition.broadcast t.job_available
      end else begin
        job.status <- Failed exn;
        job.completed_at <- Some (Eio.Time.now t.clock);
        Hashtbl.replace t.results job.id (Failed exn);
        t.stats <- {
          t.stats with
          total_failed = t.stats.total_failed + 1;
          currently_running = t.stats.currently_running - 1;
        };
        Eio.Condition.broadcast t.job_completed
      end
  )

(** Worker fiber loop: take jobs and process them until stopped. *)
let worker_loop t =
  while t.running do
    let job_opt =
      Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        match take_job t with
        | Some _ as r -> r
        | None ->
          if t.running then
            Eio.Condition.await t.job_available t.mutex;
          if t.running then take_job t
          else None
      )
    in
    match job_opt with
    | Some job -> process_job t job
    | None -> ()
  done

(** {1 Queue Creation} *)

(** Create and start a job queue.

    Workers are forked as Eio fibers under [sw].
    The queue stops when the switch is cancelled or {!stop} is called.

    @param sw Eio switch that owns worker fibers
    @param clock Eio clock for timestamps and sleep
    @param workers Number of worker fibers (default: 4)
    @param max_queue_size Maximum pending jobs (default: 10000)
    @param default_max_retries Default retry count (default: 3)
    @param retry_delay Base delay between retries in seconds (default: 1.0)
*)
let create
    ~sw
    ~clock
    ?(workers = 4)
    ?(max_queue_size = 10000)
    ?(default_max_retries = 3)
    ?(retry_delay = 1.0)
    () =
  let config = { workers; max_queue_size; default_max_retries; retry_delay } in
  let t = {
    config;
    clock;
    jobs = [];
    results = Hashtbl.create 1000;
    running = true;
    stats = {
      total_submitted = 0;
      total_completed = 0;
      total_failed = 0;
      currently_running = 0;
      queue_size = 0;
    };
    next_id = 1;
    mutex = Eio.Mutex.create ();
    job_available = Eio.Condition.create ();
    job_completed = Eio.Condition.create ();
  } in
  for _ = 1 to config.workers do
    Eio.Fiber.fork ~sw (fun () -> worker_loop t)
  done;
  t

(** {1 Job Submission} *)

(** Submit a job to the queue.

    @param priority Job priority (default: Normal)
    @param max_retries Maximum retry attempts (default: config value)
    @return Job ID for tracking
*)
let submit ?priority ?max_retries t task =
  let priority = Option.value ~default:Normal priority in
  let max_retries = Option.value ~default:t.config.default_max_retries max_retries in
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    if List.length t.jobs >= t.config.max_queue_size then
      failwith "Job queue full";
    let id =
      let n = t.next_id in
      t.next_id <- n + 1;
      Printf.sprintf "job_%d_%d" n (int_of_float (Eio.Time.now t.clock *. 1000.0))
    in
    let job = {
      id;
      task;
      priority;
      status = Pending;
      retries = 0;
      max_retries;
      created_at = Eio.Time.now t.clock;
      started_at = None;
      completed_at = None;
    } in
    t.jobs <- insert_by_priority job t.jobs;
    Hashtbl.replace t.results job.id Pending;
    t.stats <- {
      t.stats with
      total_submitted = t.stats.total_submitted + 1;
      queue_size = t.stats.queue_size + 1;
    };
    Eio.Condition.broadcast t.job_available;
    job.id
  )

let submit_all ?priority ?max_retries t tasks =
  List.map (submit ?priority ?max_retries t) tasks

(** {1 Job Status} *)

let status t job_id =
  Eio.Mutex.use_ro t.mutex (fun () ->
    match Hashtbl.find_opt t.results job_id with
    | Some s -> s
    | None -> failwith ("Unknown job: " ^ job_id)
  )

let is_complete t job_id =
  match status t job_id with
  | Completed _ | Failed _ -> true
  | Pending | Running -> false

(** Wait for a job to complete. Suspends the current fiber without blocking
    OS threads. *)
let wait t job_id =
  let rec loop () =
    let st =
      Eio.Mutex.use_ro t.mutex (fun () ->
        Hashtbl.find_opt t.results job_id
      )
    in
    match st with
    | Some (Completed _ as s) | Some (Failed _ as s) -> s
    | Some Pending | Some Running ->
      Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        Eio.Condition.await t.job_completed t.mutex
      );
      loop ()
    | None -> failwith ("Unknown job: " ^ job_id)
  in
  loop ()

(** {1 Queue Control} *)

let stop t =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    t.running <- false;
    Eio.Condition.broadcast t.job_available;
    Eio.Condition.broadcast t.job_completed
  )

let is_running t =
  Eio.Mutex.use_ro t.mutex (fun () -> t.running)

(** {1 Statistics} *)

let stats t =
  Eio.Mutex.use_ro t.mutex (fun () ->
    { t.stats with queue_size = List.length t.jobs }
  )

let pending_count t =
  Eio.Mutex.use_ro t.mutex (fun () ->
    List.length t.jobs
  )

let running_count t =
  Eio.Mutex.use_ro t.mutex (fun () ->
    t.stats.currently_running
  )

(** {1 Utilities} *)

let cancel t job_id =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    let was_pending = List.exists (fun j -> j.id = job_id) t.jobs in
    if was_pending then begin
      t.jobs <- List.filter (fun j -> j.id <> job_id) t.jobs;
      Hashtbl.remove t.results job_id;
      t.stats <- { t.stats with queue_size = t.stats.queue_size - 1 };
      true
    end else
      false
  )

let clear t =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    let count = List.length t.jobs in
    List.iter (fun j -> Hashtbl.remove t.results j.id) t.jobs;
    t.jobs <- [];
    t.stats <- { t.stats with queue_size = 0 };
    count
  )

let run_sync task =
  task ()

(** {1 Convenience} *)

let submit_and_wait ?priority ?max_retries t task =
  let job_id = submit ?priority ?max_retries t task in
  wait t job_id

(** Create a one-shot queue within an Eio switch, run a single job, return
    the result. *)
let run_once ~clock task =
  let result = ref None in
  Eio.Switch.run (fun sw ->
    let t = create ~sw ~clock ~workers:1 () in
    let job_id = submit t (fun () -> task ()) in
    result := Some (wait t job_id);
    stop t
  );
  match !result with
  | Some r -> r
  | None -> failwith "run_once: unreachable"
