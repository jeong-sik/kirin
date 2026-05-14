(** Kirin Cluster - Multi-process Scale Up (PM2 style)

    This module enables scaling across multiple CPU cores using operating system processes.
    It uses SO_REUSEPORT to allow multiple processes to bind to the same port.

    {b Benefits:}
    - Resilience: If one worker crashes, others keep running.
    - Isolation: Independent GC heaps per worker.
    - Performance: True parallelism without GIL or GC pauses affecting peers.

    {b Usage:}
    {[ 
      let () = Kirin.Cluster.start ~workers:4 (fun () ->
        Kirin.start ~port:8000 @@ routes
      )
    ]}
*)

open Unix

(** Worker process info *)
type worker = {
  pid : int;
  id : int;
}

(** Cluster state *)
type t = {
  workers : (int, worker) Hashtbl.t;
  master_pid : int;
}

(* Per-worker-id restart bookkeeping for the supervisor.

   [last_spawn] is the wall-clock time of the most recent spawn for
   that worker id.  [recent_crashes] counts consecutive crashes that
   happened *faster than [min_restart_interval]* — the supervisor
   uses this to detect a crash loop and give up before it pegs a CPU
   forking dead children. *)
type restart_state = {
  mutable last_spawn : float;
  mutable recent_crashes : int;
}

(** Minimum spacing between two spawns of the *same* worker id.  A
    crash that happens faster than this counts toward
    [max_consecutive_crashes] and triggers a sleep before the
    respawn. *)
let default_min_restart_interval = 1.0

(** Hard cap on consecutive fast crashes for a single worker id
    before the supervisor gives up and exits the master.  Without
    this cap a worker that reliably raises on startup (port already
    bound, config invalid, missing secret) would let the supervisor
    fork-bomb the host. *)
let default_max_consecutive_crashes = 10

(** Pure decision: how long should the supervisor sleep before the
    next respawn?  [<=0.0] means spawn immediately. *)
let compute_backoff_delay ~last_spawn ~now ~min_restart_interval =
  let elapsed = now -. last_spawn in
  if elapsed >= min_restart_interval then 0.0
  else min_restart_interval -. elapsed

(** Pure decision: has this worker id crashed too many times in a row
    fast enough to be considered an unrecoverable crash loop? *)
let should_give_up ~recent_crashes ~max_consecutive_crashes =
  recent_crashes >= max_consecutive_crashes

(** Pure decision: update the recent-crash counter for the *current*
    crash.  A crash that happened slower than [min_restart_interval]
    resets the counter (the previous spawn is considered "stable
    enough"); a fast crash increments it. *)
let next_recent_crashes ~last_spawn ~now ~min_restart_interval ~recent_crashes =
  let elapsed = now -. last_spawn in
  if elapsed >= min_restart_interval then 1
  else recent_crashes + 1

(** Create a socket with SO_REUSEPORT *)
let create_socket _port =
  let sock = socket PF_INET SOCK_STREAM 0 in
  setsockopt sock SO_REUSEADDR true;  (* OCaml Unix doesn't expose SO_REUSEPORT directly in older versions, 
     but modern Linux/macOS support it. We assume Eio/Cohttp handles the actual binding
     if we let the workers bind independently. 
     
     Actually, the best way in OCaml is to bind in master and inherit the FD, 
     OR use SO_REUSEPORT in each worker.
     
     Simplest strategy: Let workers bind independently with reuse_port=true if supported.
  *)
  sock

(** Spawn a worker process *)
let spawn_worker id entry_point =
  match fork () with
  | 0 -> (* Child *)
    Logger.info "[Worker %d] Started (PID: %d)" id (getpid ());
    (try entry_point () with
     | Eio.Cancel.Cancelled _ as e -> raise e
     | e ->
      Logger.error "[Worker %d] Crashed: %s" id (Printexc.to_string e));
    exit 0
  | pid -> (* Parent *)
    { pid; id }

(** Start the cluster *)
let start
    ?(workers = 0)
    ?(min_restart_interval = default_min_restart_interval)
    ?(max_consecutive_crashes = default_max_consecutive_crashes)
    entry_point =
  let num_workers = if workers > 0 then workers else Domain.recommended_domain_count () in
  Logger.info "[Master] Starting %d workers..." num_workers;

  let cluster = {
    workers = Hashtbl.create num_workers;
    master_pid = getpid ();
  } in

  (* Per-worker-id bookkeeping: how recently this slot last spawned,
     and how many consecutive fast crashes it has accumulated.  Keyed
     by worker [id], not [pid], because the [pid] changes on every
     respawn but the slot identity is the [id]. *)
  let restart_states : (int, restart_state) Hashtbl.t =
    Hashtbl.create num_workers
  in
  let record_spawn id =
    match Hashtbl.find_opt restart_states id with
    | Some st -> st.last_spawn <- gettimeofday ()
    | None ->
      Hashtbl.add restart_states id
        { last_spawn = gettimeofday (); recent_crashes = 0 }
  in

  (* Spawn initial workers *)
  for i = 1 to num_workers do
    let w = spawn_worker i entry_point in
    Hashtbl.replace cluster.workers w.pid w;
    record_spawn w.id
  done;

  (* Respawn one worker [id] with backoff + crash-loop cap.  Returns
     [`Continue] on a normal respawn, [`Give_up] when the slot has
     exceeded [max_consecutive_crashes] fast crashes — the supervisor
     stops the master in that case rather than fork-bombing. *)
  let respawn_with_backoff id =
    let now = gettimeofday () in
    let st =
      match Hashtbl.find_opt restart_states id with
      | Some st -> st
      | None ->
        let st = { last_spawn = now; recent_crashes = 0 } in
        Hashtbl.add restart_states id st;
        st
    in
    st.recent_crashes <-
      next_recent_crashes
        ~last_spawn:st.last_spawn
        ~now
        ~min_restart_interval
        ~recent_crashes:st.recent_crashes;
    if should_give_up
         ~recent_crashes:st.recent_crashes
         ~max_consecutive_crashes
    then begin
      Logger.error
        "[Master] Worker %d crashed %d times in under %.1fs each — giving up"
        id st.recent_crashes min_restart_interval;
      `Give_up
    end else begin
      let delay =
        compute_backoff_delay
          ~last_spawn:st.last_spawn
          ~now
          ~min_restart_interval
      in
      if delay > 0.0 then begin
        Logger.warn
          "[Master] Worker %d respawning too fast — sleeping %.2fs (crash #%d)"
          id delay st.recent_crashes;
        sleepf delay
      end;
      let new_w = spawn_worker id entry_point in
      Hashtbl.replace cluster.workers new_w.pid new_w;
      st.last_spawn <- gettimeofday ();
      `Continue
    end
  in

  (* Supervisor loop *)
  let rec loop () =
    try
      let pid, status = wait () in
      match Hashtbl.find_opt cluster.workers pid with
      | Some w ->
        Hashtbl.remove cluster.workers pid;
        let reason = match status with
          | WEXITED c -> Printf.sprintf "exit code %d" c
          | WSIGNALED s -> Printf.sprintf "signal %d" s
          | WSTOPPED s -> Printf.sprintf "stopped %d" s
        in
        Logger.warn "[Master] Worker %d (PID %d) died: %s. Respawning..." w.id pid reason;
        (match respawn_with_backoff w.id with
         | `Continue -> loop ()
         | `Give_up ->
           Logger.error
             "[Master] Aborting cluster: worker %d in unrecoverable crash loop"
             w.id;
           (* Surface the give-up to the caller of [start] instead of
              silently looping; let the deployment supervisor
              (systemd, k8s, etc.) decide whether to restart the
              master from scratch. *)
           raise Exit)
      | None -> loop () (* Unknown child? *)
    with
    | Exit -> raise Exit
    | Unix_error (EINTR, _, _) -> loop ()
    | e ->
      Logger.error "[Master] Supervisor error: %s" (Printexc.to_string e);
      (* Sleep on unknown supervisor errors so a recurring failure
         (e.g. ECHILD after every worker is gone) does not spin the
         CPU re-entering [loop] thousands of times a second. *)
      sleepf min_restart_interval;
      loop ()
  in
  try loop () with Exit -> ()
