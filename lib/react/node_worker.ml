(** Node.js SSR Worker

    Manages Node.js subprocess for React server-side rendering.
    Uses JSON-RPC over stdio for communication.
*)

(** Node.js worker state *)
type process = {
  pid: int;
  in_ch: in_channel;
  out_ch: out_channel;
}

let reap_pid pid =
  (* Avoid leaving zombies, but don't hang request path forever. *)
  let wait_for ~timeout_s =
    let deadline = Unix.gettimeofday () +. timeout_s in
    let rec loop () =
      match Unix.waitpid [ Unix.WNOHANG ] pid with
      | 0, _status ->
          if Unix.gettimeofday () >= deadline then false
          else (
            Kirin.Time_compat.sleep 0.05;
            loop ())
      | _pid, _status -> true
    in
    try loop () with
    | Unix.Unix_error (Unix.ECHILD, _, _) -> true
  in
  if not (wait_for ~timeout_s:0.5) then (
    (try Unix.kill pid Sys.sigterm with _ -> ());
    ignore (wait_for ~timeout_s:0.5);
    (try Unix.kill pid Sys.sigkill with _ -> ());
    (try ignore (Unix.waitpid [] pid) with _ -> ()))

type t = {
  mutable process: process option;
  config: Worker.config;
  mutable stats: Worker.stats;
  mutable status: Worker.status;
  mutable request_count: int;
  mutable error_count: int;
  start_time: float;
  on_event: Worker.event_handler;
}

(** Start Node.js process *)
let start_process config =
  try
    let env_tbl : (string, string) Hashtbl.t = Hashtbl.create 64 in
    Array.iter
      (fun kv ->
        match String.index_opt kv '=' with
        | None -> ()
        | Some i ->
            let k = String.sub kv 0 i in
            let v = String.sub kv (i + 1) (String.length kv - i - 1) in
            Hashtbl.replace env_tbl k v)
      (Unix.environment ());
    List.iter (fun (k, v) -> Hashtbl.replace env_tbl k v) config.Worker.env;
    Hashtbl.replace env_tbl "NODE_ENV" "production";
    let env =
      Hashtbl.fold (fun k v acc -> (k ^ "=" ^ v) :: acc) env_tbl [] |> Array.of_list
    in

    (* No shell: argv-only spawn prevents injection via bundle_path/env values. *)
    let (stdin_r, stdin_w) = Unix.pipe () in
    let (stdout_r, stdout_w) = Unix.pipe () in
    Unix.set_close_on_exec stdin_w;
    Unix.set_close_on_exec stdout_r;
    let close_noerr fd = try Unix.close fd with _ -> () in
    try
      let pid =
        Unix.create_process_env
          "node"
          [| "node"; config.Worker.bundle_path |]
          env
          stdin_r
          stdout_w
          Unix.stderr
      in
      (* Parent never uses these ends. *)
      close_noerr stdin_r;
      close_noerr stdout_w;
      let in_ch = Unix.in_channel_of_descr stdout_r in
      let out_ch = Unix.out_channel_of_descr stdin_w in
      Some { pid; in_ch; out_ch }
    with
    | e ->
      close_noerr stdin_r;
      close_noerr stdin_w;
      close_noerr stdout_r;
      close_noerr stdout_w;
      raise e
  with Unix.Unix_error (err, _, _) ->
    Printf.eprintf "Failed to start Node.js: %s\n%!" (Unix.error_message err);
    None

(** Create new worker *)
let create ?(on_event = Worker.null_handler) config =
  let worker = {
    process = None;
    config;
    stats = Worker.empty_stats;
    status = Worker.Dead;
    request_count = 0;
    error_count = 0;
    start_time = Unix.gettimeofday ();
    on_event;
  } in
  (* Start the process *)
  worker.process <- start_process config;
  if worker.process <> None then begin
    worker.status <- Worker.Idle;
    on_event Worker.Started;
  end;
  worker

(** Send request and receive response *)
let send_request worker req =
  match worker.process with
  | None -> Result.Error "Worker not running"
  | Some proc ->
    try
      let request_str = Protocol.encode_request req in
      output_string proc.out_ch request_str;
      output_char proc.out_ch '\n';
      flush proc.out_ch;

      let response_str = input_line proc.in_ch in
      Protocol.decode_response response_str
    with
    | End_of_file -> Result.Error "Worker process terminated"
    | Sys_error msg -> Result.Error ("IO error: " ^ msg)

(** Render URL to HTML *)
let render worker ~url ~props =
  if worker.status = Worker.Dead then
    Result.Error "Worker is dead"
  else begin
    let id = Protocol.next_id () in
    let req = Protocol.render_request ~id ~url ~props () in

    worker.status <- Worker.Busy;
    worker.on_event (Worker.Request_started { id; url });

    let start_time = Unix.gettimeofday () in
    let result = send_request worker req in
    let duration_ms = (Unix.gettimeofday () -. start_time) *. 1000.0 in

    worker.request_count <- worker.request_count + 1;
    worker.status <- Worker.Idle;

    (* Update stats *)
    let prev_total = worker.stats.avg_response_time_ms *. float_of_int (worker.stats.requests_handled) in
    worker.stats <- {
      worker.stats with
      requests_handled = worker.stats.requests_handled + 1;
      avg_response_time_ms = (prev_total +. duration_ms) /. float_of_int (worker.stats.requests_handled + 1);
      last_request_time = Some (Unix.gettimeofday ());
    };

    match result with
    | Ok response ->
      worker.on_event (Worker.Request_completed { id; duration_ms });
      Protocol.extract_html response
    | Error msg ->
      worker.error_count <- worker.error_count + 1;
      worker.stats <- { worker.stats with errors = worker.stats.errors + 1 };
      worker.on_event (Worker.Request_failed { id; error = msg });
      Result.Error msg
  end

(** Check if worker should be restarted *)
let should_restart worker =
  worker.request_count >= worker.config.restart_after_requests
  || worker.error_count >= worker.config.restart_after_errors

(** Restart worker *)
let restart worker =
  worker.on_event (Worker.Restarting { reason = "Scheduled restart" });
  worker.status <- Worker.Restarting;

  (* Close existing process *)
  (match worker.process with
  | Some proc ->
    (try
      let id = Protocol.next_id () in
      let req = Protocol.shutdown_request ~id in
      let _ = send_request worker req in
      ()
    with _ -> ());
    (try
      close_in_noerr proc.in_ch;
      close_out_noerr proc.out_ch;
    with _ -> ())
    ;
    reap_pid proc.pid
  | None -> ());

  (* Reset counters *)
  worker.request_count <- 0;
  worker.error_count <- 0;

  (* Start new process *)
  worker.process <- start_process worker.config;
  if worker.process <> None then begin
    worker.status <- Worker.Idle;
    worker.on_event Worker.Started;
  end else
    worker.status <- Worker.Dead

(** Health check *)
let health_check worker =
  match worker.status with
  | Worker.Dead -> false
  | _ ->
    let id = Protocol.next_id () in
    let req = Protocol.health_request ~id in
    match send_request worker req with
    | Ok (Protocol.Success _) -> true
    | _ -> false

(** Get worker stats *)
let stats worker =
  let uptime = Unix.gettimeofday () -. worker.start_time in
  { worker.stats with uptime_seconds = uptime }

(** Get worker status *)
let status worker = worker.status

(** Close worker *)
let close worker =
  worker.on_event Worker.Stopped;
  worker.status <- Worker.Dead;
  match worker.process with
  | Some proc ->
    (try
      (* Send shutdown request *)
      let id = Protocol.next_id () in
      let req = Protocol.shutdown_request ~id in
      let _ = send_request worker req in
      ()
    with _ -> ());
    close_in_noerr proc.in_ch;
    close_out_noerr proc.out_ch;
    reap_pid proc.pid;
    worker.process <- None
  | None -> ()

(** Render with automatic restart check *)
let render_with_restart worker ~url ~props =
  if should_restart worker then
    restart worker;
  render worker ~url ~props

(** Implement WORKER module type *)
module Impl : Worker.WORKER with type t = t = struct
  type nonrec t = t

  let create config = create config
  let render = render
  let health_check = health_check
  let stats = stats
  let status = status
  let restart = restart
  let close = close
end
