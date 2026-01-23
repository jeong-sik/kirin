(** Solid.js SSR Worker

    Node.js subprocess management for Solid.js SSR. *)

(** {1 Worker Types} *)

(** Worker state *)
type state =
  | Starting
  | Ready
  | Busy
  | Unhealthy
  | Stopped

(** Worker handle *)
type t = {
  mutable pid: int option;
  mutable state: state;
  mutable request_count: int;
  mutable last_health_check: float;
  mutable stdin: out_channel option;
  mutable stdout: in_channel option;
  bundle: string;
  max_requests: int;
  timeout: float;
}

(** {1 Worker Creation} *)

(** Create worker *)
let create ~bundle ?(max_requests = 5000) ?(timeout = 10.0) () = {
  pid = None;
  state = Stopped;
  request_count = 0;
  last_health_check = 0.0;
  stdin = None;
  stdout = None;
  bundle;
  max_requests;
  timeout;
}

(** Start worker subprocess *)
let start worker =
  if worker.state <> Stopped then
    Error "Worker already running"
  else begin
    worker.state <- Starting;
    try
      let (stdout_read, stdout_write) = Unix.pipe () in
      let (stdin_read, stdin_write) = Unix.pipe () in

      (* Fork and exec node process *)
      let pid = Unix.create_process "node"
        [|"node"; "--expose-gc"; worker.bundle|]
        stdin_read stdout_write Unix.stderr in

      Unix.close stdout_write;
      Unix.close stdin_read;

      worker.pid <- Some pid;
      worker.stdin <- Some (Unix.out_channel_of_descr stdin_write);
      worker.stdout <- Some (Unix.in_channel_of_descr stdout_read);
      worker.state <- Ready;
      worker.request_count <- 0;
      worker.last_health_check <- Unix.time ();
      Ok ()
    with e ->
      worker.state <- Stopped;
      Error (Printexc.to_string e)
  end

(** Stop worker *)
let stop worker =
  (match worker.stdin with
   | Some ch -> close_out_noerr ch
   | None -> ());
  (match worker.stdout with
   | Some ch -> close_in_noerr ch
   | None -> ());
  (match worker.pid with
   | Some pid ->
     (try Unix.kill pid Sys.sigterm with _ -> ());
     ignore (Unix.waitpid [] pid)
   | None -> ());
  worker.pid <- None;
  worker.stdin <- None;
  worker.stdout <- None;
  worker.state <- Stopped

(** Restart worker *)
let restart worker =
  stop worker;
  start worker

(** {1 Communication} *)

(** Send request and receive response *)
let send_request worker request =
  match worker.stdin, worker.stdout with
  | Some stdin, Some stdout when worker.state = Ready ->
    worker.state <- Busy;
    (try
       output_string stdin request;
       flush stdin;

       let response = input_line stdout in
       worker.request_count <- worker.request_count + 1;
       worker.state <- Ready;

       (* Check if restart needed *)
       if worker.request_count >= worker.max_requests then
         ignore (restart worker);

       Ok response
     with e ->
       worker.state <- Unhealthy;
       Error (Printexc.to_string e))
  | _ ->
    Error "Worker not ready"

(** {1 Health Check} *)

(** Check worker health *)
let health_check worker =
  let (_, request) = Protocol.encode_health_request () in
  match send_request worker request with
  | Ok response ->
    (match Protocol.decode_health_response response with
     | Ok status ->
       worker.last_health_check <- Unix.time ();
       if status.ok then
         worker.state <- Ready
       else
         worker.state <- Unhealthy;
       Ok status
     | Error msg ->
       worker.state <- Unhealthy;
       Error msg)
  | Error msg ->
    worker.state <- Unhealthy;
    Error msg

(** {1 Rendering} *)

(** Render URL to HTML *)
let render worker ~url ?(props = `Assoc []) () =
  let req = { Protocol.url; props; meta = None } in
  let (_, request) = Protocol.encode_render_request req in
  match send_request worker request with
  | Ok response -> Protocol.decode_render_response response
  | Error msg -> Error msg

(** {1 Status} *)

(** Get worker state *)
let get_state worker = worker.state

(** Get request count *)
let get_request_count worker = worker.request_count

(** Check if worker is ready *)
let is_ready worker = worker.state = Ready

(** Check if worker needs restart *)
let needs_restart worker =
  worker.request_count >= worker.max_requests ||
  worker.state = Unhealthy
