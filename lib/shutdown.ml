(** Kirin Graceful Shutdown Module

    Provides graceful shutdown capabilities for production deployments.
    Handles SIGTERM/SIGINT signals and drains active connections.

    {b Features:}
    - Signal handling (SIGTERM, SIGINT)
    - Connection draining with timeout
    - Shutdown hooks for cleanup
    - Health check integration

    {b Example:}
    {[
      let shutdown = Shutdown.create ~timeout:30.0 () in

      (* Register cleanup hooks *)
      Shutdown.on_shutdown shutdown (fun () ->
        Printf.printf "Closing database connections...\n";
        Db.close_all ());

      (* Start server with graceful shutdown *)
      Shutdown.run shutdown (fun () ->
        Kirin.start ~port:8080 @@ routes)
    ]}
*)

(** {1 Types} *)

(** Shutdown state *)
type state =
  | Running
  | ShuttingDown
  | Stopped

(** Shutdown configuration *)
type config = {
  timeout : float;           (** Max seconds to wait for connections to drain *)
  force_after : float;       (** Force shutdown after this many seconds *)
}

(** Shutdown hook *)
type hook = unit -> unit

(** Shutdown manager *)
type t = {
  config : config;
  mutable state : state;
  mutable hooks : hook list;
  mutable active_connections : int;
  mutex : Mutex.t;
  condition : Condition.t;
}

(** {1 Helpers} *)

let now () = Unix.gettimeofday ()

let with_lock t f =
  Mutex.lock t.mutex;
  Fun.protect ~finally:(fun () -> Mutex.unlock t.mutex) f

(** {1 Creation} *)

(** Default configuration *)
let default_config = {
  timeout = 30.0;
  force_after = 60.0;
}

(** Create a shutdown manager *)
let create ?(timeout = 30.0) ?(force_after = 60.0) () = {
  config = { timeout; force_after };
  state = Running;
  hooks = [];
  active_connections = 0;
  mutex = Mutex.create ();
  condition = Condition.create ();
}

(** {1 State Management} *)

(** Get current state *)
let state t = with_lock t (fun () -> t.state)

(** Check if running *)
let is_running t = state t = Running

(** Check if shutting down *)
let is_shutting_down t = state t = ShuttingDown

(** Check if stopped *)
let is_stopped t = state t = Stopped

(** {1 Connection Tracking} *)

(** Increment active connection count *)
let connection_start t =
  with_lock t (fun () ->
    if t.state = Running then begin
      t.active_connections <- t.active_connections + 1;
      true
    end else
      false  (* Reject new connections during shutdown *)
  )

(** Decrement active connection count *)
let connection_end t =
  with_lock t (fun () ->
    t.active_connections <- max 0 (t.active_connections - 1);
    if t.active_connections = 0 then
      Condition.broadcast t.condition
  )

(** Get active connection count *)
let active_connections t =
  with_lock t (fun () -> t.active_connections)

(** Wrap a handler to track connections *)
let track_connection t handler req =
  if connection_start t then begin
    Fun.protect
      ~finally:(fun () -> connection_end t)
      (fun () -> handler req)
  end else
    (* Server is shutting down, reject request *)
    match state t with
    | Running -> 
      (* Should not happen if connection_start returned false, but handle anyway *)
      Response.make ~status:`Service_unavailable (`String "Server is shutting down")
    | ShuttingDown | Stopped ->
      Response.make ~status:`Service_unavailable (`String "Server is shutting down")

(** {1 Hooks} *)

(** Register a shutdown hook *)
let on_shutdown t hook =
  with_lock t (fun () ->
    t.hooks <- hook :: t.hooks
  )

(** Run all shutdown hooks *)
let run_hooks t =
  let hooks = with_lock t (fun () -> t.hooks) in
  List.iter (fun hook ->
    try hook ()
    with exn ->
      Printf.eprintf "Shutdown hook error: %s\n%!" (Printexc.to_string exn)
  ) hooks

(** {1 Shutdown Process} *)

(** Wait for connections to drain *)
let wait_for_drain t =
  let deadline = now () +. t.config.timeout in
  with_lock t (fun () ->
    while t.active_connections > 0 && now () < deadline do
      let remaining = deadline -. now () in
      if remaining > 0.0 then begin
        (* Wait with timeout - simplified polling *)
        Mutex.unlock t.mutex;
        Unix.sleepf (min 0.1 remaining);
        Mutex.lock t.mutex
      end
    done;
    t.active_connections = 0
  )

(** Initiate graceful shutdown *)
let initiate t =
  let should_shutdown = with_lock t (fun () ->
    if t.state = Running then begin
      t.state <- ShuttingDown;
      true
    end else
      false
  ) in

  if should_shutdown then begin
    Printf.printf "\n🛑 Initiating graceful shutdown...\n%!";

    (* Wait for active connections to drain *)
    let drained = wait_for_drain t in
    if drained then
      Printf.printf "✅ All connections drained\n%!"
    else
      Printf.printf "⚠️  Timeout waiting for connections (forcing shutdown)\n%!";

    (* Run cleanup hooks *)
    Printf.printf "🧹 Running cleanup hooks...\n%!";
    run_hooks t;

    (* Mark as stopped *)
    with_lock t (fun () -> t.state <- Stopped);
    Printf.printf "👋 Shutdown complete\n%!"
  end

(** {1 Signal Handling} *)

(** Setup signal handlers *)
let setup_signals t =
  let handler _ = initiate t in
  Sys.set_signal Sys.sigterm (Sys.Signal_handle handler);
  Sys.set_signal Sys.sigint (Sys.Signal_handle handler);
  Printf.printf "📡 Signal handlers installed (SIGTERM, SIGINT)\n%!"

(** {1 Server Integration} *)

(** Run with graceful shutdown support

    Sets up signal handlers and wraps the server runner.
    When a shutdown signal is received, it will:
    1. Stop accepting new connections
    2. Wait for active connections to drain
    3. Run cleanup hooks
    4. Exit
*)
let run t server_fn =
  setup_signals t;
  try
    server_fn ()
  with exn ->
    Printf.eprintf "Server error: %s\n%!" (Printexc.to_string exn);
    initiate t

(** Middleware that tracks connections and handles shutdown *)
let middleware t handler req =
  track_connection t handler req

(** {1 Status} *)

(** Get shutdown status as JSON *)
let status_json t =
  let state_str = match state t with
    | Running -> "running"
    | ShuttingDown -> "shutting_down"
    | Stopped -> "stopped"
  in
  `Assoc [
    ("state", `String state_str);
    ("active_connections", `Int (active_connections t));
    ("hooks_count", `Int (List.length (with_lock t (fun () -> t.hooks))));
  ]
