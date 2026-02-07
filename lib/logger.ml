(** Kirin Logger - Async Structured Logging (Domain-safe)

    Architecture:
    - Any domain can emit logs (no Eio effect handlers required).
    - A dedicated logger domain pops entries from a bounded queue and writes to I/O.
    - This removes I/O latency from request/worker domains.
*)

(** Log levels *)
type level = Debug | Info | Warn | Error | Fatal

let string_of_level = function
  | Debug -> "DEBUG"
  | Info -> "INFO"
  | Warn -> "WARN"
  | Error -> "ERROR"
  | Fatal -> "FATAL"

(** Log entry structure *)
type entry = {
  timestamp : float;
  level : level;
  message : string;
  context : (string * Yojson.Safe.t) list;
  trace_id : string option;
  span_id : string option;
}

(** Current configuration *)
type config = {
  min_level : level;
  format : [`Json | `Text];
  output : out_channel;
}

let current_config = ref {
  min_level = Info;
  format = `Json;
  output = stderr;
}

(** Shared log queue (bounded to avoid unbounded memory growth). *)
let max_queue_size = 65536

let queue_mutex = Mutex.create ()
let queue_cond = Condition.create ()
let queue : entry Queue.t = Queue.create ()

let output_mutex = Mutex.create ()

let running = ref false
let logger_domain : unit Domain.t option ref = ref None

let dropped_count = ref 0

(** Format timestamp ISO8601 *)
let format_time ts = 
  let tm = Unix.gmtime ts in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d.%03dZ"
    (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
    tm.tm_hour tm.tm_min tm.tm_sec (int_of_float ((ts -. floor ts) *. 1000.))

let write_entry (entry : entry) =
  Mutex.lock output_mutex;
  Fun.protect
    ~finally:(fun () -> Mutex.unlock output_mutex)
    (fun () ->
      let cfg = !current_config in
      match cfg.format with
      | `Json ->
        let json_fields = [
          ("timestamp", `String (format_time entry.timestamp));
          ("level", `String (string_of_level entry.level));
          ("message", `String entry.message);
        ] in
        let json_fields = match entry.trace_id with
          | Some tid -> ("trace_id", `String tid) :: json_fields
          | None -> json_fields
        in
        let json_fields = match entry.span_id with
          | Some sid -> ("span_id", `String sid) :: json_fields
          | None -> json_fields
        in
        let json_fields =
          if entry.context <> [] then ("context", `Assoc entry.context) :: json_fields
          else json_fields
        in
        Yojson.Safe.to_channel cfg.output (`Assoc json_fields);
        output_string cfg.output "\n";
        flush cfg.output
      | `Text ->
        output_string cfg.output (format_time entry.timestamp);
        output_string cfg.output " ";
        output_string cfg.output (string_of_level entry.level);
        output_string cfg.output " ";
        output_string cfg.output entry.message;
        (match entry.trace_id with
        | None -> ()
        | Some tid ->
          output_string cfg.output " trace_id=";
          output_string cfg.output tid);
        (match entry.span_id with
        | None -> ()
        | Some sid ->
          output_string cfg.output " span_id=";
          output_string cfg.output sid);
        if entry.context <> [] then (
          output_string cfg.output " context=";
          Yojson.Safe.to_channel cfg.output (`Assoc entry.context)
        );
        output_string cfg.output "\n";
        flush cfg.output
    )

let take_entry () : entry option =
  Mutex.lock queue_mutex;
  Fun.protect
    ~finally:(fun () -> Mutex.unlock queue_mutex)
    (fun () ->
      while Queue.is_empty queue && !running do
        Condition.wait queue_cond queue_mutex
      done;
      if Queue.is_empty queue then None else Some (Queue.take queue)
    )

(** The actual writer loop (runs in dedicated domain). *)
let rec run_logger () =
  match take_entry () with
  | None -> ()
  | Some entry ->
    (try write_entry entry with _ -> ());
    run_logger ()

(** Shutdown logger gracefully *)
let shutdown () =
  let d_opt =
    Mutex.lock queue_mutex;
    if !running then running := false;
    Condition.broadcast queue_cond;
    let d = !logger_domain in
    logger_domain := None;
    Mutex.unlock queue_mutex;
    d
  in
  Option.iter Domain.join d_opt;
  Mutex.lock output_mutex;
  Fun.protect ~finally:(fun () -> Mutex.unlock output_mutex) (fun () ->
    flush !current_config.output
  )

(** Start the logger background fiber *)
let start sw =
  Mutex.lock queue_mutex;
  let should_start =
    if !running then false
    else (
      running := true;
      true
    )
  in
  if should_start then (
    let d = Domain.spawn run_logger in
    logger_domain := Some d
  );
  Mutex.unlock queue_mutex;

  (* Ensure we always stop the logger when the surrounding switch ends. *)
  Eio.Switch.on_release sw shutdown
(** Emit log entry (Blocking if full) *)
let emit level message context trace_id =
  if level >= !current_config.min_level then
    let ts = Unix.gettimeofday () in
    let entry = { timestamp=ts; level; message; context; trace_id; span_id=None } in
    let enqueued =
      Mutex.lock queue_mutex;
      Fun.protect
        ~finally:(fun () -> Mutex.unlock queue_mutex)
        (fun () ->
          if !running then (
            if Queue.length queue >= max_queue_size then (
              incr dropped_count;
              false
            ) else (
              Queue.add entry queue;
              Condition.signal queue_cond;
              true
            )
          ) else
            false
        )
    in
    if not enqueued then (
      (* Fallback: if the async logger isn't running (or queue is full), write
         synchronously rather than blocking or growing memory. *)
      (try write_entry entry with _ -> ())
    )
(** Configure logger *)
let configure ?min_level ?format ?output () = 
  let c = !current_config in
  current_config := {
    min_level = Option.value min_level ~default:c.min_level;
    format = Option.value format ~default:c.format;
    output = Option.value output ~default:c.output;
  }

(** Helper functions *)
let debug ?(ctx=[]) ?trace_id fmt = 
  Printf.ksprintf (fun msg -> emit Debug msg ctx trace_id) fmt

let info ?(ctx=[]) ?trace_id fmt = 
  Printf.ksprintf (fun msg -> emit Info msg ctx trace_id) fmt

let warn ?(ctx=[]) ?trace_id fmt = 
  Printf.ksprintf (fun msg -> emit Warn msg ctx trace_id) fmt

let error ?(ctx=[]) ?trace_id fmt = 
  Printf.ksprintf (fun msg -> emit Error msg ctx trace_id) fmt

(** Request logging middleware *)
let middleware next req = 
  let start = Unix.gettimeofday () in
  let trace_id = match Request.header "x-request-id" req with 
    | Some id -> id
    | None -> 
      (* Generate new trace ID if missing *)
      let id = string_of_float start |> Digestif.SHA256.digest_string |> Digestif.SHA256.to_hex in
      let id = String.sub id 0 8 in
      id 
  in
  
  let method_ = Request.meth req |> Cohttp.Code.string_of_method in
  let path = Request.path req in
  
  info ~ctx:[("method", `String method_); ("path", `String path)] ~trace_id "Request started";
  
  let resp = next req in
  
  let duration = (Unix.gettimeofday () -. start) *. 1000.0 in
  let status = Response.status_code resp in
  
  info ~ctx:[
    ("method", `String method_); 
    ("path", `String path);
    ("status", `Int status);
    ("duration_ms", `Float duration)
  ] ~trace_id "Request completed";
  
  resp
