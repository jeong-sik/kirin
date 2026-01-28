(** Kirin Logger - Async Structured Logging (Lock-Freeish Architecture)

    Architecture:
    - Each domain pushes logs to a shared thread-safe stream (Eio.Stream).
    - A dedicated 'Logger Domain' pops from the stream and writes to I/O.
    - This removes I/O latency and lock contention from worker domains.
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

(** Shared log queue (capacity 4096 to absorb bursts) *)
let log_stream = Eio.Stream.create 4096

(** Format timestamp ISO8601 *)
let format_time ts = 
  let tm = Unix.gmtime ts in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d.%03dZ"
    (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
    tm.tm_hour tm.tm_min tm.tm_sec (int_of_float ((ts -. floor ts) *. 1000.))

(** The actual writer loop (runs in dedicated fiber/domain) *)
let rec run_logger () = 
  try
    let entry = Eio.Stream.take log_stream in
    let json_fields = [
      ("timestamp", `String (format_time entry.timestamp));
      ("level", `String (string_of_level entry.level));
      ("message", `String entry.message);
    ] in
    let json_fields = match entry.trace_id with 
      | Some tid -> ("trace_id", `String tid) :: json_fields
      | None -> json_fields
    in
    let json_fields = if entry.context <> [] then 
      ("context", `Assoc entry.context) :: json_fields 
    else json_fields in
    
    let json = `Assoc json_fields in
    
    (* I/O operation - safe here as we are in dedicated loop *)
    Yojson.Safe.to_channel !current_config.output json;
    output_string !current_config.output "\n";
    flush !current_config.output;
    
    run_logger () 
  with _ -> run_logger () (* Resilience: never crash the logger *)

(** Start the logger background fiber *)
let start sw =
  Eio.Fiber.fork ~sw run_logger

(** Shutdown logger gracefully *)
let shutdown () =
  (* Send poison pill or just wait? Eio streams don't have close.
     For now, we just flush stderr to ensure OS buffers are empty. *)
  flush !current_config.output
(** Emit log entry (Blocking if full) *)
let emit level message context trace_id =
  if level >= !current_config.min_level then
    let ts = Unix.gettimeofday () in
    let entry = { timestamp=ts; level; message; context; trace_id; span_id=None } in
    (* Warning: This will block if queue is full. 
       In v0.4.0, implement ring buffer or drop strategy. *)
    Eio.Stream.add log_stream entry
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