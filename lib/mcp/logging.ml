(** Kirin MCP - Logging Capability

    MCP logging levels and message emission.
    Supports the logging/setLevel method and
    notifications/message notification. *)

(** MCP log levels (ordered by severity) *)
type log_level =
  | Debug
  | Info
  | Notice
  | Warning
  | Error
  | Critical
  | Alert
  | Emergency

(** Log message *)
type log_message = {
  level : log_level;
  logger : string option;    (** logger name *)
  data : Yojson.Safe.t;      (** structured log data *)
}

(** Logging state *)
type t = {
  mutable current_level : log_level;
  mutable handler : (log_message -> unit) option;  (** callback for log emission *)
}

(* {1 Level helpers} *)

let level_to_int = function
  | Debug     -> 0
  | Info      -> 1
  | Notice    -> 2
  | Warning   -> 3
  | Error     -> 4
  | Critical  -> 5
  | Alert     -> 6
  | Emergency -> 7

let log_level_to_string = function
  | Debug     -> "debug"
  | Info      -> "info"
  | Notice    -> "notice"
  | Warning   -> "warning"
  | Error     -> "error"
  | Critical  -> "critical"
  | Alert     -> "alert"
  | Emergency -> "emergency"

let log_level_of_string = function
  | "debug"     -> Ok Debug
  | "info"      -> Ok Info
  | "notice"    -> Ok Notice
  | "warning"   -> Ok Warning
  | "error"     -> Ok Error
  | "critical"  -> Ok Critical
  | "alert"     -> Ok Alert
  | "emergency" -> Ok Emergency
  | s -> Error (Printf.sprintf "unknown log level: %S" s)

(* {1 Constructor} *)

let create ?(level = Warning) ?handler () =
  { current_level = level; handler }

(* {1 Level Management} *)

let set_level t level =
  t.current_level <- level

let current_level t = t.current_level

let should_log t level =
  level_to_int level >= level_to_int t.current_level

(* {1 Log Emission} *)

let log t ~level ?logger data =
  if should_log t level then
    match t.handler with
    | Some handler ->
      let msg = { level; logger; data } in
      handler msg
    | None -> ()

let debug t ?logger data = log t ~level:Debug ?logger data
let info t ?logger data = log t ~level:Info ?logger data
let warning t ?logger data = log t ~level:Warning ?logger data
let error t ?logger data = log t ~level:Error ?logger data

(* {1 JSON Encoding} *)

let log_message_to_json msg =
  let fields = [
    "level", `String (log_level_to_string msg.level);
    "data", msg.data;
  ] in
  let fields = match msg.logger with
    | Some name -> ("logger", `String name) :: fields
    | None -> fields
  in
  `Assoc fields
