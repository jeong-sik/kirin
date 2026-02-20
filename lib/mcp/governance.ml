(** Kirin MCP - Governance Layer

    Controls tool execution permissions, rate limiting,
    and audit logging for MCP servers. *)

(** Governance level *)
type level =
  | Development   (** All tools allowed, no audit *)
  | Production    (** Configured tools only, audit enabled *)
  | Enterprise    (** Strict allowlist, full audit trail *)

(** Tool permission *)
type tool_permission =
  | Allowed
  | Denied of string   (** reason *)
  | Requires_approval  (** human-in-the-loop *)

(** Governance configuration *)
type config = {
  level : level;
  allowed_tools : string list option;     (** None = all allowed *)
  denied_tools : string list;             (** Always denied *)
  max_concurrent_tools : int;             (** Rate limiting *)
  audit_enabled : bool;
  require_session : bool;                 (** Require valid session for tool calls *)
}

(** Audit event *)
type audit_event = {
  timestamp : float;
  event_type : string;          (** "tool_call", "tool_result", "access_denied", etc. *)
  tool_name : string option;
  session_id : string option;
  details : Yojson.Safe.t option;
  outcome : string;             (** "success", "error", "denied" *)
}

(** Audit log (in-memory ring buffer) *)
type audit_log = {
  events : audit_event Queue.t;
  max_events : int;
}

(* {1 Constructors} *)

let default_development () =
  {
    level = Development;
    allowed_tools = None;
    denied_tools = [];
    max_concurrent_tools = 32;
    audit_enabled = false;
    require_session = false;
  }

let default_production () =
  {
    level = Production;
    allowed_tools = None;
    denied_tools = [];
    max_concurrent_tools = 8;
    audit_enabled = true;
    require_session = true;
  }

let default_enterprise () =
  {
    level = Enterprise;
    allowed_tools = Some [];
    denied_tools = [];
    max_concurrent_tools = 4;
    audit_enabled = true;
    require_session = true;
  }

let create_audit_log ?(max_events = 1000) () =
  { events = Queue.create (); max_events }

(* {1 Permission Checks} *)

let check_tool_permission config ~tool_name =
  if List.mem tool_name config.denied_tools then
    Denied (Printf.sprintf "tool %S is in the deny list" tool_name)
  else
    match config.allowed_tools with
    | Some allowed ->
      if List.mem tool_name allowed then Allowed
      else
        (match config.level with
         | Enterprise -> Requires_approval
         | _ -> Denied (Printf.sprintf "tool %S is not in the allow list" tool_name))
    | None -> Allowed

(* {1 Audit} *)

let record_event log event =
  if Queue.length log.events >= log.max_events then
    ignore (Queue.pop log.events);
  Queue.push event log.events

let tool_call_event ?session_id ~tool_name ~outcome ?details () =
  {
    timestamp = Unix.gettimeofday ();
    event_type = "tool_call";
    tool_name = Some tool_name;
    session_id;
    details;
    outcome;
  }

let recent_events log n =
  let all = Queue.to_seq log.events |> List.of_seq in
  let len = List.length all in
  if n >= len then all
  else
    let drop = len - n in
    let rec skip lst k =
      if k <= 0 then lst
      else match lst with
        | [] -> []
        | _ :: rest -> skip rest (k - 1)
    in
    skip all drop

let audit_event_to_json event =
  let fields = [
    "timestamp", `Float event.timestamp;
    "event_type", `String event.event_type;
    "outcome", `String event.outcome;
  ] in
  let fields = match event.tool_name with
    | Some n -> ("tool_name", `String n) :: fields
    | None -> fields
  in
  let fields = match event.session_id with
    | Some s -> ("session_id", `String s) :: fields
    | None -> fields
  in
  let fields = match event.details with
    | Some d -> ("details", d) :: fields
    | None -> fields
  in
  `Assoc fields

let audit_log_to_json log =
  let events = Queue.to_seq log.events
    |> Seq.map audit_event_to_json
    |> List.of_seq
  in
  `Assoc [
    "max_events", `Int log.max_events;
    "count", `Int (Queue.length log.events);
    "events", `List events;
  ]

(* {1 JSON Encoding} *)

let level_to_string = function
  | Development -> "development"
  | Production -> "production"
  | Enterprise -> "enterprise"

let level_of_string = function
  | "development" -> Ok Development
  | "production" -> Ok Production
  | "enterprise" -> Ok Enterprise
  | s -> Error (Printf.sprintf "unknown governance level: %S" s)

let config_to_json config =
  let allowed = match config.allowed_tools with
    | None -> `Null
    | Some tools -> `List (List.map (fun s -> `String s) tools)
  in
  `Assoc [
    "level", `String (level_to_string config.level);
    "allowed_tools", allowed;
    "denied_tools", `List (List.map (fun s -> `String s) config.denied_tools);
    "max_concurrent_tools", `Int config.max_concurrent_tools;
    "audit_enabled", `Bool config.audit_enabled;
    "require_session", `Bool config.require_session;
  ]

let config_of_json json =
  let open Yojson.Safe.Util in
  let level_str = json |> member "level" |> to_string in
  let level = match level_of_string level_str with
    | Ok l -> l
    | Error msg -> failwith msg
  in
  let allowed_tools =
    match json |> member "allowed_tools" with
    | `Null -> None
    | `List items -> Some (List.map to_string items)
    | _ -> failwith "allowed_tools must be null or array"
  in
  let denied_tools =
    json |> member "denied_tools" |> to_list |> List.map to_string
  in
  {
    level;
    allowed_tools;
    denied_tools;
    max_concurrent_tools = json |> member "max_concurrent_tools" |> to_int;
    audit_enabled = json |> member "audit_enabled" |> to_bool;
    require_session = json |> member "require_session" |> to_bool;
  }
