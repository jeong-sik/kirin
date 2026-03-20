type level = Development | Production | Enterprise
type tool_permission = Allowed | Denied of string | Requires_approval
type config = {
  level : level;
  allowed_tools : string list option;
  denied_tools : string list;
  max_concurrent_tools : int;
  audit_enabled : bool;
  require_session : bool;
}
type audit_event = {
  timestamp : float;
  event_type : string;
  tool_name : string option;
  session_id : string option;
  details : Yojson.Safe.t option;
  outcome : string;
}
type audit_log = {
  events : audit_event Queue.t;
  max_events : int;
  mutex : Eio.Mutex.t;
}
val default_development : unit -> config
val default_production : unit -> config
val default_enterprise : unit -> config
val create_audit_log : ?max_events:int -> unit -> audit_log
val check_tool_permission : config -> tool_name:string -> tool_permission
val record_event : audit_log -> audit_event -> unit
val tool_call_event :
  ?session_id:string ->
  tool_name:string ->
  outcome:string -> ?details:Yojson.Safe.t -> unit -> audit_event
val recent_events : audit_log -> int -> audit_event list
val audit_event_to_json :
  audit_event -> [> `Assoc of (string * Yojson.Safe.t) list ]
val audit_log_to_json :
  audit_log ->
  [> `Assoc of
       (string *
        [> `Int of int
         | `List of [> `Assoc of (string * Yojson.Safe.t) list ] list ])
       list ]
val level_to_string : level -> string
val level_of_string : string -> (level, string) result
val config_to_json :
  config ->
  [> `Assoc of
       (string *
        [> `Bool of bool
         | `Int of int
         | `List of [> `String of string ] list
         | `Null
         | `String of string ])
       list ]
val config_of_json : Yojson__Safe.t -> config
