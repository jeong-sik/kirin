type log_level =
  | Debug
  | Info
  | Notice
  | Warning
  | Error
  | Critical
  | Alert
  | Emergency

type log_message =
  { level : log_level
  ; logger : string option
  ; data : Yojson.Safe.t
  }

type t =
  { mutable current_level : log_level
  ; mutable handler : (log_message -> unit) option
  }

val level_to_int : log_level -> int
val log_level_to_string : log_level -> string
val log_level_of_string : string -> (log_level, string) result
val create : ?level:log_level -> ?handler:(log_message -> unit) -> unit -> t
val set_level : t -> log_level -> unit
val current_level : t -> log_level
val should_log : t -> log_level -> bool
val log : t -> level:log_level -> ?logger:string -> Yojson.Safe.t -> unit
val debug : t -> ?logger:string -> Yojson.Safe.t -> unit
val info : t -> ?logger:string -> Yojson.Safe.t -> unit
val warning : t -> ?logger:string -> Yojson.Safe.t -> unit
val error : t -> ?logger:string -> Yojson.Safe.t -> unit
val log_message_to_json : log_message -> [> `Assoc of (string * Yojson.Safe.t) list ]
