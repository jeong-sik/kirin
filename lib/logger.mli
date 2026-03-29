(** Kirin Logger - Async Structured Logging (Domain-safe)

    @since 1.0.0
    @status stable

    Architecture:
    - Any domain can emit logs (no Eio effect handlers required).
    - A dedicated logger domain pops entries from a bounded queue and writes to I/O.
    - This removes I/O latency from request/worker domains. *)

(** {1 Types} *)

(** Log levels. *)
type level =
  | Debug
  | Info
  | Warn
  | Error
  | Fatal

(** Log entry structure. *)
type entry =
  { timestamp : float
  ; level : level
  ; message : string
  ; context : (string * Yojson.Safe.t) list
  ; trace_id : string option
  ; span_id : string option
  }

(** Logger configuration. *)
type config =
  { min_level : level
  ; format : [ `Json | `Text ]
  ; output : out_channel
  }

(** {1 Level Conversion} *)

(** [string_of_level level] returns a human-readable string for the log level. *)
val string_of_level : level -> string

(** {1 Lifecycle} *)

(** [start sw] starts the background logger domain.
    The logger is automatically shut down when [sw] is released.
    Calling [start] when the logger is already running is a no-op. *)
val start : Eio.Switch.t -> unit

(** [shutdown ()] stops the background logger and flushes pending entries.
    Blocks until the logger domain has joined. *)
val shutdown : unit -> unit

(** {1 Configuration} *)

(** [configure ?min_level ?format ?output ()] updates the logger configuration.
    Only specified fields are changed; others retain current values. *)
val configure
  :  ?min_level:level
  -> ?format:[ `Json | `Text ]
  -> ?output:out_channel
  -> unit
  -> unit

(** {1 Logging Functions} *)

(** [emit level message context trace_id] enqueues a log entry.
    If the logger is not running or the queue is full,
    the entry is written synchronously as a fallback. *)
val emit : level -> string -> (string * Yojson.Safe.t) list -> string option -> unit

(** [debug ?ctx ?trace_id fmt ...] logs at Debug level. *)
val debug
  :  ?ctx:(string * Yojson.Safe.t) list
  -> ?trace_id:string
  -> ('a, unit, string, unit) format4
  -> 'a

(** [info ?ctx ?trace_id fmt ...] logs at Info level. *)
val info
  :  ?ctx:(string * Yojson.Safe.t) list
  -> ?trace_id:string
  -> ('a, unit, string, unit) format4
  -> 'a

(** [warn ?ctx ?trace_id fmt ...] logs at Warn level. *)
val warn
  :  ?ctx:(string * Yojson.Safe.t) list
  -> ?trace_id:string
  -> ('a, unit, string, unit) format4
  -> 'a

(** [error ?ctx ?trace_id fmt ...] logs at Error level. *)
val error
  :  ?ctx:(string * Yojson.Safe.t) list
  -> ?trace_id:string
  -> ('a, unit, string, unit) format4
  -> 'a

(** {1 Middleware} *)

(** [middleware] logs request start/completion with method, path, status, and
    duration. Adds a trace ID from the [x-request-id] header or generates one. *)
val middleware : (Request.t -> Response.t) -> Request.t -> Response.t
