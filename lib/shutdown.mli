(** Graceful shutdown with signal handling and connection draining.

    @since 1.0.0
    @status stable *)

(** {1 Types} *)

type state =
  | Running
  | ShuttingDown
  | Stopped

type config =
  { timeout : float
  ; force_after : float
  }

type hook = unit -> unit
type t

(** {1 Configuration} *)

val default_config : config

(** {1 Creation} *)

val create : ?timeout:float -> ?force_after:float -> unit -> t

(** {1 State Management} *)

val state : t -> state
val is_running : t -> bool
val is_shutting_down : t -> bool
val is_stopped : t -> bool

(** {1 Connection Tracking} *)

val connection_start : t -> bool
val connection_end : t -> unit
val active_connections : t -> int

(** {1 Hooks} *)

val on_shutdown : t -> hook -> unit

(** {1 Shutdown Process} *)

val initiate : t -> unit
val setup_signals : t -> unit

(** {1 Server Integration} *)

val run : t -> (unit -> unit) -> unit
val middleware : t -> ('a -> Response.t) -> 'a -> Response.t

(** {1 Status} *)

val status_json : t -> [> `Assoc of (string * [> `Int of int | `String of string ]) list ]
