(** Kirin DB Events - PostgreSQL LISTEN/NOTIFY Integration

    Turns PostgreSQL into a realtime message bus.

    {b Features:}
    - Asynchronous LISTEN loop
    - JSON payload support
    - Automatic reconnection
    - Integration with Eio streams *)

(** {1 Types} *)

(** Notification type. *)
type notification = {
  channel : string;
  payload : string;
  process_id : int;
}

(** Connection configuration. *)
type config = {
  uri : string;
  channels : string list;
  reconnect_delay : float;
}

(** [default_config uri] creates a default configuration for the given URI.
    Default channel is ["kirin_events"], reconnect delay is 1.0s. *)
val default_config : string -> config

(** Database driver interface for LISTEN/NOTIFY. *)
module type DB_DRIVER = sig
  type connection
  val connect : Uri.t -> (connection, [> Caqti_error.connect]) result
  val listen : connection -> string list -> (unit, [> Caqti_error.call_or_retrieve]) result
  val wait_for_notification : connection -> (notification option, [> Caqti_error.call_or_retrieve]) result
  val disconnect : connection -> unit
end

(** Event stream handle. *)
type t = {
  stream : notification Eio.Stream.t;
  sw : Eio.Switch.t;
}

(** {1 Lifecycle} *)

(** [create ~sw ~env ~config] creates and starts a listener.
    The listener runs as an Eio fiber under [sw]. *)
val create :
  sw:Eio.Switch.t ->
  env:< clock : float Eio.Time.clock_ty Eio.Resource.t; .. > ->
  config:config ->
  t

(** [subscribe t] returns the notification stream for consumption. *)
val subscribe : t -> notification Eio.Stream.t

(** {1 Publishing} *)

(** [notify pool channel payload] publishes a NOTIFY on the given channel.
    Uses pg_notify() via the connection pool. *)
val notify :
  Db.pool ->
  string ->
  string ->
  (unit, Db.error) result
