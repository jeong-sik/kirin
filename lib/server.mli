(** Server module - Eio-based HTTP server with multicore support.

    @since 1.0.0
    @status stable *)

(** Server configuration. *)
type config = {
  port : int;
  host : string;
  backlog : int;
  request_timeout : float;
  stream_read_timeout : float;
}

(** Default server configuration (port 8000, 30s request timeout). *)
val default_config : config

(** [run ?config ~sw ~env handler] runs the server within an existing Eio switch.
    Use this when you need control over the Eio environment. *)
val run :
  ?config:config ->
  sw:Eio.Switch.t ->
  env:< net : _ Eio.Net.t ; clock : _ Eio.Time.clock ; .. > ->
  Router.handler ->
  unit

(** [start ?port ?request_timeout ?stream_read_timeout ?domains handler]
    is the main entry point. Sets up Eio, multicore domains, and starts serving.
    Blocks until the server shuts down. *)
val start :
  ?port:int ->
  ?request_timeout:float ->
  ?stream_read_timeout:float ->
  ?domains:int ->
  Router.handler ->
  unit
