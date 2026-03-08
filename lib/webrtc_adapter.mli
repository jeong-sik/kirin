(** Kirin WebRTC Adapter.

    Provides WebRTC peer-to-peer communication with Kirin-style APIs:
    data channels, signaling server via WebSocket, ICE candidate trickle,
    and STUN/TURN integration.

    Currently a mock implementation. Phase 2 will replace internals
    with real ocaml-webrtc bindings while preserving this interface. *)

(** {1 Re-exported Types}

    All types from {!Webrtc_config} are included here. *)

include module type of Webrtc_config

(** {1 Signaling}

    Re-export of {!Webrtc_signaling} for signaling server operations. *)

module Signaling : module type of Webrtc_signaling

(** {1 DataChannel} *)

module DataChannel : sig
  (** Data channel handle.

      Record fields are exposed for the current mock implementation.
      Phase 2 may make this type opaque. *)
  type t = {
    label : string;
    mutable state : datachannel_state;
    options : datachannel_options;
    mutable on_open : (unit -> unit) option;
    mutable on_message : (string -> unit) option;
    mutable on_close : (unit -> unit) option;
    mutable on_error : (string -> unit) option;
    mutex : Eio.Mutex.t;
  }

  (** [create ~label ?options ()] creates a new data channel with the given label. *)
  val create : label:string -> ?options:datachannel_options -> unit -> t

  (** [label t] returns the channel label. *)
  val label : t -> string

  (** [state t] returns the current channel state. *)
  val state : t -> datachannel_state

  (** [is_open t] returns [true] if the channel is in the [Open] state. *)
  val is_open : t -> bool

  (** [on_open t callback] registers a callback for the channel open event. *)
  val on_open : t -> (unit -> unit) -> unit

  (** [on_message t callback] registers a callback for incoming messages. *)
  val on_message : t -> (string -> unit) -> unit

  (** [on_close t callback] registers a callback for the channel close event. *)
  val on_close : t -> (unit -> unit) -> unit

  (** [on_error t callback] registers a callback for channel errors. *)
  val on_error : t -> (string -> unit) -> unit

  (** [send t data] sends a string message on the channel.
      Returns [Error] if the channel is not open. *)
  val send : t -> string -> (unit, string) result

  (** [send_binary t data] sends binary data on the channel.
      Returns [Error] if the channel is not open. *)
  val send_binary : t -> string -> (unit, string) result

  (** [close t] closes the data channel. *)
  val close : t -> unit
end

(** {1 PeerConnection} *)

module PeerConnection : sig
  (** Peer connection handle.

      Record fields are exposed for the current mock implementation.
      Phase 2 may make this type opaque. *)
  type t = {
    ice_servers : ice_servers;
    mutable ice_state : ice_state;
    mutable local_description : session_description option;
    mutable remote_description : session_description option;
    mutable data_channels : DataChannel.t list;
    mutable ice_candidates : ice_candidate list;
    mutable on_ice_candidate : (ice_candidate -> unit) option;
    mutable on_ice_state_change : (ice_state -> unit) option;
    mutable on_data_channel : (DataChannel.t -> unit) option;
    mutex : Eio.Mutex.t;
    mutable ice_ufrag : string;
    mutable ice_pwd : string;
  }

  (** [create ?ice_servers ()] creates a new peer connection. *)
  val create : ?ice_servers:ice_servers -> unit -> t

  (** [ice_state t] returns the current ICE connection state. *)
  val ice_state : t -> ice_state

  (** [local_description t] returns the local session description, if set. *)
  val local_description : t -> session_description option

  (** [remote_description t] returns the remote session description, if set. *)
  val remote_description : t -> session_description option

  (** [on_ice_candidate t callback] registers a callback for new ICE candidates. *)
  val on_ice_candidate : t -> (ice_candidate -> unit) -> unit

  (** [on_ice_state_change t callback] registers a callback for ICE state changes. *)
  val on_ice_state_change : t -> (ice_state -> unit) -> unit

  (** [on_data_channel t callback] registers a callback for incoming data channels. *)
  val on_data_channel : t -> (DataChannel.t -> unit) -> unit

  (** [create_data_channel t ~label ?options ()] creates a data channel on the connection. *)
  val create_data_channel : t -> label:string -> ?options:datachannel_options -> unit -> DataChannel.t

  (** [create_offer t] generates an SDP offer using the ocaml-webrtc SDP module. *)
  val create_offer : t -> session_description

  (** [create_answer t] generates an SDP answer from the remote description.
      Returns [Error] if no remote description is set or SDP parsing fails. *)
  val create_answer : t -> (session_description, string) result

  (** [set_local_description t desc] sets the local session description. *)
  val set_local_description : t -> session_description -> (unit, string) result

  (** [set_remote_description t desc] sets the remote session description.
      Returns [Error] if the SDP is invalid. *)
  val set_remote_description : t -> session_description -> (unit, string) result

  (** [add_ice_candidate t candidate] adds a remote ICE candidate. *)
  val add_ice_candidate : t -> ice_candidate -> (unit, string) result

  (** [close t] closes the peer connection and all its data channels. *)
  val close : t -> unit
end

(** {1 HTTP/WebSocket Handlers} *)

(** [signaling_handler ()] creates a WebSocket handler for WebRTC signaling.
    Clients connect with a [?room=<id>] query parameter.
    Returns a handler compatible with Kirin routing. *)
val signaling_handler : unit -> (Request.t -> Response.t)

(** [stun_servers_handler req] returns the configured ICE/STUN servers as JSON. *)
val stun_servers_handler : Request.t -> Response.t

(** {1 Routes Helper} *)

(** [routes ()] returns the default WebRTC routes:
    - [GET /webrtc/signaling] -- signaling WebSocket endpoint
    - [GET /webrtc/stun-servers] -- STUN server info endpoint *)
val routes : unit -> Router.route list
