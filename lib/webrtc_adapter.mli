(** Kirin WebRTC Adapter.

    Thin wrapper around ocaml-webrtc providing Kirin-style APIs for
    WebRTC peer-to-peer communication: signaling, ICE, DataChannels,
    and STUN/TURN integration.

    Protocol modules ({!Sdp}, {!Ice}, {!Dcep}, etc.) are re-exported
    from ocaml-webrtc. The {!Peer} module provides the full Eio-based
    WebRTC stack. Kirin helpers bridge Kirin types with protocol types.

    @since 1.0.0
    @status experimental
    This module's API may change in future versions. *)

(** {1 Re-exported Types}

    All types from {!Webrtc_config} are included here. *)

include module type of Webrtc_config

(** {1 Signaling}

    Re-export of {!Webrtc_signaling} for signaling server operations. *)

module Signaling : module type of Webrtc_signaling

(** {1 Protocol Module Re-exports}

    Direct access to ocaml-webrtc protocol stack modules. *)

module Sdp = Webrtc.Sdp
module Ice = Webrtc.Ice
module Stun = Webrtc.Stun
module Turn = Webrtc.Turn
module Dcep = Webrtc.Dcep

(** {1 Full WebRTC Stack}

    Eio-based full WebRTC stack integrating ICE, DTLS, SCTP,
    and DataChannels.
    See {!Webrtc.Webrtc_eio} for the full API. *)

module Peer = Webrtc.Webrtc_eio

(** {1 Kirin-Style Helpers} *)

(** [create_peer ?ice_servers ~role ()] creates a WebRTC peer.
    Converts Kirin {!stun_server} list to protocol ICE configuration. *)
val create_peer : ?ice_servers:ice_servers -> role:Peer.role -> unit -> Peer.t

(** [create_datachannel peer ~label] creates a DataChannel on a peer. *)
val create_datachannel : Peer.t -> label:string -> Peer.datachannel

(** [send_datachannel peer channel data] sends string data on a channel.
    Returns [Ok bytes_sent] or [Error msg]. *)
val send_datachannel : Peer.t -> Peer.datachannel -> string -> (int, string) result

(** [create_offer peer] generates an SDP offer for signaling.
    Uses the peer's ICE credentials with a {b placeholder} DTLS fingerprint
    (all zeros). Not suitable for real DTLS negotiation — use {!Peer.connect}
    for production WebRTC connections. *)
val create_offer : Peer.t -> session_description

(** [create_answer peer ~remote_sdp] generates an SDP answer.
    Uses a {b placeholder} DTLS fingerprint. See {!create_offer} caveat.
    Returns [Error] if the remote SDP cannot be parsed. *)
val create_answer : Peer.t -> remote_sdp:string -> (session_description, string) result

(** {1 HTTP/WebSocket Handlers} *)

(** [signaling_handler ()] creates a WebSocket handler for WebRTC signaling.
    Clients connect with a [?room=<id>] query parameter. *)
val signaling_handler : unit -> Request.t -> Response.t

(** [stun_servers_handler req] returns the configured ICE/STUN servers as JSON. *)
val stun_servers_handler : Request.t -> Response.t

(** {1 Routes Helper} *)

(** [routes ()] returns the default WebRTC routes:
    - [GET /webrtc/signaling] -- signaling WebSocket endpoint
    - [GET /webrtc/stun-servers] -- STUN server info endpoint *)
val routes : unit -> Router.route list
