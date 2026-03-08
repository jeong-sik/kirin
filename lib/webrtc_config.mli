(** WebRTC type definitions and default configurations.

    Types are bridged between Kirin's signaling layer and the ocaml-webrtc
    protocol stack. Peer-level connection state is re-exported from
    {!Webrtc.Webrtc_eio} to ensure type equality with {!Peer.get_state}. *)

(** {1 Connection State}

    Re-exported from {!Webrtc.Webrtc_eio.connection_state}.
    For ICE-layer states, use [Webrtc.Ice.connection_state] directly. *)

type connection_state = Webrtc.Webrtc_eio.connection_state =
  | New
  | Connecting
  | Connected
  | Disconnected
  | Failed
  | Closed

(** {1 Signaling Types} *)

(** ICE candidate for signaling (lightweight, string-based). *)
type ice_candidate = {
  candidate : string;
  sdp_mid : string option;
  sdp_mline_index : int option;
  ufrag : string option;
}

(** SDP type per RFC 3264. *)
type sdp_type = Offer | Answer | Pranswer | Rollback

(** Session description containing SDP type and body. *)
type session_description = {
  sdp_type : sdp_type;
  sdp : string;
}

(** {1 ICE Server Configuration} *)

(** STUN/TURN server configuration for signaling. *)
type stun_server = {
  urls : string list;
  username : string option;
  credential : string option;
}

(** List of ICE servers (STUN/TURN). *)
type ice_servers = stun_server list

(** Default ICE servers (Google public STUN). *)
val default_ice_servers : ice_servers

(** {1 Type Conversions}

    Bridge between Kirin signaling types and ocaml-webrtc protocol types. *)

(** Parse a signaling candidate string into a structured SDP candidate. *)
val sdp_candidate_of_signaling :
  ice_candidate -> (Webrtc.Sdp.ice_candidate, string) result

(** Convert a structured SDP candidate to a signaling candidate. *)
val signaling_of_sdp_candidate :
  ?sdp_mid:string ->
  ?sdp_mline_index:int ->
  ?ufrag:string ->
  Webrtc.Sdp.ice_candidate -> ice_candidate

(** Convert a Kirin STUN server to an ocaml-webrtc ICE server. *)
val ice_server_of_stun : stun_server -> Webrtc.Ice.ice_server

(** Convert an ocaml-webrtc ICE server to a Kirin STUN server. *)
val stun_of_ice_server : Webrtc.Ice.ice_server -> stun_server
