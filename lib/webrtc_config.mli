(** WebRTC type definitions and default configurations.

    Defines the core types shared across the WebRTC adapter:
    ICE states, candidates, SDP, data channel options, and STUN servers. *)

(** {1 ICE State} *)

(** ICE connection state per RFC 8445. *)
type ice_state =
  | New
  | Checking
  | Connected
  | Completed
  | Failed
  | Disconnected
  | Closed

(** {1 ICE Candidate} *)

(** ICE candidate describing a potential connection endpoint. *)
type ice_candidate = {
  candidate : string;
  sdp_mid : string option;
  sdp_mline_index : int option;
  ufrag : string option;
}

(** {1 Session Description} *)

(** SDP type per RFC 3264. *)
type sdp_type = Offer | Answer | Pranswer | Rollback

(** Session description containing SDP type and body. *)
type session_description = {
  sdp_type : sdp_type;
  sdp : string;
}

(** {1 Data Channel} *)

(** Data channel state. *)
type datachannel_state =
  | Connecting
  | Open
  | Closing
  | DCClosed

(** Data channel creation options. *)
type datachannel_options = {
  ordered : bool;
  max_packet_life_time : int option;
  max_retransmits : int option;
  protocol : string;
  negotiated : bool;
  id : int option;
}

(** Default data channel options: ordered, no lifetime/retransmit limits,
    empty protocol, not pre-negotiated, no explicit id. *)
val default_datachannel_options : datachannel_options

(** {1 STUN/TURN Servers} *)

(** STUN/TURN server configuration. *)
type stun_server = {
  urls : string list;
  username : string option;
  credential : string option;
}

(** List of ICE servers (STUN/TURN). *)
type ice_servers = stun_server list

(** Default ICE servers (Google public STUN). *)
val default_ice_servers : ice_servers
