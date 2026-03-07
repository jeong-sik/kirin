(** WebRTC type definitions and default configurations *)

(** ICE connection state *)
type ice_state =
  | New
  | Checking
  | Connected
  | Completed
  | Failed
  | Disconnected
  | Closed

(** ICE candidate *)
type ice_candidate = {
  candidate : string;
  sdp_mid : string option;
  sdp_mline_index : int option;
  ufrag : string option;
}

(** SDP type *)
type sdp_type = Offer | Answer | Pranswer | Rollback

(** Session Description *)
type session_description = {
  sdp_type : sdp_type;
  sdp : string;
}

(** Data channel state *)
type datachannel_state =
  | Connecting
  | Open
  | Closing
  | DCClosed

(** Data channel options *)
type datachannel_options = {
  ordered : bool;
  max_packet_life_time : int option;
  max_retransmits : int option;
  protocol : string;
  negotiated : bool;
  id : int option;
}

let default_datachannel_options = {
  ordered = true;
  max_packet_life_time = None;
  max_retransmits = None;
  protocol = "";
  negotiated = false;
  id = None;
}

(** STUN server configuration *)
type stun_server = {
  urls : string list;
  username : string option;
  credential : string option;
}

(** ICE server configuration *)
type ice_servers = stun_server list

let default_ice_servers = [
  { urls = ["stun:stun.l.google.com:19302"]; username = None; credential = None };
]
