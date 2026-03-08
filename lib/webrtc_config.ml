(** WebRTC type definitions and default configurations.

    Types are bridged between Kirin's signaling layer and the ocaml-webrtc
    protocol stack. The peer-level connection state is re-exported from
    {!Webrtc.Webrtc_eio} to ensure type equality with {!Peer.get_state}.
    Signaling-specific types (lightweight candidates, SDP descriptions)
    remain Kirin-defined.

    For ICE-layer states (RFC 8445), use {!Webrtc.Ice.connection_state}
    directly.

    Conversion functions at the bottom bridge between the two type systems. *)

(** {1 Connection State}

    Re-exported from {!Webrtc.Webrtc_eio.connection_state}.
    This is the peer-level state returned by {!Peer.get_state}.
    Kirin code can pattern-match directly (e.g. [New], [Connected]).

    For ICE-layer states (7 variants including [Checking] and [Completed]),
    use [Webrtc.Ice.connection_state] instead. *)

type connection_state = Webrtc.Webrtc_eio.connection_state =
  | New
  | Connecting
  | Connected
  | Disconnected
  | Failed
  | Closed

(** {1 Signaling Types}

    These types are used for WebSocket signaling (JSON exchange) and are
    intentionally lightweight. See {!sdp_candidate_of_signaling} to convert
    to structured ocaml-webrtc types. *)

(** ICE candidate for signaling. The [candidate] field contains the SDP
    candidate attribute string (e.g. ["candidate:... typ host ..."]). *)
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

(** {1 ICE Server Configuration} *)

(** STUN/TURN server configuration for signaling.
    See {!ice_server_of_stun} to convert to ocaml-webrtc format. *)
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

(** {1 Type Conversions}

    Bridge between Kirin signaling types and ocaml-webrtc protocol types. *)

(** Parse a signaling candidate string into a structured SDP candidate.
    The [candidate] field of {!ice_candidate} is parsed via
    {!Webrtc.Sdp.parse_candidate}. *)
let sdp_candidate_of_signaling (c : ice_candidate) =
  Webrtc.Sdp.parse_candidate c.candidate

(** Convert a structured SDP candidate back to a signaling candidate.
    Optionally pass [sdp_mid], [sdp_mline_index], and [ufrag] to preserve
    signaling metadata that is not part of the candidate string. *)
let signaling_of_sdp_candidate ?sdp_mid ?sdp_mline_index ?ufrag
    (c : Webrtc.Sdp.ice_candidate) =
  { candidate = Webrtc.Sdp.candidate_to_string c
  ; sdp_mid
  ; sdp_mline_index
  ; ufrag
  }

(** Convert a Kirin STUN server config to an ocaml-webrtc ICE server config.
    Sets [tls_ca] to [None]. *)
let ice_server_of_stun (s : stun_server) : Webrtc.Ice.ice_server =
  { Webrtc.Ice.urls = s.urls
  ; username = s.username
  ; credential = s.credential
  ; tls_ca = None
  }

(** Convert an ocaml-webrtc ICE server config to a Kirin STUN server config.
    Drops the [tls_ca] field. *)
let stun_of_ice_server (s : Webrtc.Ice.ice_server) : stun_server =
  { urls = s.urls
  ; username = s.username
  ; credential = s.credential
  }
