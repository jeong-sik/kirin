(** Kirin WebRTC Adapter (v1.0.0)

    Thin wrapper around ocaml-webrtc, following the grpc.ml pattern.
    Protocol modules are re-exported directly; Kirin helpers bridge types.

    {b Example - Signaling Server:}
    {[
      let () = Kirin.start ~port:8080
        @@ Kirin.logger
        @@ Kirin.router [
             Kirin.WebRTC.signaling_handler ();
           ]
    ]}

    {b Example - Peer Connection:}
    {[
      let peer = Kirin.WebRTC.create_peer ~role:Client () in
      Kirin.WebRTC.Peer.on_datachannel peer (fun dc ->
        dc.on_message <- Some (fun data ->
          Printf.printf "Received: %s\n" (Bytes.to_string data)));
      Kirin.WebRTC.Peer.run peer ~sw ~net ~clock
    ]}
*)

(* Types from Webrtc_config *)
include Webrtc_config

(* Signaling server — Kirin-specific *)
module Signaling = struct
  include Webrtc_signaling
end

(** {1 Protocol Module Re-exports}

    Direct access to ocaml-webrtc protocol stack modules.
    For advanced usage; most users should use Kirin helpers below. *)

module Sdp = Webrtc.Sdp
module Ice = Webrtc.Ice
module Stun = Webrtc.Stun
module Turn = Webrtc.Turn
module Dcep = Webrtc.Dcep

(** {1 Full WebRTC Stack}

    Re-export of {!Webrtc.Webrtc_eio} — the Eio-based full WebRTC stack
    integrating ICE, DTLS, SCTP, and DataChannels. *)

module Peer = Webrtc.Webrtc_eio

(** {1 Kirin-Style Helpers} *)

(** Placeholder DTLS fingerprint (all zeros).
    Not suitable for real DTLS negotiation — for signaling/testing only. *)
let placeholder_dtls_fingerprint =
  { Sdp.hash_func = "sha-256"
  ; Sdp.fingerprint =
      "00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:\
       00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00"
  }

(** Create a WebRTC peer with Kirin ICE server configuration.
    Converts Kirin {!stun_server} list to {!Ice.ice_config}.
    ICE role is derived from the WebRTC [role]:
    [Client] maps to [Ice.Controlling], [Server] to [Ice.Controlled]. *)
let create_peer ?(ice_servers = default_ice_servers) ~role () =
  let ws_ice_servers = List.map ice_server_of_stun ice_servers in
  let ice_role = match role with
    | Peer.Client -> Ice.Controlling
    | Peer.Server -> Ice.Controlled
  in
  let ice_config =
    { Peer.default_ice_config with
      ice_servers = ws_ice_servers
    ; role = ice_role
    }
  in
  Peer.create ~ice_config ~role ()

(** Create a DataChannel on a peer.
    Shorthand for {!Peer.create_datachannel}. *)
let create_datachannel peer ~label =
  Peer.create_datachannel peer ~label

(** Send string data on a DataChannel.
    Converts to bytes internally.
    Returns [Ok bytes_sent] or [Error msg]. *)
let send_datachannel peer channel data =
  Peer.send_channel peer channel (Bytes.of_string data)

(** Create an SDP offer for signaling.
    Uses the peer's ICE credentials with a {b placeholder} DTLS fingerprint
    (all zeros). The generated SDP is {b not suitable for real DTLS negotiation}.
    For production WebRTC connections, use {!Peer.connect} which performs
    actual DTLS handshake with proper certificate fingerprints. *)
let create_offer peer =
  let ufrag, pwd = Peer.get_local_credentials peer in
  let sdp =
    Sdp.create_datachannel_offer
      ~ice_ufrag:ufrag ~ice_pwd:pwd
      ~fingerprint:placeholder_dtls_fingerprint ~sctp_port:5000
  in
  { sdp_type = Offer; sdp = Sdp.to_string sdp }

(** Create an SDP answer from a remote offer SDP string.
    Uses a {b placeholder} DTLS fingerprint (all zeros) — not suitable for
    real DTLS negotiation. For production use, see {!Peer.connect}.
    Returns [Error] if SDP parsing fails. *)
let create_answer peer ~remote_sdp =
  match Sdp.parse remote_sdp with
  | Error e -> Error (Printf.sprintf "Failed to parse remote SDP: %s" e)
  | Ok offer_session ->
    let ufrag, pwd = Peer.get_local_credentials peer in
    let sdp =
      Sdp.create_answer
        ~offer:offer_session
        ~ice_ufrag:ufrag ~ice_pwd:pwd
        ~fingerprint:placeholder_dtls_fingerprint
    in
    Ok { sdp_type = Answer; sdp = Sdp.to_string sdp }

(** {1 HTTP/WebSocket Handlers} *)

(** Create signaling server WebSocket handler.
    Use with Kirin.websocket for signaling. *)
let signaling_handler () =
  let server = Signaling.create_server () in
  fun req ->
    if not (Websocket.is_upgrade_request req) then
      Response.make ~status:`Bad_request (`String "Expected WebSocket upgrade")
    else
      let peer_id = Printf.sprintf "peer_%d" (Random.int 100000) in
      let room_id = Request.query "room" req |> Option.value ~default:"default" in
      Signaling.join_room server ~room_id ~peer_id;
      match Websocket.upgrade_response req with
      | Ok resp ->
        Response.with_header "X-Peer-Id" peer_id resp
      | Error msg ->
        Response.make ~status:`Bad_request (`String msg)

(** STUN server info endpoint. Returns configured ICE servers as JSON. *)
let stun_servers_handler _req =
  let servers = List.map (fun s ->
    `Assoc [
      ("urls", `List (List.map (fun u -> `String u) s.urls));
    ]
  ) default_ice_servers in
  Response.json (`Assoc [("iceServers", `List servers)])

(** {1 Routes Helper} *)

(** Create WebRTC signaling routes:
    - [GET /webrtc/signaling] -- signaling WebSocket endpoint
    - [GET /webrtc/stun-servers] -- STUN server info endpoint *)
let routes () = [
  Router.get "/webrtc/signaling" (signaling_handler ());
  Router.get "/webrtc/stun-servers" stun_servers_handler;
]
