(** WebRTC tests (Phase 11) -- v1.0.0 real ocaml-webrtc API *)

open Alcotest
open Test_helpers

module WR = Kirin.WebRTC

(* Test connection state types *)
let test_webrtc_ice_states () =
  let states : Kirin.webrtc_connection_state list = [
    New; Connecting; Connected; Disconnected; Failed; Closed
  ] in
  check int "connection states count" 6 (List.length states)

(* Test peer creation *)
let test_webrtc_peer_create () =
  let peer = WR.create_peer ~role:WR.Peer.Client () in
  check bool "state is new"
    true (WR.Peer.get_state peer = WR.Peer.New)

(* Test peer creation with custom ICE servers *)
let test_webrtc_peer_ice_servers () =
  let ice_servers = [
    { WR.urls = ["stun:stun.example.com:3478"]; username = None; credential = None };
    { WR.urls = ["turn:turn.example.com:3478"]; username = Some "user"; credential = Some "pass" };
  ] in
  let peer = WR.create_peer ~ice_servers ~role:WR.Peer.Client () in
  check bool "state is new"
    true (WR.Peer.get_state peer = WR.Peer.New)

(* Test DataChannel creation on a peer *)
let test_webrtc_datachannel_create () =
  let peer = WR.create_peer ~role:WR.Peer.Client () in
  let dc = WR.create_datachannel peer ~label:"test-channel" in
  check string "label" "test-channel" dc.WR.Peer.label

(* Test create_offer *)
let test_webrtc_create_offer () =
  let peer = WR.create_peer ~role:WR.Peer.Client () in
  let _ = WR.create_datachannel peer ~label:"data" in
  let offer = WR.create_offer peer in
  check bool "offer type" true (offer.sdp_type = WR.Offer);
  check bool "offer has SDP" true (String.length offer.sdp > 0);
  check bool "SDP contains v=0" true (String.sub offer.sdp 0 3 = "v=0")

(* Test ICE server conversion roundtrip *)
let test_webrtc_ice_server_conversion () =
  let stun : WR.stun_server = {
    urls = ["stun:example.com:3478"];
    username = Some "user";
    credential = Some "pass";
  } in
  let ice = WR.ice_server_of_stun stun in
  let back = WR.stun_of_ice_server ice in
  check (list string) "urls" stun.urls back.urls;
  check (option string) "username" stun.username back.username;
  check (option string) "credential" stun.credential back.credential

(* Test Signaling message encoding *)
let test_webrtc_signaling_encode () =
  let msg = WR.Signaling.SdpOffer { from_peer = "peer1"; sdp = "v=0..." } in
  let json = WR.Signaling.encode_message msg in
  check bool "is json object" true (json.[0] = '{');
  let parsed = Yojson.Safe.from_string json in
  let type_field = Yojson.Safe.Util.member "type" parsed |> Yojson.Safe.Util.to_string in
  check string "type is offer" "offer" type_field;
  let sdp_field = Yojson.Safe.Util.member "sdp" parsed |> Yojson.Safe.Util.to_string in
  check string "sdp preserved" "v=0..." sdp_field

(* Test Signaling message decoding *)
let test_webrtc_signaling_decode () =
  let json = {|{"type":"join","peerId":"peer123","room":"test-room"}|} in
  match WR.Signaling.decode_message json with
  | Ok (WR.Signaling.Join { peer_id; room }) ->
    check string "peer_id" "peer123" peer_id;
    check string "room" "test-room" room
  | Ok _ -> fail "expected Join message"
  | Error e -> fail ("decode failed: " ^ e)

(* Test Signaling decode error *)
let test_webrtc_signaling_decode_error () =
  let invalid_json = {|{"type":"unknown_type"}|} in
  match WR.Signaling.decode_message invalid_json with
  | Error _ -> ()  (* expected *)
  | Ok _ -> fail "expected error for unknown type"

(* Test ICE candidate encoding/decoding *)
let test_webrtc_ice_candidate () =
  let candidate : WR.ice_candidate = {
    candidate = "candidate:1 1 UDP 2122252543 192.168.1.1 12345 typ host";
    sdp_mid = Some "0";
    sdp_mline_index = Some 0;
    ufrag = None;
  } in
  let msg = WR.Signaling.IceCandidate { from_peer = "peer1"; candidate } in
  let json = WR.Signaling.encode_message msg in
  let parsed = Yojson.Safe.from_string json in
  let type_field = Yojson.Safe.Util.member "type" parsed |> Yojson.Safe.Util.to_string in
  check string "type is ice-candidate" "ice-candidate" type_field;
  let cand_obj = Yojson.Safe.Util.member "candidate" parsed in
  let cand_str = Yojson.Safe.Util.member "candidate" cand_obj |> Yojson.Safe.Util.to_string in
  check bool "candidate preserved" true (String.length cand_str > 0)

(* Test routes helper *)
let test_webrtc_routes () =
  let routes = WR.routes () in
  check int "route count" 2 (List.length routes)

let tests = [
  test_case "ice states" `Quick (with_eio test_webrtc_ice_states);
  test_case "peer create" `Quick (with_eio test_webrtc_peer_create);
  test_case "peer ice servers" `Quick (with_eio test_webrtc_peer_ice_servers);
  test_case "datachannel create" `Quick (with_eio test_webrtc_datachannel_create);
  test_case "create offer" `Quick (with_eio test_webrtc_create_offer);
  test_case "ice server conversion" `Quick (with_eio test_webrtc_ice_server_conversion);
  test_case "signaling encode" `Quick (with_eio test_webrtc_signaling_encode);
  test_case "signaling decode" `Quick (with_eio test_webrtc_signaling_decode);
  test_case "signaling decode error" `Quick (with_eio test_webrtc_signaling_decode_error);
  test_case "ice candidate" `Quick (with_eio test_webrtc_ice_candidate);
  test_case "routes helper" `Quick (with_eio test_webrtc_routes);
]
