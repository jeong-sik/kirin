(** WebRTC Signaling server implementation *)

(** Signaling message type *)
type message =
  | SdpOffer of { from_peer : string; sdp : string }
  | SdpAnswer of { from_peer : string; sdp : string }
  | IceCandidate of { from_peer : string; candidate : Webrtc_config.ice_candidate }
  | Join of { peer_id : string; room : string }
  | Leave of { peer_id : string }
  | Error of { message : string }

(** Encode message to JSON *)
let encode_message msg =
  let json = match msg with
    | SdpOffer { from_peer; sdp } ->
      `Assoc [
        ("type", `String "offer");
        ("from", `String from_peer);
        ("sdp", `String sdp);
      ]
    | SdpAnswer { from_peer; sdp } ->
      `Assoc [
        ("type", `String "answer");
        ("from", `String from_peer);
        ("sdp", `String sdp);
      ]
    | IceCandidate { from_peer; candidate } ->
      `Assoc [
        ("type", `String "ice-candidate");
        ("from", `String from_peer);
        ("candidate", `Assoc [
          ("candidate", `String candidate.Webrtc_config.candidate);
          ("sdpMid", match candidate.Webrtc_config.sdp_mid with Some s -> `String s | None -> `Null);
          ("sdpMLineIndex", match candidate.Webrtc_config.sdp_mline_index with Some i -> `Int i | None -> `Null);
        ]);
      ]
    | Join { peer_id; room } ->
      `Assoc [
        ("type", `String "join");
        ("peerId", `String peer_id);
        ("room", `String room);
      ]
    | Leave { peer_id } ->
      `Assoc [
        ("type", `String "leave");
        ("peerId", `String peer_id);
      ]
    | Error { message } ->
      `Assoc [
        ("type", `String "error");
        ("message", `String message);
      ]
  in
  Yojson.Safe.to_string json

(** Decode message from JSON *)
let decode_message json_str =
  try
    let json = Yojson.Safe.from_string json_str in
    let open Yojson.Safe.Util in
    let msg_type = json |> member "type" |> to_string in
    match msg_type with
    | "offer" ->
      let from_peer = json |> member "from" |> to_string in
      let sdp = json |> member "sdp" |> to_string in
      Ok (SdpOffer { from_peer; sdp })
    | "answer" ->
      let from_peer = json |> member "from" |> to_string in
      let sdp = json |> member "sdp" |> to_string in
      Ok (SdpAnswer { from_peer; sdp })
    | "ice-candidate" ->
      let from_peer = json |> member "from" |> to_string in
      let cand = json |> member "candidate" in
      let candidate : Webrtc_config.ice_candidate = {
        candidate = cand |> member "candidate" |> to_string;
        sdp_mid = cand |> member "sdpMid" |> to_string_option;
        sdp_mline_index = cand |> member "sdpMLineIndex" |> to_int_option;
        ufrag = None;
      } in
      Ok (IceCandidate { from_peer; candidate })
    | "join" ->
      let peer_id = json |> member "peerId" |> to_string in
      let room = json |> member "room" |> to_string in
      Ok (Join { peer_id; room })
    | "leave" ->
      let peer_id = json |> member "peerId" |> to_string in
      Ok (Leave { peer_id })
    | t ->
      Result.Error (Printf.sprintf "Unknown message type: %s" t)
  with
  | Yojson.Json_error e -> Result.Error (Printf.sprintf "JSON parse error: %s" e)
  | Yojson.Safe.Util.Type_error (msg, _) -> Result.Error (Printf.sprintf "Decode error: %s" msg)

(** Signaling room state *)
type room = {
  id : string;
  peers : (string, unit) Hashtbl.t;
  mutex : Eio.Mutex.t;
}

type server = {
  rooms : (string, room) Hashtbl.t;
  mutex : Eio.Mutex.t;
}

let create_server () = {
  rooms = Hashtbl.create 16;
  mutex = Eio.Mutex.create ();
}

let get_or_create_room server room_id =
  Eio.Mutex.use_rw ~protect:true server.mutex (fun () ->
    match Hashtbl.find_opt server.rooms room_id with
    | Some r -> r
    | None ->
      let r = {
        id = room_id;
        peers = Hashtbl.create 8;
        mutex = Eio.Mutex.create ();
      } in
      Hashtbl.replace server.rooms room_id r;
      r
  )

let join_room server ~room_id ~peer_id =
  let room = get_or_create_room server room_id in
  Eio.Mutex.use_rw ~protect:true room.mutex (fun () ->
    Hashtbl.replace room.peers peer_id ())

let leave_room server ~room_id ~peer_id =
  match Hashtbl.find_opt server.rooms room_id with
  | None -> ()
  | Some room ->
    Eio.Mutex.use_rw ~protect:true room.mutex (fun () ->
      Hashtbl.remove room.peers peer_id)

let get_peers server room_id =
  match Hashtbl.find_opt server.rooms room_id with
  | None -> []
  | Some room ->
    Eio.Mutex.use_ro room.mutex (fun () ->
      Hashtbl.fold (fun id _ acc -> id :: acc) room.peers [])
