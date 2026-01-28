(** Kirin WebRTC Adapter (Phase 11)

    Provides WebRTC peer-to-peer communication with Kirin-style APIs.

    {b Features:}
    - Data Channels for P2P communication
    - Signaling server via WebSocket
    - ICE candidate trickle
    - STUN/TURN integration

    {b Example - Signaling Server:}
    {[
      let () = Kirin.start ~port:8080
        @@ Kirin.logger
        @@ Kirin.router [
             Kirin.get "/ws/signaling" (Kirin.WebRTC.signaling_handler());
           ]
    ]}

    {b Example - Peer Connection:}
    {[
      let pc = Kirin.WebRTC.PeerConnection.create () in

      (* Create data channel *)
      let dc = Kirin.WebRTC.DataChannel.create pc ~label:"chat" () in

      (* Handle incoming messages *)
      Kirin.WebRTC.DataChannel.on_message dc (fun msg ->
        Printf.printf "Received: %s\n" msg);

      (* Create offer *)
      let offer = Kirin.WebRTC.PeerConnection.create_offer pc in

      (* Exchange SDP via signaling server... *)
    ]}
*)

(** {1 Types} *)

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

(** {1 Default Configurations} *)

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

(** {1 DataChannel} *)

module DataChannel = struct
  type t = {
    label : string;
    mutable state : datachannel_state;
    options : datachannel_options;
    mutable on_open : (unit -> unit) option;
    mutable on_message : (string -> unit) option;
    mutable on_close : (unit -> unit) option;
    mutable on_error : (string -> unit) option;
    mutex : Mutex.t;
  }

  let create ~label ?(options = default_datachannel_options) () = {
    label;
    state = Connecting;
    options;
    on_open = None;
    on_message = None;
    on_close = None;
    on_error = None;
    mutex = Mutex.create ();
  }

  let label t = t.label

  let state t = t.state

  let is_open t = t.state = Open

  let on_open t callback =
    Mutex.lock t.mutex;
    t.on_open <- Some callback;
    Mutex.unlock t.mutex

  let on_message t callback =
    Mutex.lock t.mutex;
    t.on_message <- Some callback;
    Mutex.unlock t.mutex

  let on_close t callback =
    Mutex.lock t.mutex;
    t.on_close <- Some callback;
    Mutex.unlock t.mutex

  let on_error t callback =
    Mutex.lock t.mutex;
    t.on_error <- Some callback;
    Mutex.unlock t.mutex

  let send t _data =
    if t.state <> Open then
      Error "DataChannel is not open"
    else
      (* TODO: Integrate with Webrtc.Dcep for actual sending *)
      Ok ()

  let send_binary t _data =
    if t.state <> Open then
      Error "DataChannel is not open"
    else
      Ok ()

  let close t =
    Mutex.lock t.mutex;
    t.state <- Closing;
    Mutex.unlock t.mutex;
    (* TODO: Send DCEP close message *)
    Mutex.lock t.mutex;
    t.state <- DCClosed;
    (match t.on_close with Some f -> f () | None -> ());
    Mutex.unlock t.mutex
end

(** {1 PeerConnection} *)

module PeerConnection = struct
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
    mutex : Mutex.t;
    (* Internal WebRTC state from ocaml-webrtc *)
    mutable ice_ufrag : string;
    mutable ice_pwd : string;
  }

  let generate_ufrag () =
    let chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" in
    String.init 4 (fun _ -> chars.[Random.int (String.length chars)])

  let generate_pwd () =
    let chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789+/" in
    String.init 24 (fun _ -> chars.[Random.int (String.length chars)])

  let create ?(ice_servers = default_ice_servers) () =
    Random.self_init ();
    {
      ice_servers;
      ice_state = New;
      local_description = None;
      remote_description = None;
      data_channels = [];
      ice_candidates = [];
      on_ice_candidate = None;
      on_ice_state_change = None;
      on_data_channel = None;
      mutex = Mutex.create ();
      ice_ufrag = generate_ufrag ();
      ice_pwd = generate_pwd ();
    }

  let ice_state t = t.ice_state

  let local_description t = t.local_description

  let remote_description t = t.remote_description

  let on_ice_candidate t callback =
    Mutex.lock t.mutex;
    t.on_ice_candidate <- Some callback;
    Mutex.unlock t.mutex

  let on_ice_state_change t callback =
    Mutex.lock t.mutex;
    t.on_ice_state_change <- Some callback;
    Mutex.unlock t.mutex

  let on_data_channel t callback =
    Mutex.lock t.mutex;
    t.on_data_channel <- Some callback;
    Mutex.unlock t.mutex

  (** Create a data channel *)
  let create_data_channel t ~label ?options () =
    let dc = DataChannel.create ~label ?options () in
    Mutex.lock t.mutex;
    t.data_channels <- dc :: t.data_channels;
    Mutex.unlock t.mutex;
    dc

  (** Generate SDP offer using ocaml-webrtc's SDP module *)
  let create_offer t =
    (* Use Webrtc.Sdp to create proper SDP *)
    let fingerprint = {
      Webrtc.Sdp.hash_func = "sha-256";
      fingerprint = "00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00";
    } in
    let sdp = Webrtc.Sdp.create_datachannel_offer
      ~ice_ufrag:t.ice_ufrag
      ~ice_pwd:t.ice_pwd
      ~fingerprint
      ~sctp_port:5000
    in
    let desc = {
      sdp_type = Offer;
      sdp = Webrtc.Sdp.to_string sdp;
    } in
    Mutex.lock t.mutex;
    t.local_description <- Some desc;
    Mutex.unlock t.mutex;
    desc

  (** Generate SDP answer *)
  let create_answer t =
    match t.remote_description with
    | None -> Error "No remote description set"
    | Some remote_desc ->
      (* Parse the remote SDP to get session *)
      match Webrtc.Sdp.parse remote_desc.sdp with
      | Error e -> Error (Printf.sprintf "Failed to parse remote SDP: %s" e)
      | Ok offer_session ->
        let fingerprint = {
          Webrtc.Sdp.hash_func = "sha-256";
          fingerprint = "00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00";
        } in
        let sdp = Webrtc.Sdp.create_answer
          ~offer:offer_session
          ~ice_ufrag:t.ice_ufrag
          ~ice_pwd:t.ice_pwd
          ~fingerprint
        in
        let desc = {
          sdp_type = Answer;
          sdp = Webrtc.Sdp.to_string sdp;
        } in
        Mutex.lock t.mutex;
        t.local_description <- Some desc;
        Mutex.unlock t.mutex;
        Ok desc

  (** Set local description *)
  let set_local_description t desc =
    Mutex.lock t.mutex;
    t.local_description <- Some desc;
    Mutex.unlock t.mutex;
    Ok ()

  (** Set remote description *)
  let set_remote_description t desc =
    match Webrtc.Sdp.parse desc.sdp with
    | Ok _parsed_sdp ->
      Mutex.lock t.mutex;
      t.remote_description <- Some desc;
      Mutex.unlock t.mutex;
      Ok ()
    | Error e ->
      Error (Printf.sprintf "Invalid SDP: %s" e)

  (** Add ICE candidate *)
  let add_ice_candidate t candidate =
    Mutex.lock t.mutex;
    t.ice_candidates <- candidate :: t.ice_candidates;
    Mutex.unlock t.mutex;
    Ok ()

  (** Close the connection *)
  let close t =
    Mutex.lock t.mutex;
    t.ice_state <- Closed;
    List.iter (fun dc -> DataChannel.close dc) t.data_channels;
    t.data_channels <- [];
    (match t.on_ice_state_change with Some f -> f Closed | None -> ());
    Mutex.unlock t.mutex
end

(** {1 Signaling} *)

module Signaling = struct
  (** Signaling message type *)
  type message =
    | SdpOffer of { from_peer : string; sdp : string }
    | SdpAnswer of { from_peer : string; sdp : string }
    | IceCandidate of { from_peer : string; candidate : ice_candidate }
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
            ("candidate", `String candidate.candidate);
            ("sdpMid", match candidate.sdp_mid with Some s -> `String s | None -> `Null);
            ("sdpMLineIndex", match candidate.sdp_mline_index with Some i -> `Int i | None -> `Null);
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
        let candidate = {
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
        Error (Printf.sprintf "Unknown message type: %s" t)
    with
    | Yojson.Json_error e -> Error (Printf.sprintf "JSON parse error: %s" e)
    | e -> Error (Printf.sprintf "Decode error: %s" (Printexc.to_string e))

  (** Signaling room state *)
  type room = {
    id : string;
    peers : (string, unit) Hashtbl.t;
    mutex : Mutex.t;
  }

  type server = {
    rooms : (string, room) Hashtbl.t;
    mutex : Mutex.t;
  }

  let create_server () = {
    rooms = Hashtbl.create 16;
    mutex = Mutex.create ();
  }

  let get_or_create_room server room_id =
    Mutex.lock server.mutex;
    let room = match Hashtbl.find_opt server.rooms room_id with
      | Some r -> r
      | None ->
        let r = {
          id = room_id;
          peers = Hashtbl.create 8;
          mutex = Mutex.create ();
        } in
        Hashtbl.replace server.rooms room_id r;
        r
    in
    Mutex.unlock server.mutex;
    room

  let join_room server ~room_id ~peer_id =
    let room = get_or_create_room server room_id in
    Mutex.lock room.mutex;
    Hashtbl.replace room.peers peer_id ();
    Mutex.unlock room.mutex

  let leave_room server ~room_id ~peer_id =
    match Hashtbl.find_opt server.rooms room_id with
    | None -> ()
    | Some room ->
      Mutex.lock room.mutex;
      Hashtbl.remove room.peers peer_id;
      Mutex.unlock room.mutex

  let get_peers server room_id =
    match Hashtbl.find_opt server.rooms room_id with
    | None -> []
    | Some room ->
      Mutex.lock room.mutex;
      let peers = Hashtbl.fold (fun id _ acc -> id :: acc) room.peers [] in
      Mutex.unlock room.mutex;
      peers
end

(** {1 HTTP/WebSocket Handlers} *)

(** Create signaling server WebSocket handler

    Use with Kirin.websocket for signaling.
    Messages are JSON encoded.
*)
let signaling_handler () =
  let server = Signaling.create_server () in
  fun req ->
    if not (Websocket.is_upgrade_request req) then
      Response.make ~status:`Bad_request (`String "Expected WebSocket upgrade")
    else
      let peer_id = Printf.sprintf "peer_%d" (Random.int 100000) in
      let room_id = Request.query "room" req |> Option.value ~default:"default" in

      (* Join room *)
      Signaling.join_room server ~room_id ~peer_id;

      (* Return upgrade response with peer info *)
      match Websocket.upgrade_response req with
      | Ok resp -> 
        Response.with_header "X-Peer-Id" peer_id resp
      | Error msg -> 
        Response.make ~status:`Bad_request (`String msg)

(** STUN server info endpoint *)
let stun_servers_handler _req =
  let servers = List.map (fun s ->
    `Assoc [
      ("urls", `List (List.map (fun u -> `String u) s.urls));
    ]
  ) default_ice_servers in
  Response.json (`Assoc [("iceServers", `List servers)])

(** {1 Routes Helper} *)

(** Create WebRTC signaling routes *)
let routes () = [
  Router.get "/webrtc/signaling" (signaling_handler ());
  Router.get "/webrtc/stun-servers" stun_servers_handler;
]
