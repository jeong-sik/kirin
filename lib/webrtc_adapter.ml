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

(* Sub-module includes *)
include Webrtc_config

module Signaling = struct
  include Webrtc_signaling
end

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
    mutex : Eio.Mutex.t;
  }

  let create ~label ?(options = default_datachannel_options) () = {
    label;
    state = Connecting;
    options;
    on_open = None;
    on_message = None;
    on_close = None;
    on_error = None;
    mutex = Eio.Mutex.create ();
  }

  let label t = t.label

  let state t = t.state

  let is_open t = t.state = Open

  let on_open t callback =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      t.on_open <- Some callback)

  let on_message t callback =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      t.on_message <- Some callback)

  let on_close t callback =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      t.on_close <- Some callback)

  let on_error t callback =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      t.on_error <- Some callback)

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
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      t.state <- Closing);
    (* TODO: Send DCEP close message *)
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      t.state <- DCClosed;
      (match t.on_close with Some f -> f () | None -> ()))
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
    mutex : Eio.Mutex.t;
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
      mutex = Eio.Mutex.create ();
      ice_ufrag = generate_ufrag ();
      ice_pwd = generate_pwd ();
    }

  let ice_state t = t.ice_state

  let local_description t = t.local_description

  let remote_description t = t.remote_description

  let on_ice_candidate t callback =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      t.on_ice_candidate <- Some callback)

  let on_ice_state_change t callback =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      t.on_ice_state_change <- Some callback)

  let on_data_channel t callback =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      t.on_data_channel <- Some callback)

  (** Create a data channel *)
  let create_data_channel t ~label ?options () =
    let dc = DataChannel.create ~label ?options () in
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      t.data_channels <- dc :: t.data_channels);
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
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      t.local_description <- Some desc);
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
        Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
          t.local_description <- Some desc);
        Ok desc

  (** Set local description *)
  let set_local_description t desc =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      t.local_description <- Some desc);
    Ok ()

  (** Set remote description *)
  let set_remote_description t desc =
    match Webrtc.Sdp.parse desc.sdp with
    | Ok _parsed_sdp ->
      Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        t.remote_description <- Some desc);
      Ok ()
    | Error e ->
      Error (Printf.sprintf "Invalid SDP: %s" e)

  (** Add ICE candidate *)
  let add_ice_candidate t candidate =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      t.ice_candidates <- candidate :: t.ice_candidates);
    Ok ()

  (** Close the connection *)
  let close t =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      t.ice_state <- Closed;
      List.iter (fun dc -> DataChannel.close dc) t.data_channels;
      t.data_channels <- [];
      (match t.on_ice_state_change with Some f -> f Closed | None -> ()))
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
