(** WebRTC Signaling server implementation.

    Manages signaling rooms where peers exchange SDP offers/answers
    and ICE candidates for WebRTC connection establishment.

    @since 1.0.0
    @status experimental
    This module's API may change in future versions. *)

(** {1 Message Types} *)

(** Signaling message type. *)
type message =
  | SdpOffer of { from_peer : string; sdp : string }
  | SdpAnswer of { from_peer : string; sdp : string }
  | IceCandidate of { from_peer : string; candidate : Webrtc_config.ice_candidate }
  | Join of { peer_id : string; room : string }
  | Leave of { peer_id : string }
  | Error of { message : string }

(** [encode_message msg] serializes a signaling message to a JSON string. *)
val encode_message : message -> string

(** [decode_message json_str] parses a JSON string into a signaling message.
    Returns [Error] on malformed JSON or unknown message types. *)
val decode_message : string -> (message, string) result

(** {1 Room Management} *)

(** Signaling room holding a set of connected peers. *)
type room = {
  id : string;
  peers : (string, unit) Hashtbl.t;
  mutex : Eio.Mutex.t;
}

(** Signaling server managing multiple rooms. *)
type server = {
  rooms : (string, room) Hashtbl.t;
  mutex : Eio.Mutex.t;
}

(** [create_server ()] creates a new signaling server instance. *)
val create_server : unit -> server

(** [get_or_create_room server room_id] returns the room for [room_id],
    creating it if it does not exist. *)
val get_or_create_room : server -> string -> room

(** [join_room server ~room_id ~peer_id] adds a peer to a room. *)
val join_room : server -> room_id:string -> peer_id:string -> unit

(** [leave_room server ~room_id ~peer_id] removes a peer from a room.
    No-op if the room or peer does not exist.

    Removes the room itself from [server.rooms] when the last peer
    leaves, so a stream of unique room ids cannot accumulate empty
    entries forever. *)
val leave_room : server -> room_id:string -> peer_id:string -> unit

(** [get_peers server room_id] returns the list of peer IDs in a room.
    Returns an empty list if the room does not exist. *)
val get_peers : server -> string -> string list

(** [room_exists server room_id] reports whether the server is
    currently tracking a room with this id.  Contract: an empty
    room is *not* tracked — once the last peer leaves,
    [leave_room] collects the entry. *)
val room_exists : server -> string -> bool
