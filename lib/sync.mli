(** Kirin Sync - Local-First Sync Engine (Linear/Replicache style)

    Provides a protocol for building local-first applications.
    Handles Push (mutations from clients), Pull (delta updates),
    and Poke (change notifications).

    Architecture:
    - Clients store data locally (IndexedDB/SQLite)
    - Clients send mutations to server (Push)
    - Server processes mutations and updates DB
    - Server notifies connected clients (Poke)
    - Clients fetch new data since last version (Pull) *)

(** {1 Types} *)

(** Client identifier. *)
type client_id = string

(** Server version (monotonically increasing). *)
type version = int64

(** Client mutation. *)
type mutation =
  { id : int
  ; name : string
  ; args : Yojson.Safe.t
  }

(** Push request containing client mutations. *)
type push_request =
  { client_id : client_id
  ; mutations : mutation list
  }

(** Sync patch operation. *)
type patch =
  { op : [ `Put | `Del | `Clear ]
  ; key : string
  ; value : Yojson.Safe.t option
  }

(** Pull response with delta updates. *)
type pull_response =
  { cookie : version
  ; patch : patch list
  ; last_mutation_id : int
  }

(** {1 Pub/Sub for Poke} *)

module PubSub : sig
  (** Pub/Sub mechanism for notifying clients of changes. *)

  (** Pub/Sub handle. *)
  type t

  (** [create ()] creates a new pub/sub instance. *)
  val create : unit -> t

  (** [poke t] notifies all listeners of a change. *)
  val poke : t -> unit

  (** [wait t last_version] blocks until the version exceeds [last_version].
      Returns the new version. *)
  val wait : t -> version -> version
end

(** {1 Sync Engine} *)

(** Database backend interface for the sync engine. *)
module type DB = sig
  (** [process_push client_id mutations] applies mutations and returns new server version. *)
  val process_push : client_id -> mutation list -> (version, string) result

  (** [process_pull client_id from_version] returns changes since a specific version. *)
  val process_pull : client_id -> version -> (pull_response, string) result
end

(** [Make(Db)] creates a sync engine backed by [Db]. *)
module Make (_ : DB) : sig
  (** [handle_push req] handles a push request (mutations).
      Notifies all clients via poke on success. *)
  val handle_push : push_request -> (unit, string) result

  (** [handle_pull client_id from_version] handles a pull request (delta sync). *)
  val handle_pull : client_id -> version -> (pull_response, string) result

  (** [handle_poke client_id current_version] waits for changes (Long Polling / SSE).
      Returns the new version when changes are available. *)
  val handle_poke : client_id -> version -> version
end
