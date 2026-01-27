(** Kirin Sync - Local-First Sync Engine (Linear/Replicache style)

    This module provides a protocol for building local-first applications.
    It handles:
    1. Push: Receiving mutations from clients
    2. Pull: Sending delta updates to clients
    3. Poke: Notifying clients of changes

    Architecture:
    - Clients store data locally (IndexedDB/SQLite)
    - Clients send mutations to server (Push)
    - Server processes mutations and updates DB
    - Server notifies connected clients (Poke)
    - Clients fetch new data since last version (Pull)
*)

(** {1 Types} *)

type client_id = string
type version = int64

type mutation = {
  id : int;
  name : string;
  args : Yojson.Safe.t;
}

type push_request = {
  client_id : client_id;
  mutations : mutation list;
}

type patch = {
  op : [`Put | `Del | `Clear];
  key : string;
  value : Yojson.Safe.t option;
}

type pull_response = {
  cookie : version; (** Server version after applying changes *)
  patch : patch list; (** Changes to apply to client *)
  last_mutation_id : int; (** Last processed mutation for this client *)
}

(** {1 Pub/Sub for Poke} *)

module PubSub = struct
  type t = {
    mutex : Eio.Mutex.t;
    cond : Eio.Condition.t;
    mutable version : version;
  }

  let create () = {
    mutex = Eio.Mutex.create ();
    cond = Eio.Condition.create ();
    version = 0L;
  }

  (** Notify all listeners of a change *)
  let poke t =
    Eio.Mutex.use_rw t.mutex ~protect:true (fun () ->
      t.version <- Int64.add t.version 1L;
      Eio.Condition.broadcast t.cond)

  (** Wait for a change (Long Polling / SSE) *)
  let wait t last_version =
    Eio.Mutex.use_ro t.mutex (fun () ->
      while t.version <= last_version do
        Eio.Condition.await t.cond t.mutex
      done;
      t.version)
end

(** {1 Sync Engine} *)

module type DB = sig
  (** Apply a list of mutations and return the new server version *)
  val process_push : client_id -> mutation list -> (version, string) result

  (** Get changes since a specific version for a client *)
  val process_pull : client_id -> version -> (pull_response, string) result
end

module Make (Db : DB) = struct
  let pubsub = PubSub.create ()

  (** Handle Push request (Mutations) *)
  let handle_push req =
    match Db.process_push req.client_id req.mutations with
    | Ok _new_version ->
        PubSub.poke pubsub; (* Notify all clients *)
        Ok ()
    | Error e -> Error e

  (** Handle Pull request (Delta Sync) *)
  let handle_pull client_id from_version =
    Db.process_pull client_id from_version

  (** Handle Poke (Wait for changes) *)
  let handle_poke _client_id current_version =
    PubSub.wait pubsub current_version
end
