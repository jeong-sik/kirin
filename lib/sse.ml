(** Server-Sent Events (SSE) - Realtime Broadcaster

    {b Features:}
    - Channel-based broadcasting
    - Automatic keep-alive (ping)
    - Last-Event-ID support
    - High-performance dispatch
*)

(** SSE event type *)
type event = {
  event : string option;
  data : string;
  id : string option;
  retry : int option;
}

(** Create a simple data-only SSE event *)
let data d = { event = None; data = d; id = None; retry = None }

(** Create an SSE event with type (positional) *)
let event t d = { event = Some t; data = d; id = None; retry = None }

(** Create an SSE event with type (labeled, backward compat) *)
let event_typed ~event_type d = { event = Some event_type; data = d; id = None; retry = None }

(** Add ID to SSE event *)
let with_id id e = { e with id = Some id }

(** Add retry interval to SSE event *)
let with_retry ms e = { e with retry = Some ms }

(** Encode SSE event to string.
    Field order: event, data, id, retry (common convention, matches MDN examples) *)
let encode e =
  let buf = Buffer.create 64 in
  (* 1. event type *)
  (match e.event with
   | Some t -> Buffer.add_string buf ("event: " ^ t ^ "\n")
   | None -> ());
  (* 2. data (handles multi-line) *)
  String.split_on_char '\n' e.data
  |> List.iter (fun line ->
       Buffer.add_string buf ("data: " ^ line ^ "\n"));
  (* 3. id *)
  (match e.id with
   | Some id -> Buffer.add_string buf ("id: " ^ id ^ "\n")
   | None -> ());
  (* 4. retry *)
  (match e.retry with
   | Some ms -> Buffer.add_string buf ("retry: " ^ string_of_int ms ^ "\n")
   | None -> ());
  Buffer.add_string buf "\n";
  Buffer.contents buf

(** {1 Broadcaster} *)

module Broadcaster = struct
  type t = {
    clients : (int, event Eio.Stream.t) Hashtbl.t;
    next_id : int Atomic.t;
    mutex : Eio.Mutex.t;
  }

  (** Create a broadcaster *)
  let create () = { 
    clients = Hashtbl.create 100;
    next_id = Atomic.make 0;
    mutex = Eio.Mutex.create ();
  }

      (** Subscribe a client, returning a stream of events *)
      let subscribe t =
        let id = Atomic.fetch_and_add t.next_id 1 in
        let stream = Eio.Stream.create 100 in
        Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
          Hashtbl.add t.clients id stream
        );
        (id, stream)
    
      (** Unsubscribe a client *)
      let unsubscribe t id =
        Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
          Hashtbl.remove t.clients id
        )  (** Broadcast an event to all connected clients *)
  let broadcast t evt =
    Eio.Mutex.use_ro t.mutex (fun () ->
      Hashtbl.iter (fun _id stream ->
        (* Blocking add for now - increase buffer size in production *)
        Eio.Stream.add stream evt
      ) t.clients
    )
end

(** Create SSE response from an event stream.
    Reads events from the Eio stream, encodes them to SSE wire format,
    and yields the encoded strings via the Producer pattern. *)
let response stream =
  let headers = Http.Header.of_list [
    ("Content-Type", "text/event-stream");
    ("Cache-Control", "no-cache");
    ("Connection", "keep-alive");
    ("X-Accel-Buffering", "no");
  ] in
  let stream_producer body_stream =
    let rec loop () =
      let evt = Eio.Stream.take stream in
      Eio.Stream.add body_stream (encode evt);
      loop ()
    in
    (try loop () with _ -> ());
    Eio.Stream.add body_stream ""
  in
  Response.make ~status:`OK ~headers (`Producer stream_producer)
(** Keep-alive ping *)
let ping = ": ping\n\n"

(** Get Last-Event-ID header *)
let last_event_id req = 
  Request.header "Last-Event-ID" req

(** Middleware to handle SSE headers (deprecated, use response directly) *) 
let middleware next req = next req

(** Helper for creating an SSE endpoint handler with a Broadcaster *)

let handler broadcaster _req =

  let _id, stream = Broadcaster.subscribe broadcaster in

  (* Note: We need a way to detect disconnect and unsubscribe.

     Currently Response.stream doesn't provide a callback.

     For now, we rely on GC or explicit heartbeat failure. *)

  response stream



(** Legacy response for list of events (Phase 32 compat) *)

(** Create an SSE response from a finite list of events.
    Encodes all events to a single string body (not streaming). *)
let response_legacy events =
  let buf = Buffer.create 256 in
  List.iter (fun evt -> Buffer.add_string buf (encode evt)) events;
  let headers = Http.Header.of_list [
    ("Content-Type", "text/event-stream");
    ("Cache-Control", "no-cache");
  ] in
  Response.make ~status:`OK ~headers (`String (Buffer.contents buf))