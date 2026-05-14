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

(* SSE wire format is line-oriented: each field occupies one line
   and a blank line terminates the event.  The HTML living
   standard treats CR, LF, *and* CRLF as line breaks
   (whatwg.org/html§server-sent-events).

   Two surfaces matter at encode time:

   1. The [event] and [id] fields MUST stay on a single line.  If
      a caller smuggles a [\n] or [\r] inside either, the encoded
      output gets extra rows the receiver reads as legitimate
      additional SSE fields — SSE-protocol injection.  Reject
      those bytes here rather than silently writing a malformed
      (attacker-shaped) stream.

   2. The [data] field is intentionally multi-line, but the old
      splitter only recognised [\n].  A standalone [\r] therefore
      escaped into the wire and the receiver would interpret it
      as a line break anyway — same injection by a different
      route.  Split on CR / LF / CRLF so what we emit matches the
      spec's parsing rules. *)

let contains_cr_or_lf s =
  let bad = ref false in
  String.iter (fun c -> if c = '\r' || c = '\n' then bad := true) s;
  !bad

let validate_single_line ~field s =
  if contains_cr_or_lf s then
    invalid_arg
      (Printf.sprintf
         "Sse.encode: %s field contains CR/LF; SSE protocol injection rejected"
         field)

(* Split a string on any of CR, LF, CRLF.  Empty input yields one
   empty segment, matching [String.split_on_char] semantics so the
   prior "empty data still emits a single [data: ] line" behaviour
   is preserved. *)
let split_sse_lines s =
  let buf = Buffer.create (String.length s) in
  let acc = ref [] in
  let flush () = acc := Buffer.contents buf :: !acc; Buffer.clear buf in
  let n = String.length s in
  let i = ref 0 in
  while !i < n do
    let c = s.[!i] in
    if c = '\r' then begin
      flush ();
      (* CRLF is one break, not two. *)
      if !i + 1 < n && s.[!i + 1] = '\n' then incr i;
      incr i
    end else if c = '\n' then begin
      flush ();
      incr i
    end else begin
      Buffer.add_char buf c;
      incr i
    end
  done;
  flush ();
  List.rev !acc

(** Encode SSE event to string.
    Field order: event, data, id, retry (common convention, matches MDN examples).

    Raises [Invalid_argument] when [event] or [id] contain CR/LF —
    SSE protocol injection guard. *)
let encode e =
  Option.iter (validate_single_line ~field:"event") e.event;
  Option.iter (validate_single_line ~field:"id") e.id;
  let buf = Buffer.create 64 in
  (* 1. event type *)
  (match e.event with
   | Some t -> Buffer.add_string buf ("event: " ^ t ^ "\n")
   | None -> ());
  (* 2. data (CR/LF/CRLF all break lines) *)
  split_sse_lines e.data
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
      Eio.Stream.add body_stream (Some (encode evt));
      loop ()
    in
    (try loop () with Eio.Io _ | End_of_file -> ());
    Eio.Stream.add body_stream None
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