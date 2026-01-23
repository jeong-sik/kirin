(** Server-Sent Events (SSE) support *)

(** SSE event type *)
type event = {
  event : string option;  (* Event type (optional) *)
  data : string;          (* Event data (required) *)
  id : string option;     (* Event ID for reconnection (optional) *)
  retry : int option;     (* Reconnection interval in ms (optional) *)
}

(** Create a simple data-only event *)
let data content =
  { event = None; data = content; id = None; retry = None }

(** Create an event with type *)
let event ~event_type content =
  { event = Some event_type; data = content; id = None; retry = None }

(** Add ID to event *)
let with_id id evt =
  { evt with id = Some id }

(** Add retry interval to event *)
let with_retry ms evt =
  { evt with retry = Some ms }

(** Encode event to SSE format *)
let encode evt =
  let buf = Buffer.create 64 in

  (* Event type *)
  (match evt.event with
   | Some e -> Buffer.add_string buf ("event: " ^ e ^ "\n")
   | None -> ());

  (* Data (can be multi-line - each line prefixed with "data: ") *)
  let lines = String.split_on_char '\n' evt.data in
  List.iter (fun line ->
    Buffer.add_string buf "data: ";
    Buffer.add_string buf line;
    Buffer.add_char buf '\n'
  ) lines;

  (* ID *)
  (match evt.id with
   | Some id -> Buffer.add_string buf ("id: " ^ id ^ "\n")
   | None -> ());

  (* Retry *)
  (match evt.retry with
   | Some ms -> Buffer.add_string buf ("retry: " ^ string_of_int ms ^ "\n")
   | None -> ());

  (* End of event *)
  Buffer.add_char buf '\n';
  Buffer.contents buf

(** Encode multiple events *)
let encode_many events =
  String.concat "" (List.map encode events)

(** SSE response headers *)
let headers = [
  ("content-type", "text/event-stream");
  ("cache-control", "no-cache");
  ("connection", "keep-alive");
  ("x-accel-buffering", "no");  (* Disable nginx buffering *)
]

(** Create an SSE response with initial events *)
let response events =
  let body = encode_many events in
  let resp = Response.make ~status:`OK body in
  List.fold_left (fun r (k, v) -> Response.with_header k v r) resp headers

(** Create an empty SSE response (ready for streaming) *)
let stream_start () =
  let resp = Response.make ~status:`OK "" in
  List.fold_left (fun r (k, v) -> Response.with_header k v r) resp headers

(** Comment line (for keep-alive ping) *)
let comment text =
  ": " ^ text ^ "\n\n"

(** Keep-alive ping (empty comment) *)
let ping () = ": ping\n\n"

(** Parse Last-Event-ID header for reconnection *)
let last_event_id req =
  Request.header "last-event-id" req

(** SSE handler type for streaming *)
type stream_handler = {
  on_start : unit -> event list;           (* Initial events *)
  on_reconnect : string -> event list;     (* Events after reconnection (with last ID) *)
}

(** Default stream handler *)
let default_handler = {
  on_start = (fun () -> []);
  on_reconnect = (fun _ -> []);
}

(** Create SSE endpoint handler *)
let handler ~on_events : Request.t -> Response.t =
  fun req ->
    let events = match last_event_id req with
      | Some _id -> on_events ()  (* Could filter based on ID *)
      | None -> on_events ()
    in
    response events

(** Middleware to serve SSE at a specific path *)
let middleware ~path ~on_events : (Request.t -> Response.t) -> (Request.t -> Response.t) =
  fun next_handler req ->
    if Request.path req = path then
      handler ~on_events req
    else
      next_handler req
