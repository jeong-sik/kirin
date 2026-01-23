(** Kirin MCP - Transport Layer

    Transport implementations for MCP communication.
    Supports stdio (for CLI tools) and HTTP+SSE (for web servers).
*)

open Eio

(** {1 Types} *)

(** Stdio transport record *)
type stdio_transport = {
  ic : Buf_read.t;
  oc : Buf_write.t;
}

(** HTTP+SSE transport record *)
type http_sse_transport = {
  mutable pending_requests : (Jsonrpc.id * Jsonrpc.response Eio.Promise.u) list;
  mutable message_queue : Jsonrpc.message Eio.Stream.t;
}

(** Transport type *)
type t =
  | Stdio of stdio_transport
  | Http_sse of http_sse_transport

(** Transport error *)
exception Transport_error of string

(** {1 Stdio Transport} *)

(** Read a single line from stdio *)
let read_line_stdio ic =
  try
    Buf_read.line ic
  with End_of_file ->
    raise (Transport_error "Connection closed")

(** Write a line to stdio *)
let write_line_stdio oc line =
  Buf_write.string oc line;
  Buf_write.char oc '\n';
  Buf_write.flush oc

(** Read a JSON-RPC message from stdio
    MCP uses newline-delimited JSON (NDJSON) format *)
let read_message_stdio ic =
  let line = read_line_stdio ic in
  try
    let json = Yojson.Safe.from_string line in
    Jsonrpc.decode json
  with
  | Yojson.Json_error msg ->
    raise (Transport_error (Printf.sprintf "JSON parse error: %s" msg))

(** Write a JSON-RPC message to stdio *)
let write_message_stdio oc msg =
  let json = Jsonrpc.encode msg in
  let line = Yojson.Safe.to_string json in
  write_line_stdio oc line

(** {1 HTTP+SSE Transport} *)

(** Create HTTP+SSE transport *)
let create_http_sse () =
  Http_sse {
    pending_requests = [];
    message_queue = Stream.create 100;
  }

(** Queue an incoming message (called by HTTP handler) *)
let queue_message transport msg =
  match transport with
  | Http_sse t -> Stream.add t.message_queue msg
  | Stdio _ -> raise (Transport_error "Cannot queue message on stdio transport")

(** Read from HTTP+SSE transport (blocks until message available) *)
let read_message_http (t : http_sse_transport) =
  Stream.take t.message_queue

(** Register a pending request for response matching *)
let register_pending_request (t : http_sse_transport) id resolver =
  t.pending_requests <- (id, resolver) :: t.pending_requests

(** Resolve a pending request with a response *)
let resolve_pending_request (t : http_sse_transport) id response =
  match List.assoc_opt id t.pending_requests with
  | Some resolver ->
    t.pending_requests <- List.remove_assoc id t.pending_requests;
    Promise.resolve resolver response
  | None -> ()

(** {1 Generic Interface} *)

(** Read a message from transport *)
let read_message = function
  | Stdio { ic; _ } -> read_message_stdio ic
  | Http_sse t -> read_message_http t

(** Write a message to transport *)
let write_message transport msg =
  match transport with
  | Stdio { oc; _ } -> write_message_stdio oc msg
  | Http_sse _ ->
    (* For HTTP+SSE, messages are written via SSE connection *)
    (* This is handled by the server/client at a higher level *)
    ()

(** {1 Constructors} *)

(** Create stdio transport from Eio streams *)
let of_stdio ~ic ~oc =
  Stdio { ic; oc }

(** Create HTTP+SSE transport *)
let of_http_sse = create_http_sse

(** {1 Utilities} *)

(** Check if transport is stdio *)
let is_stdio = function
  | Stdio _ -> true
  | Http_sse _ -> false

(** Check if transport is HTTP+SSE *)
let is_http_sse = function
  | Http_sse _ -> true
  | Stdio _ -> false

(** Send a request and wait for response (stdio only) *)
let send_request transport request =
  match transport with
  | Stdio { ic; oc } ->
    write_message_stdio oc (Jsonrpc.Request request);
    (* Read responses until we get one matching our id *)
    let rec wait_for_response () =
      match read_message_stdio ic with
      | Jsonrpc.Response resp when resp.id = request.id -> resp
      | _ -> wait_for_response ()
    in
    wait_for_response ()
  | Http_sse _ ->
    raise (Transport_error "Use async methods for HTTP+SSE transport")

(** Send a notification (no response expected) *)
let send_notification transport notif =
  write_message transport (Jsonrpc.Notification notif)
