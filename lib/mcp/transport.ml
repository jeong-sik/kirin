(** Kirin MCP - Transport Layer

    Transport implementations for MCP communication.
    Supports stdio (for CLI tools) and Streamable HTTP (for web servers).
    Updated for MCP 2025-11-25 specification.
*)

open Eio

(** {1 Types} *)

(** Stdio transport record *)
type stdio_transport = {
  ic : Buf_read.t;
  oc : Buf_write.t;
}

(** Streamable HTTP transport record (2025-11-25) *)
type streamable_http_transport = {
  endpoint : string;  (** MCP endpoint URL, e.g. "http://localhost:8080/mcp" *)
  mutable pending_requests : (Jsonrpc.id * Jsonrpc.response Eio.Promise.u) list;
  mutable message_queue : Jsonrpc.message Eio.Stream.t;
  mutable session_id : string option;  (** Mcp-Session-Id header value *)
}

(** Transport type *)
type t =
  | Stdio of stdio_transport
  | Streamable_http of streamable_http_transport

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

(** {1 Streamable HTTP Transport} *)

(** Create Streamable HTTP transport (2025-11-25) *)
let create_streamable_http ?(endpoint = "") () =
  Streamable_http {
    endpoint;
    pending_requests = [];
    message_queue = Stream.create 100;
    session_id = None;
  }

(** Queue an incoming message (called by HTTP handler) *)
let queue_message transport msg =
  match transport with
  | Streamable_http t -> Stream.add t.message_queue msg
  | Stdio _ -> raise (Transport_error "Cannot queue message on stdio transport")

(** Read from Streamable HTTP transport (blocks until message available) *)
let read_message_streamable_http (t : streamable_http_transport) =
  Stream.take t.message_queue

(** Register a pending request for response matching *)
let register_pending_request (t : streamable_http_transport) id resolver =
  t.pending_requests <- (id, resolver) :: t.pending_requests

(** Resolve a pending request with a response *)
let resolve_pending_request (t : streamable_http_transport) id response =
  match List.assoc_opt id t.pending_requests with
  | Some resolver ->
    t.pending_requests <- List.remove_assoc id t.pending_requests;
    Promise.resolve resolver response
  | None -> ()

(** Get session ID from transport *)
let session_id = function
  | Streamable_http t -> t.session_id
  | Stdio _ -> None

(** Set session ID on transport *)
let set_session_id transport id =
  match transport with
  | Streamable_http t -> t.session_id <- Some id
  | Stdio _ -> () (* Session IDs are not used with stdio *)

(** {1 Generic Interface} *)

(** Read a message from transport *)
let read_message = function
  | Stdio { ic; _ } -> read_message_stdio ic
  | Streamable_http t -> read_message_streamable_http t

(** Write a message to transport *)
let write_message transport msg =
  match transport with
  | Stdio { oc; _ } -> write_message_stdio oc msg
  | Streamable_http t ->
    Stream.add t.message_queue msg

(** {1 Constructors} *)

(** Create stdio transport from Eio streams *)
let of_stdio ~ic ~oc =
  Stdio { ic; oc }

(** Create Streamable HTTP transport *)
let of_streamable_http = create_streamable_http

(** Get the endpoint URL from an HTTP transport *)
let endpoint = function
  | Streamable_http t -> Some t.endpoint
  | Stdio _ -> None

(** {1 Utilities} *)

(** Check if transport is stdio *)
let is_stdio = function
  | Stdio _ -> true
  | Streamable_http _ -> false

(** Check if transport is Streamable HTTP *)
let is_streamable_http = function
  | Streamable_http _ -> true
  | Stdio _ -> false

(** Send a request via HTTP transport and wait for response.
    Registers a pending request with a Promise, queues the message,
    and blocks the current fiber until the response arrives via
    [resolve_pending_request]. *)
let send_http_request (t : streamable_http_transport) (request : Jsonrpc.request) =
  let promise, resolver = Promise.create () in
  register_pending_request t request.id resolver;
  Stream.add t.message_queue (Jsonrpc.Request request);
  Promise.await promise

(** Send a request and wait for response.
    For stdio: writes the request and reads responses until a matching ID arrives.
    For HTTP: queues the request and awaits a Promise resolved by the HTTP handler. *)
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
  | Streamable_http t ->
    send_http_request t request

(** Send a notification (no response expected) *)
let send_notification transport notif =
  write_message transport (Jsonrpc.Notification notif)
