(** Kirin MCP - Session Management

    Manages MCP session state and lifecycle.
    Handles initialization, capability negotiation, and cleanup.
*)

(** {1 Types} *)

(** Session state *)
type state =
  | Uninitialized
  | Initializing
  | Ready
  | Closing
  | Closed

(** Session info *)
type t = {
  mutable state : state;
  mutable client_info : Protocol.implementation_info option;
  mutable client_capabilities : Protocol.client_capabilities option;
  mutable server_info : Protocol.implementation_info;
  mutable server_capabilities : Protocol.server_capabilities;
  mutable request_id : int;
  mutable session_id : string option;
}

(** {1 Constructor} *)

(** Generate a session ID from /dev/urandom (128-bit CSPRNG).
    Reads 16 bytes and hex-encodes to 32 chars.
    Same pattern as lib/auth/secure_random.ml. *)
let generate_session_id () =
  let len = 16 in
  let buf = Bytes.create len in
  let fd = Unix.openfile "/dev/urandom" [Unix.O_RDONLY] 0 in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
    let rec loop off =
      if off < len then begin
        let n = Unix.read fd buf off (len - off) in
        if n <= 0 then failwith "generate_session_id: /dev/urandom short read";
        loop (off + n)
      end
    in
    loop 0);
  Bytes.to_seq buf
  |> Seq.map (fun c -> Printf.sprintf "%02x" (Char.code c))
  |> List.of_seq |> String.concat ""

(** Create a new session *)
let create ~server_name ~server_version () =
  {
    state = Uninitialized;
    client_info = None;
    client_capabilities = None;
    server_info = {
      Protocol.name = server_name;
      version = server_version;
      description = None;
    };
    server_capabilities = {
      Protocol.tools = None;
      resources = None;
      prompts = None;
      logging = None;
    };
    request_id = 0;
    session_id = None;
  }

(** {1 State Management} *)

(** Get current state *)
let state t = t.state

(** Check if session is ready *)
let is_ready t = t.state = Ready

(** Check if session is closed *)
let is_closed t = t.state = Closed

(** {1 Capability Management} *)

(** Enable tools capability *)
let enable_tools t =
  t.server_capabilities <- { t.server_capabilities with
    tools = Some { Protocol.list_changed = None } }

(** Enable resources capability *)
let enable_resources t =
  t.server_capabilities <- { t.server_capabilities with
    resources = Some { Protocol.subscribe = None; list_changed = None } }

(** Enable prompts capability *)
let enable_prompts t =
  t.server_capabilities <- { t.server_capabilities with
    prompts = Some { Protocol.list_changed = None } }

(** Enable logging capability *)
let enable_logging t =
  t.server_capabilities <- { t.server_capabilities with logging = Some true }

(** Get server capabilities *)
let server_capabilities t = t.server_capabilities

(** Get client capabilities *)
let client_capabilities t = t.client_capabilities

(** {1 Initialization} *)

(** Handle initialize request.
    Generates a session ID on first call. *)
let handle_initialize t (params : Protocol.initialize_params) =
  match t.state with
  | Uninitialized ->
    t.state <- Initializing;
    t.client_info <- Some params.client_info;
    t.client_capabilities <- Some params.capabilities;
    t.session_id <- Some (generate_session_id ());
    Ok {
      Protocol.protocol_version = Protocol.protocol_version;
      capabilities = t.server_capabilities;
      server_info = t.server_info;
      _meta = None;
    }
  | _ ->
    Error "Session already initialized"

(** Handle initialized notification *)
let handle_initialized t =
  match t.state with
  | Initializing ->
    t.state <- Ready;
    Ok ()
  | _ ->
    Error "Unexpected initialized notification"

(** {1 Request ID Generation} *)

(** Generate next request ID *)
let next_request_id t =
  let id = t.request_id in
  t.request_id <- id + 1;
  Jsonrpc.Int id

(** {1 Shutdown} *)

(** Begin shutdown *)
let begin_shutdown t =
  t.state <- Closing

(** Complete shutdown *)
let complete_shutdown t =
  t.state <- Closed

(** {1 Info Accessors} *)

(** Get server info *)
let server_info t = t.server_info

(** Get client info *)
let client_info t = t.client_info

(** Get session ID *)
let session_id t = t.session_id
