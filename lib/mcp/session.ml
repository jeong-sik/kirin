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
}

(** {1 Constructor} *)

(** Create a new session *)
let create ~server_name ~server_version () =
  {
    state = Uninitialized;
    client_info = None;
    client_capabilities = None;
    server_info = {
      Protocol.name = server_name;
      version = server_version;
    };
    server_capabilities = {
      Protocol.tools = None;
      resources = None;
      prompts = None;
      logging = None;
    };
    request_id = 0;
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
  t.server_capabilities <- { t.server_capabilities with tools = Some true }

(** Enable resources capability *)
let enable_resources t =
  t.server_capabilities <- { t.server_capabilities with resources = Some true }

(** Enable prompts capability *)
let enable_prompts t =
  t.server_capabilities <- { t.server_capabilities with prompts = Some true }

(** Enable logging capability *)
let enable_logging t =
  t.server_capabilities <- { t.server_capabilities with logging = Some true }

(** Get server capabilities *)
let server_capabilities t = t.server_capabilities

(** Get client capabilities *)
let client_capabilities t = t.client_capabilities

(** {1 Initialization} *)

(** Handle initialize request *)
let handle_initialize t (params : Protocol.initialize_params) =
  match t.state with
  | Uninitialized ->
    t.state <- Initializing;
    t.client_info <- Some params.client_info;
    t.client_capabilities <- Some params.capabilities;
    Ok {
      Protocol.protocol_version = Protocol.protocol_version;
      capabilities = t.server_capabilities;
      server_info = t.server_info;
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
