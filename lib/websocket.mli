(** WebSocket support per RFC 6455.

    Handles the HTTP upgrade handshake, frame encoding/decoding,
    and provides a middleware for routing WebSocket connections.

    @since 1.0.0
    @status stable *)

(** {1 Types} *)

(** WebSocket opcodes. *)
type opcode =
  | Continuation
  | Text
  | Binary
  | Close
  | Ping
  | Pong

(** [opcode_of_int n] converts an integer to an opcode. *)
val opcode_of_int : int -> opcode option

(** [int_of_opcode op] converts an opcode to its integer value. *)
val int_of_opcode : opcode -> int

(** WebSocket frame. *)
type frame = {
  fin : bool;
  opcode : opcode;
  payload : string;
}

(** WebSocket close status codes. *)
type close_code =
  | Normal
  | GoingAway
  | ProtocolError
  | UnsupportedData
  | InvalidPayload
  | PolicyViolation
  | MessageTooBig
  | InternalError

(** [int_of_close_code code] converts a close code to its integer value. *)
val int_of_close_code : close_code -> int

(** [close_code_of_int n] converts an integer to a close code. *)
val close_code_of_int : int -> close_code option

(** {1 Handshake} *)

(** [compute_accept_key client_key] computes the Sec-WebSocket-Accept value
    from the client's Sec-WebSocket-Key. *)
val compute_accept_key : string -> string

(** [is_upgrade_request req] returns [true] if the request is a WebSocket upgrade. *)
val is_upgrade_request : Request.t -> bool

(** [upgrade_response req] creates a 101 Switching Protocols response
    or returns an error if the key header is missing. *)
val upgrade_response : Request.t -> (Response.t, string) result

(** {1 Frame Operations} *)

(** [apply_mask mask_key payload] XORs payload bytes with the 4-byte mask. *)
val apply_mask : string -> string -> string

(** [encode_frame frame] encodes a frame for server-to-client transmission (no mask). *)
val encode_frame : frame -> string

(** Default per-frame payload cap used by [decode_frame] (16 MiB).
    Exposed so callers can reuse the same baseline when sizing their
    own buffers or building their own framing layer. *)
val default_max_payload_size : int

(** [decode_frame ?max_payload_size data] decodes a client-to-server
    frame (masked). Returns the frame and the number of bytes consumed,
    or an error.

    @param max_payload_size caps the announced payload length before
           any allocation. Defaults to [default_max_payload_size].
           Frames whose announced length exceeds the cap are rejected
           with an [Error] message suitable for emitting a
           [1009 MessageTooBig] close. Frames whose 64-bit length
           overflowed OCaml's signed 63-bit [int] are rejected as
           oversized rather than as incomplete — declaring them
           incomplete would invite the caller to keep buffering an
           attacker-chosen number of bytes. *)
val decode_frame : ?max_payload_size:int -> string -> (frame * int, string) result

(** {1 Frame Constructors} *)

(** [text_frame ?fin payload] creates a text frame. *)
val text_frame : ?fin:bool -> string -> frame

(** [binary_frame ?fin payload] creates a binary frame. *)
val binary_frame : ?fin:bool -> string -> frame

(** [ping_frame ?payload ()] creates a ping frame. *)
val ping_frame : ?payload:string -> unit -> frame

(** [pong_frame ~payload] creates a pong frame echoing the ping payload. *)
val pong_frame : payload:string -> frame

(** [close_frame ?code ?reason ()] creates a close frame. *)
val close_frame : ?code:close_code -> ?reason:string -> unit -> frame

(** [parse_close_payload payload] extracts the close code and reason from a close frame. *)
val parse_close_payload : string -> close_code option * string

(** {1 Handler} *)

(** WebSocket connection handler callbacks. *)
type handler = {
  on_open : unit -> unit;
  on_message : frame -> frame option;
  on_close : close_code option -> string -> unit;
  on_error : string -> unit;
}

(** Default handler that echoes text/binary messages and responds to pings. *)
val echo_handler : handler

(** {1 Middleware} *)

(** [middleware ~path ~handler] creates a middleware that upgrades
    matching requests to WebSocket connections. *)
val middleware :
  path:string ->
  handler:handler ->
  (Request.t -> Response.t) -> (Request.t -> Response.t)
