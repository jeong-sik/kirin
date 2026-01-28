(** WebSocket support per RFC 6455 *)

(** WebSocket GUID for handshake *)
let websocket_guid = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

(** WebSocket opcodes *)
type opcode =
  | Continuation  (* 0x0 *)
  | Text          (* 0x1 *)
  | Binary        (* 0x2 *)
  | Close         (* 0x8 *)
  | Ping          (* 0x9 *)
  | Pong          (* 0xA *)

let opcode_of_int = function
  | 0x0 -> Some Continuation
  | 0x1 -> Some Text
  | 0x2 -> Some Binary
  | 0x8 -> Some Close
  | 0x9 -> Some Ping
  | 0xA -> Some Pong
  | _ -> None

let int_of_opcode = function
  | Continuation -> 0x0
  | Text -> 0x1
  | Binary -> 0x2
  | Close -> 0x8
  | Ping -> 0x9
  | Pong -> 0xA

(** WebSocket frame *)
type frame = {
  fin : bool;
  opcode : opcode;
  payload : string;
}

(** WebSocket close status codes *)
type close_code =
  | Normal            (* 1000 *)
  | GoingAway         (* 1001 *)
  | ProtocolError     (* 1002 *)
  | UnsupportedData   (* 1003 *)
  | InvalidPayload    (* 1007 *)
  | PolicyViolation   (* 1008 *)
  | MessageTooBig     (* 1009 *)
  | InternalError     (* 1011 *)

let int_of_close_code = function
  | Normal -> 1000
  | GoingAway -> 1001
  | ProtocolError -> 1002
  | UnsupportedData -> 1003
  | InvalidPayload -> 1007
  | PolicyViolation -> 1008
  | MessageTooBig -> 1009
  | InternalError -> 1011

let close_code_of_int = function
  | 1000 -> Some Normal
  | 1001 -> Some GoingAway
  | 1002 -> Some ProtocolError
  | 1003 -> Some UnsupportedData
  | 1007 -> Some InvalidPayload
  | 1008 -> Some PolicyViolation
  | 1009 -> Some MessageTooBig
  | 1011 -> Some InternalError
  | _ -> None

(** Compute Sec-WebSocket-Accept from client key *)
let compute_accept_key client_key =
  let combined = client_key ^ websocket_guid in
  let hash = Digestif.SHA1.digest_string combined in
  Base64.encode_exn (Digestif.SHA1.to_raw_string hash)

(** Check if request is a WebSocket upgrade request *)
let is_upgrade_request req =
  let upgrade = Request.header "upgrade" req in
  let connection = Request.header "connection" req in
  let ws_key = Request.header "sec-websocket-key" req in
  match upgrade, connection, ws_key with
  | Some u, Some c, Some _ ->
    String.lowercase_ascii u = "websocket" &&
    String.lowercase_ascii c |> String.split_on_char ',' |>
    List.map String.trim |>
    List.exists (fun s -> String.lowercase_ascii s = "upgrade")
  | _ -> false

(** Create WebSocket upgrade response *)
let upgrade_response req =
  match Request.header "sec-websocket-key" req with
  | None -> Error "Missing Sec-WebSocket-Key header"
  | Some client_key ->
    let accept_key = compute_accept_key client_key in
    let resp = Response.make ~status:`Switching_protocols (`String "")
      |> Response.with_header "upgrade" "websocket"
      |> Response.with_header "connection" "Upgrade"
      |> Response.with_header "sec-websocket-accept" accept_key
    in
    (* Check for subprotocol *)
    let resp = match Request.header "sec-websocket-protocol" req with
      | Some protocols ->
        (* Take first requested protocol as accepted *)
        let first_proto = String.split_on_char ',' protocols
          |> List.hd
          |> String.trim
        in
        Response.with_header "sec-websocket-protocol" first_proto resp
      | None -> resp
    in
    Ok resp

(** Apply XOR mask to payload *)
let apply_mask mask_key payload =
  let len = String.length payload in
  let buf = Bytes.create len in
  for i = 0 to len - 1 do
    let mask_byte = Char.code mask_key.[i mod 4] in
    let payload_byte = Char.code payload.[i] in
    Bytes.set buf i (Char.chr (payload_byte lxor mask_byte))
  done;
  Bytes.to_string buf

(** Encode a frame for sending to client (server-to-client: no mask) *)
let encode_frame frame =
  let payload_len = String.length frame.payload in
  let buf = Buffer.create (payload_len + 14) in

  (* First byte: FIN + opcode *)
  let first_byte = (if frame.fin then 0x80 else 0) lor (int_of_opcode frame.opcode) in
  Buffer.add_char buf (Char.chr first_byte);

  (* Second byte: MASK=0 + payload length *)
  if payload_len < 126 then
    Buffer.add_char buf (Char.chr payload_len)
  else if payload_len < 65536 then begin
    Buffer.add_char buf (Char.chr 126);
    Buffer.add_char buf (Char.chr ((payload_len lsr 8) land 0xFF));
    Buffer.add_char buf (Char.chr (payload_len land 0xFF))
  end else begin
    Buffer.add_char buf (Char.chr 127);
    (* 64-bit length (big-endian) *)
    for i = 7 downto 0 do
      Buffer.add_char buf (Char.chr ((payload_len lsr (i * 8)) land 0xFF))
    done
  end;

  (* Payload (no mask for server-to-client) *)
  Buffer.add_string buf frame.payload;
  Buffer.contents buf

(** Decode a frame from client (client-to-server: masked) *)
let decode_frame data =
  let len = String.length data in
  if len < 2 then Error "Frame too short"
  else
    let first_byte = Char.code data.[0] in
    let second_byte = Char.code data.[1] in

    let fin = (first_byte land 0x80) <> 0 in
    let opcode_int = first_byte land 0x0F in
    let masked = (second_byte land 0x80) <> 0 in
    let payload_len_indicator = second_byte land 0x7F in

    match opcode_of_int opcode_int with
    | None -> Error (Printf.sprintf "Unknown opcode: 0x%X" opcode_int)
    | Some opcode ->
      let header_len, payload_len =
        if payload_len_indicator < 126 then
          (2, payload_len_indicator)
        else if payload_len_indicator = 126 then
          if len < 4 then (4, -1)  (* Need more data *)
          else
            let plen = (Char.code data.[2] lsl 8) lor (Char.code data.[3]) in
            (4, plen)
        else (* 127 *)
          if len < 10 then (10, -1)  (* Need more data *)
          else begin
            let plen = ref 0 in
            for i = 0 to 7 do
              plen := (!plen lsl 8) lor (Char.code data.[2 + i])
            done;
            (10, !plen)
          end
      in

      if payload_len < 0 then Error "Incomplete frame header"
      else
        let mask_len = if masked then 4 else 0 in
        let total_len = header_len + mask_len + payload_len in

        if len < total_len then
          Error (Printf.sprintf "Incomplete frame: need %d, have %d" total_len len)
        else
          let payload =
            let raw_payload = String.sub data (header_len + mask_len) payload_len in
            if masked then
              let mask_key = String.sub data header_len 4 in
              apply_mask mask_key raw_payload
            else
              raw_payload
          in
          Ok ({ fin; opcode; payload }, total_len)

(** Create a text frame *)
let text_frame ?(fin = true) payload =
  { fin; opcode = Text; payload }

(** Create a binary frame *)
let binary_frame ?(fin = true) payload =
  { fin; opcode = Binary; payload }

(** Create a ping frame *)
let ping_frame ?(payload = "") () =
  { fin = true; opcode = Ping; payload }

(** Create a pong frame (in response to ping) *)
let pong_frame ~payload =
  { fin = true; opcode = Pong; payload }

(** Create a close frame *)
let close_frame ?(code = Normal) ?(reason = "") () =
  let payload =
    let code_int = int_of_close_code code in
    let code_bytes = String.init 2 (fun i ->
      if i = 0 then Char.chr ((code_int lsr 8) land 0xFF)
      else Char.chr (code_int land 0xFF)
    ) in
    code_bytes ^ reason
  in
  { fin = true; opcode = Close; payload }

(** Parse close frame payload *)
let parse_close_payload payload =
  if String.length payload < 2 then
    (None, "")
  else
    let code_int = (Char.code payload.[0] lsl 8) lor (Char.code payload.[1]) in
    let code = close_code_of_int code_int in
    let reason = if String.length payload > 2 then String.sub payload 2 (String.length payload - 2) else "" in
    (code, reason)

(** WebSocket connection handler type *)
type handler = {
  on_open : unit -> unit;
  on_message : frame -> frame option;  (* Return frame to send, or None *)
  on_close : close_code option -> string -> unit;
  on_error : string -> unit;
}

(** Default handler that echoes text messages *)
let echo_handler = {
  on_open = (fun () -> ());
  on_message = (fun frame ->
    match frame.opcode with
    | Text | Binary -> Some frame  (* Echo back *)
    | Ping -> Some (pong_frame ~payload:frame.payload)
    | _ -> None
  );
  on_close = (fun _ _ -> ());
  on_error = (fun _ -> ());
}

(** WebSocket middleware for handling upgrade requests *)
let middleware ~path ~handler : (Request.t -> Response.t) -> (Request.t -> Response.t) =
  fun next_handler req ->
    let req_path = Request.path req in
    if req_path = path && is_upgrade_request req then
      match upgrade_response req with
      | Ok resp ->
        (* Note: Actual WebSocket communication would happen after
           the upgrade response is sent, using the underlying connection.
           This middleware only handles the HTTP upgrade handshake. *)
        handler.on_open ();
        resp
      | Error msg ->
        Response.bad_request ~body:msg ()
    else
      next_handler req
