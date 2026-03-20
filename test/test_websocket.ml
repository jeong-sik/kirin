(** WebSocket module tests *)

open Alcotest

(* -- opcode conversion --------------------------------------------- *)

let test_opcode_of_int () =
  check bool "text" true (Kirin.Websocket.opcode_of_int 0x1 = Some Kirin.Websocket.Text);
  check bool "binary" true (Kirin.Websocket.opcode_of_int 0x2 = Some Kirin.Websocket.Binary);
  check bool "close" true (Kirin.Websocket.opcode_of_int 0x8 = Some Kirin.Websocket.Close);
  check bool "ping" true (Kirin.Websocket.opcode_of_int 0x9 = Some Kirin.Websocket.Ping);
  check bool "pong" true (Kirin.Websocket.opcode_of_int 0xA = Some Kirin.Websocket.Pong);
  check bool "continuation" true (Kirin.Websocket.opcode_of_int 0x0 = Some Kirin.Websocket.Continuation);
  check bool "invalid" true (Kirin.Websocket.opcode_of_int 0x3 = None)

let test_int_of_opcode () =
  check int "text" 0x1 (Kirin.Websocket.int_of_opcode Kirin.Websocket.Text);
  check int "binary" 0x2 (Kirin.Websocket.int_of_opcode Kirin.Websocket.Binary);
  check int "close" 0x8 (Kirin.Websocket.int_of_opcode Kirin.Websocket.Close);
  check int "ping" 0x9 (Kirin.Websocket.int_of_opcode Kirin.Websocket.Ping);
  check int "pong" 0xA (Kirin.Websocket.int_of_opcode Kirin.Websocket.Pong);
  check int "continuation" 0x0 (Kirin.Websocket.int_of_opcode Kirin.Websocket.Continuation)

let test_opcode_roundtrip () =
  let opcodes = [Kirin.Websocket.Continuation; Text; Binary; Close; Ping; Pong] in
  List.iter (fun op ->
    let n = Kirin.Websocket.int_of_opcode op in
    check bool "roundtrip"
      true (Kirin.Websocket.opcode_of_int n = Some op)
  ) opcodes

let opcode_tests = [
  test_case "opcode_of_int" `Quick test_opcode_of_int;
  test_case "int_of_opcode" `Quick test_int_of_opcode;
  test_case "roundtrip" `Quick test_opcode_roundtrip;
]

(* -- close codes --------------------------------------------------- *)

let test_close_code_values () =
  check int "Normal" 1000 (Kirin.Websocket.int_of_close_code Kirin.Websocket.Normal);
  check int "GoingAway" 1001 (Kirin.Websocket.int_of_close_code Kirin.Websocket.GoingAway);
  check int "ProtocolError" 1002 (Kirin.Websocket.int_of_close_code Kirin.Websocket.ProtocolError);
  check int "UnsupportedData" 1003 (Kirin.Websocket.int_of_close_code Kirin.Websocket.UnsupportedData);
  check int "InvalidPayload" 1007 (Kirin.Websocket.int_of_close_code Kirin.Websocket.InvalidPayload);
  check int "PolicyViolation" 1008 (Kirin.Websocket.int_of_close_code Kirin.Websocket.PolicyViolation);
  check int "MessageTooBig" 1009 (Kirin.Websocket.int_of_close_code Kirin.Websocket.MessageTooBig);
  check int "InternalError" 1011 (Kirin.Websocket.int_of_close_code Kirin.Websocket.InternalError)

let test_close_code_roundtrip () =
  let codes = [
    Kirin.Websocket.Normal; GoingAway; ProtocolError; UnsupportedData;
    InvalidPayload; PolicyViolation; MessageTooBig; InternalError;
  ] in
  List.iter (fun c ->
    let n = Kirin.Websocket.int_of_close_code c in
    check bool "close roundtrip" true (Kirin.Websocket.close_code_of_int n = Some c)
  ) codes

let test_close_code_unknown () =
  check bool "unknown code" true (Kirin.Websocket.close_code_of_int 9999 = None)

let close_code_tests = [
  test_case "close code values" `Quick test_close_code_values;
  test_case "close code roundtrip" `Quick test_close_code_roundtrip;
  test_case "unknown close code" `Quick test_close_code_unknown;
]

(* -- apply_mask ---------------------------------------------------- *)

let test_mask_identity () =
  let mask = "\x12\x34\x56\x78" in
  let payload = "Hello, WebSocket" in
  let masked = Kirin.Websocket.apply_mask mask payload in
  let unmasked = Kirin.Websocket.apply_mask mask masked in
  check string "roundtrip" payload unmasked

let test_mask_empty () =
  let mask = "\x00\x00\x00\x00" in
  let result = Kirin.Websocket.apply_mask mask "" in
  check string "empty" "" result

let test_mask_zero_key () =
  let mask = "\x00\x00\x00\x00" in
  let payload = "abc" in
  let result = Kirin.Websocket.apply_mask mask payload in
  check string "zero mask = identity" payload result

let mask_tests = [
  test_case "mask/unmask roundtrip" `Quick test_mask_identity;
  test_case "empty payload" `Quick test_mask_empty;
  test_case "zero mask key" `Quick test_mask_zero_key;
]

(* -- frame constructors -------------------------------------------- *)

let test_text_frame () =
  let f = Kirin.Websocket.text_frame "hello" in
  check bool "fin" true f.Kirin.Websocket.fin;
  check bool "opcode is Text" true (f.Kirin.Websocket.opcode = Kirin.Websocket.Text);
  check string "payload" "hello" f.Kirin.Websocket.payload

let test_text_frame_continuation () =
  let f = Kirin.Websocket.text_frame ~fin:false "part1" in
  check bool "not fin" false f.Kirin.Websocket.fin

let test_binary_frame () =
  let f = Kirin.Websocket.binary_frame "\x00\x01\x02" in
  check bool "opcode is Binary" true (f.Kirin.Websocket.opcode = Kirin.Websocket.Binary);
  check string "payload" "\x00\x01\x02" f.Kirin.Websocket.payload

let test_ping_frame () =
  let f = Kirin.Websocket.ping_frame () in
  check bool "opcode is Ping" true (f.Kirin.Websocket.opcode = Kirin.Websocket.Ping);
  check string "empty payload" "" f.Kirin.Websocket.payload

let test_ping_frame_with_payload () =
  let f = Kirin.Websocket.ping_frame ~payload:"heartbeat" () in
  check string "payload" "heartbeat" f.Kirin.Websocket.payload

let test_pong_frame () =
  let f = Kirin.Websocket.pong_frame ~payload:"heartbeat" in
  check bool "opcode is Pong" true (f.Kirin.Websocket.opcode = Kirin.Websocket.Pong);
  check string "payload echoes" "heartbeat" f.Kirin.Websocket.payload

let test_close_frame_default () =
  let f = Kirin.Websocket.close_frame () in
  check bool "opcode is Close" true (f.Kirin.Websocket.opcode = Kirin.Websocket.Close);
  check bool "fin" true f.Kirin.Websocket.fin;
  check bool "payload >= 2 bytes" true (String.length f.Kirin.Websocket.payload >= 2)

let test_close_frame_with_reason () =
  let f = Kirin.Websocket.close_frame ~code:Kirin.Websocket.GoingAway ~reason:"bye" () in
  check bool "payload has reason" true (String.length f.Kirin.Websocket.payload > 2)

let frame_tests = [
  test_case "text frame" `Quick test_text_frame;
  test_case "text continuation" `Quick test_text_frame_continuation;
  test_case "binary frame" `Quick test_binary_frame;
  test_case "ping frame" `Quick test_ping_frame;
  test_case "ping with payload" `Quick test_ping_frame_with_payload;
  test_case "pong frame" `Quick test_pong_frame;
  test_case "close frame default" `Quick test_close_frame_default;
  test_case "close frame with reason" `Quick test_close_frame_with_reason;
]

(* -- encode/decode roundtrip --------------------------------------- *)

let test_encode_small_text () =
  let frame = Kirin.Websocket.text_frame "Hi" in
  let encoded = Kirin.Websocket.encode_frame frame in
  (* First byte: 0x80 | 0x01 = 0x81, second byte: 2 (length) *)
  check int "encoded length" 4 (String.length encoded);
  check int "first byte" 0x81 (Char.code encoded.[0]);
  check int "length byte" 2 (Char.code encoded.[1])

let test_encode_medium_payload () =
  let payload = String.make 200 'x' in
  let frame = Kirin.Websocket.text_frame payload in
  let encoded = Kirin.Websocket.encode_frame frame in
  (* 2 header + 2 extended length + 200 payload = 204 *)
  check int "encoded length" 204 (String.length encoded);
  check int "length indicator" 126 (Char.code encoded.[1])

let test_encode_decode_roundtrip () =
  let payload = "test" in
  let mask_key = "\xAA\xBB\xCC\xDD" in
  let masked_payload = Kirin.Websocket.apply_mask mask_key payload in
  let buf = Buffer.create 32 in
  Buffer.add_char buf (Char.chr 0x81);
  Buffer.add_char buf (Char.chr (0x80 lor String.length payload));
  Buffer.add_string buf mask_key;
  Buffer.add_string buf masked_payload;
  let data = Buffer.contents buf in
  match Kirin.Websocket.decode_frame data with
  | Ok (frame, consumed) ->
    check bool "fin" true frame.Kirin.Websocket.fin;
    check bool "opcode Text" true (frame.Kirin.Websocket.opcode = Kirin.Websocket.Text);
    check string "decoded payload" payload frame.Kirin.Websocket.payload;
    check int "consumed bytes" (String.length data) consumed
  | Error msg ->
    fail ("decode failed: " ^ msg)

let test_decode_too_short () =
  match Kirin.Websocket.decode_frame "\x81" with
  | Error _ -> ()
  | Ok _ -> fail "should fail on short data"

let test_decode_unmasked () =
  let data = "\x81\x02Hi" in
  match Kirin.Websocket.decode_frame data with
  | Ok (frame, _) ->
    check string "payload" "Hi" frame.Kirin.Websocket.payload
  | Error msg ->
    fail ("unmasked decode: " ^ msg)

let encode_tests = [
  test_case "encode small text" `Quick test_encode_small_text;
  test_case "encode medium payload" `Quick test_encode_medium_payload;
  test_case "encode/decode roundtrip (masked)" `Quick test_encode_decode_roundtrip;
  test_case "decode too short" `Quick test_decode_too_short;
  test_case "decode unmasked" `Quick test_decode_unmasked;
]

(* -- parse_close_payload ------------------------------------------- *)

let test_parse_close_normal () =
  let f = Kirin.Websocket.close_frame ~code:Kirin.Websocket.Normal ~reason:"goodbye" () in
  let (code, reason) = Kirin.Websocket.parse_close_payload f.Kirin.Websocket.payload in
  check bool "code is Normal" true (code = Some Kirin.Websocket.Normal);
  check string "reason" "goodbye" reason

let test_parse_close_empty () =
  let (code, reason) = Kirin.Websocket.parse_close_payload "" in
  check bool "no code" true (code = None);
  check string "no reason" "" reason

let test_parse_close_code_only () =
  let f = Kirin.Websocket.close_frame ~code:Kirin.Websocket.GoingAway () in
  let (code, reason) = Kirin.Websocket.parse_close_payload f.Kirin.Websocket.payload in
  check bool "GoingAway" true (code = Some Kirin.Websocket.GoingAway);
  check string "empty reason" "" reason

let close_payload_tests = [
  test_case "parse normal close" `Quick test_parse_close_normal;
  test_case "parse empty close" `Quick test_parse_close_empty;
  test_case "parse code-only close" `Quick test_parse_close_code_only;
]

(* -- compute_accept_key -------------------------------------------- *)

let test_accept_key () =
  let result = Kirin.Websocket.compute_accept_key "dGhlIHNhbXBsZSBub25jZQ==" in
  check string "RFC 6455 test vector" "s3pPLMBiTxaQ9kYGzzhZRbK+xOo=" result

let handshake_tests = [
  test_case "accept key (RFC 6455)" `Quick test_accept_key;
]

(* -- echo_handler -------------------------------------------------- *)

let test_echo_text () =
  let frame = Kirin.Websocket.text_frame "echo me" in
  let result = Kirin.Websocket.echo_handler.Kirin.Websocket.on_message frame in
  check bool "returns frame" true (Option.is_some result);
  let resp = Option.get result in
  check string "echoed payload" "echo me" resp.Kirin.Websocket.payload

let test_echo_ping_to_pong () =
  let frame = Kirin.Websocket.ping_frame ~payload:"ping-data" () in
  let result = Kirin.Websocket.echo_handler.Kirin.Websocket.on_message frame in
  check bool "returns pong" true (Option.is_some result);
  let resp = Option.get result in
  check bool "is pong" true (resp.Kirin.Websocket.opcode = Kirin.Websocket.Pong);
  check string "pong payload" "ping-data" resp.Kirin.Websocket.payload

let test_echo_close_ignored () =
  let frame = Kirin.Websocket.close_frame () in
  let result = Kirin.Websocket.echo_handler.Kirin.Websocket.on_message frame in
  check bool "close returns None" true (Option.is_none result)

let echo_tests = [
  test_case "echo text" `Quick test_echo_text;
  test_case "ping to pong" `Quick test_echo_ping_to_pong;
  test_case "close ignored" `Quick test_echo_close_ignored;
]

(* -- run ----------------------------------------------------------- *)

let () =
  Alcotest.run "Websocket" [
    ("opcode", opcode_tests);
    ("close_code", close_code_tests);
    ("mask", mask_tests);
    ("frame_constructors", frame_tests);
    ("encode/decode", encode_tests);
    ("close_payload", close_payload_tests);
    ("handshake", handshake_tests);
    ("echo_handler", echo_tests);
  ]
