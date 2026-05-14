(** WebSocket tests *)

open Alcotest

let test_ws_accept_key () =
  (* Test vector from RFC 6455 *)
  let client_key = "dGhlIHNhbXBsZSBub25jZQ==" in
  let accept_key = Kirin.Websocket.compute_accept_key client_key in
  check string "accept key" "s3pPLMBiTxaQ9kYGzzhZRbK+xOo=" accept_key

let test_ws_is_upgrade_request () =
  (* Valid upgrade request *)
  let raw_valid = Http.Request.make
    ~headers:(Http.Header.of_list [
      ("upgrade", "websocket");
      ("connection", "Upgrade");
      ("sec-websocket-key", "dGhlIHNhbXBsZSBub25jZQ==");
    ])
    ~meth:`GET "/" in
  let body_source = Eio.Flow.string_source "" |> Eio.Buf_read.of_flow ~max_size:1024 in
  let req_valid = Kirin.Request.make ~raw:raw_valid ~body_source in
  check bool "is upgrade" true (Kirin.is_websocket_upgrade req_valid);

  (* Non-upgrade request *)
  let raw_invalid = Http.Request.make
    ~headers:(Http.Header.of_list [("content-type", "text/html")])
    ~meth:`GET "/" in
  let body_source = Eio.Flow.string_source "" |> Eio.Buf_read.of_flow ~max_size:1024 in
  let req_invalid = Kirin.Request.make ~raw:raw_invalid ~body_source in
  check bool "not upgrade" false (Kirin.is_websocket_upgrade req_invalid)

let test_ws_upgrade_response () =
  let raw = Http.Request.make
    ~headers:(Http.Header.of_list [
      ("upgrade", "websocket");
      ("connection", "Upgrade");
      ("sec-websocket-key", "dGhlIHNhbXBsZSBub25jZQ==");
    ])
    ~meth:`GET "/" in
  let body_source = Eio.Flow.string_source "" |> Eio.Buf_read.of_flow ~max_size:1024 in
  let req = Kirin.Request.make ~raw ~body_source in

  match Kirin.websocket_upgrade req with
  | Ok resp ->
    check int "status 101" 101 (Kirin.Response.status_code resp);
    check (option string) "upgrade header" (Some "websocket")
      (Kirin.Response.header "upgrade" resp);
    check (option string) "accept key" (Some "s3pPLMBiTxaQ9kYGzzhZRbK+xOo=")
      (Kirin.Response.header "sec-websocket-accept" resp)
  | Error msg ->
    fail ("Upgrade failed: " ^ msg)

let test_ws_encode_text_frame () =
  let frame = Kirin.ws_text "Hello" in
  let encoded = Kirin.ws_encode frame in

  (* First byte: FIN=1, opcode=1 (text) = 0x81 *)
  check char "first byte" '\x81' encoded.[0];
  (* Second byte: MASK=0, length=5 = 0x05 *)
  check char "second byte" '\x05' encoded.[1];
  (* Payload *)
  check string "payload" "Hello" (String.sub encoded 2 5)

let test_ws_encode_large_frame () =
  (* Test 16-bit length encoding (126-65535 bytes) *)
  let payload = String.make 1000 'X' in
  let frame = Kirin.ws_binary payload in
  let encoded = Kirin.ws_encode frame in

  (* First byte: FIN=1, opcode=2 (binary) = 0x82 *)
  check char "first byte" '\x82' encoded.[0];
  (* Second byte: MASK=0, length=126 (indicating 16-bit length follows) *)
  check char "second byte" '\x7e' encoded.[1];
  (* 16-bit length in big-endian: 1000 = 0x03E8 *)
  check int "length high" 0x03 (Char.code encoded.[2]);
  check int "length low" 0xE8 (Char.code encoded.[3])

let test_ws_decode_masked_frame () =
  (* Build a masked text frame: "Hi" with mask key [0x12, 0x34, 0x56, 0x78] *)
  let raw = Bytes.create 8 in
  Bytes.set raw 0 '\x81';  (* FIN=1, opcode=1 *)
  Bytes.set raw 1 '\x82';  (* MASK=1, length=2 *)
  (* Mask key *)
  Bytes.set raw 2 '\x12';
  Bytes.set raw 3 '\x34';
  Bytes.set raw 4 '\x56';
  Bytes.set raw 5 '\x78';
  (* Masked payload: 'H' xor 0x12, 'i' xor 0x34 *)
  Bytes.set raw 6 (Char.chr (Char.code 'H' lxor 0x12));
  Bytes.set raw 7 (Char.chr (Char.code 'i' lxor 0x34));

  match Kirin.ws_decode (Bytes.to_string raw) with
  | Ok (frame, consumed) ->
    check bool "fin" true frame.fin;
    check bool "is text" (frame.opcode = Kirin.Text) true;
    check string "payload" "Hi" frame.payload;
    check int "consumed" 8 consumed
  | Error msg ->
    fail ("Decode failed: " ^ msg)

let test_ws_close_frame () =
  let frame = Kirin.ws_close ~code:Kirin.Websocket.Normal ~reason:"Bye" () in
  check bool "is close" (frame.opcode = Kirin.Close) true;
  (* Payload: 2 bytes for code (1000) + "Bye" *)
  check int "payload length" 5 (String.length frame.payload);
  (* Parse it back *)
  let code, reason = Kirin.Websocket.parse_close_payload frame.payload in
  check (option bool) "code is Normal" (Some true)
    (Option.map (fun c -> c = Kirin.Websocket.Normal) code);
  check string "reason" "Bye" reason

(* Build a 64-bit length header (no mask) declaring [announced] bytes
   of payload, without actually providing them. The decoder must
   reject the announcement before any allocation that scales with
   [announced]. *)
let build_64bit_header ~announced =
  let buf = Bytes.create 10 in
  Bytes.set buf 0 '\x82';            (* FIN=1, opcode=binary *)
  Bytes.set buf 1 '\x7f';            (* MASK=0, len indicator=127 *)
  for i = 0 to 7 do
    Bytes.set buf (2 + i)
      (Char.chr ((announced lsr ((7 - i) * 8)) land 0xFF))
  done;
  Bytes.to_string buf

let test_ws_decode_rejects_oversized () =
  (* Announce 2 GiB, far past the 16 MiB default. The decoder must
     refuse before trying to confirm those bytes have arrived. *)
  let header = build_64bit_header ~announced:(2 * 1024 * 1024 * 1024) in
  (match Kirin.ws_decode header with
   | Ok _ -> fail "expected oversized payload to be rejected"
   | Error msg ->
     (* Message should be the size-cap class, not the
        "incomplete frame" class — they have very different
        operational meanings to the caller. *)
     check bool "rejected by size cap" true
       (String.length msg > 0
        && (try let _ = Str.search_forward
                  (Str.regexp_string "exceeds maximum") msg 0 in true
            with Not_found -> false)))

let test_ws_decode_respects_custom_cap () =
  (* Use the 16-bit indicator (announced=200) and a tiny custom cap. *)
  let raw =
    let header = Bytes.create 4 in
    Bytes.set header 0 '\x82';
    Bytes.set header 1 '\x7e';
    Bytes.set header 2 '\x00';
    Bytes.set header 3 '\xc8';      (* 200 *)
    Bytes.to_string header ^ String.make 200 'A'
  in
  (match Kirin.ws_decode ~max_payload_size:100 raw with
   | Ok _ -> fail "expected payload above custom cap to be rejected"
   | Error _ -> ());
  (* Same input is accepted when the cap is raised above 200. *)
  (match Kirin.ws_decode ~max_payload_size:1024 raw with
   | Ok (frame, _) ->
     check int "payload length" 200 (String.length frame.payload)
   | Error msg -> fail ("expected acceptance, got: " ^ msg))

let test_ws_decode_rejects_64bit_high_bit_set () =
  (* 64-bit length with high bit set wraps the OCaml 63-bit int to a
     negative number when read with lsl-or; that must be rejected as
     oversized, never as "incomplete frame" (which invites buffering). *)
  let buf = Bytes.create 10 in
  Bytes.set buf 0 '\x82';
  Bytes.set buf 1 '\x7f';
  Bytes.set buf 2 '\xff';            (* high byte's high bit set *)
  for i = 3 to 9 do Bytes.set buf i '\xff' done;
  (match Kirin.ws_decode (Bytes.to_string buf) with
   | Ok _ -> fail "expected high-bit-set 64-bit length to be rejected"
   | Error msg ->
     check bool "rejected as oversized, not as incomplete"
       true
       (try let _ = Str.search_forward
              (Str.regexp_string "exceeds maximum") msg 0 in true
        with Not_found -> false))

let test_ws_ping_pong () =
  let ping = Kirin.ws_ping ~payload:"ping-data" () in
  check bool "is ping" (ping.opcode = Kirin.Ping) true;
  check string "ping payload" "ping-data" ping.payload;

  let pong = Kirin.ws_pong ~payload:ping.payload in
  check bool "is pong" (pong.opcode = Kirin.Pong) true;
  check string "pong echoes" "ping-data" pong.payload

let tests = [
  test_case "compute accept key" `Quick test_ws_accept_key;
  test_case "is upgrade request" `Quick test_ws_is_upgrade_request;
  test_case "upgrade response" `Quick test_ws_upgrade_response;
  test_case "encode text frame" `Quick test_ws_encode_text_frame;
  test_case "encode large frame" `Quick test_ws_encode_large_frame;
  test_case "decode masked frame" `Quick test_ws_decode_masked_frame;
  test_case "decode rejects oversized payload" `Quick test_ws_decode_rejects_oversized;
  test_case "decode respects custom cap" `Quick test_ws_decode_respects_custom_cap;
  test_case "decode rejects 64-bit high-bit-set length" `Quick
    test_ws_decode_rejects_64bit_high_bit_set;
  test_case "close frame" `Quick test_ws_close_frame;
  test_case "ping pong" `Quick test_ws_ping_pong;
]
