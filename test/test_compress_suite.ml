(** Compression tests *)

open Alcotest
open Test_helpers

let test_compress_parse_accept_encoding () =
  check (option Alcotest.(testable (fun fmt -> function
    | Kirin.Compress.Gzip -> Format.fprintf fmt "Gzip"
    | Kirin.Compress.Deflate -> Format.fprintf fmt "Deflate"
  ) (=)))
    "gzip preferred" (Some Kirin.Compress.Gzip)
    (Kirin.Compress.parse_accept_encoding "gzip, deflate, br");

  check (option Alcotest.(testable (fun fmt -> function
    | Kirin.Compress.Gzip -> Format.fprintf fmt "Gzip"
    | Kirin.Compress.Deflate -> Format.fprintf fmt "Deflate"
  ) (=)))
    "deflate only" (Some Kirin.Compress.Deflate)
    (Kirin.Compress.parse_accept_encoding "deflate")

let test_compress_skip_for_images () =
  check bool "skip images" true (Kirin.Compress.skip_compression_for "image/png");
  check bool "skip video" true (Kirin.Compress.skip_compression_for "video/mp4");
  check bool "skip gzip" true (Kirin.Compress.skip_compression_for "application/gzip");
  check bool "compress text" false (Kirin.Compress.skip_compression_for "text/html");
  check bool "compress json" false (Kirin.Compress.skip_compression_for "application/json")

let test_compress_gzip () =
  (* Create a large enough input to compress *)
  let input = String.make 2000 'A' in
  let compressed = Kirin.compress_gzip input in
  check bool "compressed smaller" true (String.length compressed < String.length input);
  (* Gzip magic bytes: 0x1f 0x8b *)
  check bool "gzip magic" true
    (String.length compressed >= 2 &&
     Char.code compressed.[0] = 0x1f &&
     Char.code compressed.[1] = 0x8b)

let test_compress_deflate () =
  let input = String.make 2000 'B' in
  let compressed = Kirin.compress_deflate input in
  check bool "compressed smaller" true (String.length compressed < String.length input)

let test_compress_middleware () =
  (* Create a handler that returns large content *)
  let large_content = String.make 2000 'X' in
  let handler _req = Kirin.html large_content in
  let with_compress = Kirin.compress handler in

  (* Request with Accept-Encoding: gzip *)
  let req = make_test_request
    ~headers:[("accept-encoding", "gzip, deflate")]
    "/" in
  let resp = with_compress req in

  check (option string) "content-encoding" (Some "gzip") (Kirin.Response.header "content-encoding" resp);
  check bool "response compressed" true (String.length (response_body_to_string (Kirin.Response.body resp)) < 2000)

(* Pretty-printer for the algorithm option so the new RFC tests
   can use the existing [check (option ...)] shape without
   re-declaring the testable inline at every callsite. *)
let algo_testable =
  Alcotest.testable
    (fun fmt -> function
       | Kirin.Compress.Gzip -> Format.fprintf fmt "Gzip"
       | Kirin.Compress.Deflate -> Format.fprintf fmt "Deflate")
    (=)

(* RFC 7231 §3.1.2.1: coding names are case-insensitive.  Browsers
   send lowercase but proxies and custom HTTP clients occasionally
   send uppercase; before this PR an uppercase coding silently
   disabled compression even though the client clearly accepted
   it. *)
let test_accept_encoding_is_case_insensitive () =
  check (option algo_testable) "GZIP upper"
    (Some Kirin.Compress.Gzip)
    (Kirin.Compress.parse_accept_encoding "GZIP, br");
  check (option algo_testable) "Gzip mixed"
    (Some Kirin.Compress.Gzip)
    (Kirin.Compress.parse_accept_encoding "Gzip");
  check (option algo_testable) "DEFLATE upper"
    (Some Kirin.Compress.Deflate)
    (Kirin.Compress.parse_accept_encoding "DEFLATE")

(* RFC 7231 §5.3.4: [q=0] is an explicit reject.  Before this PR
   the server cheerfully compressed for clients that had opted
   out — a real concern for CRIME/BREACH-aware clients that set
   [Accept-Encoding: gzip;q=0] deliberately. *)
let test_accept_encoding_q_zero_is_reject () =
  check (option algo_testable) "gzip rejected, no alternative"
    None
    (Kirin.Compress.parse_accept_encoding "gzip;q=0");
  check (option algo_testable) "gzip rejected, deflate accepted"
    (Some Kirin.Compress.Deflate)
    (Kirin.Compress.parse_accept_encoding "gzip;q=0, deflate");
  check (option algo_testable) "both rejected"
    None
    (Kirin.Compress.parse_accept_encoding "gzip;q=0, deflate;q=0");
  check (option algo_testable) "uppercase Q=0 still rejects"
    None
    (Kirin.Compress.parse_accept_encoding "gzip;Q=0")

(* Sanity: q > 0 accepts; gzip > deflate priority preserved. *)
let test_accept_encoding_q_positive_accepts () =
  check (option algo_testable) "gzip q=0.5 accepted"
    (Some Kirin.Compress.Gzip)
    (Kirin.Compress.parse_accept_encoding "gzip;q=0.5");
  check (option algo_testable) "gzip wins over deflate"
    (Some Kirin.Compress.Gzip)
    (Kirin.Compress.parse_accept_encoding "deflate;q=1.0, gzip;q=0.5")

let test_accept_encoding_whitespace_tolerance () =
  (* Real proxies sometimes pad with arbitrary spacing around
     ';' and '='; the parser must still see the q-value. *)
  check (option algo_testable) "spaces around ; and ="
    None
    (Kirin.Compress.parse_accept_encoding "gzip ; q = 0");
  check (option algo_testable) "spaces around comma"
    (Some Kirin.Compress.Gzip)
    (Kirin.Compress.parse_accept_encoding "  gzip  ,  deflate  ")

let test_accept_encoding_garbage_q_is_ignored () =
  (* A malformed q-value should be treated as if absent (default
     1.0), not as a reject.  Pin "garbage means accept" rather
     than "garbage means reject". *)
  check (option algo_testable) "garbage q-value defaults to accept"
    (Some Kirin.Compress.Gzip)
    (Kirin.Compress.parse_accept_encoding "gzip;q=banana")

let tests = [
  test_case "parse accept-encoding" `Quick test_compress_parse_accept_encoding;
  test_case "skip compression for" `Quick test_compress_skip_for_images;
  test_case "gzip compression" `Quick test_compress_gzip;
  test_case "deflate compression" `Quick test_compress_deflate;
  test_case "compression middleware" `Quick test_compress_middleware;
  test_case "accept-encoding case insensitive" `Quick test_accept_encoding_is_case_insensitive;
  test_case "accept-encoding q=0 is reject" `Quick test_accept_encoding_q_zero_is_reject;
  test_case "accept-encoding q>0 accepts" `Quick test_accept_encoding_q_positive_accepts;
  test_case "accept-encoding whitespace tolerance" `Quick test_accept_encoding_whitespace_tolerance;
  test_case "accept-encoding garbage q ignored" `Quick test_accept_encoding_garbage_q_is_ignored;
]
