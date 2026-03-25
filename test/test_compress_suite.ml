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

let tests = [
  test_case "parse accept-encoding" `Quick test_compress_parse_accept_encoding;
  test_case "skip compression for" `Quick test_compress_skip_for_images;
  test_case "gzip compression" `Quick test_compress_gzip;
  test_case "deflate compression" `Quick test_compress_deflate;
  test_case "compression middleware" `Quick test_compress_middleware;
]
