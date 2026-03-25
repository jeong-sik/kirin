(** TLS config tests *)

open Alcotest

let test_tls_dev_config () =
  let config = Kirin.tls_dev () in
  match Kirin.tls_validate config with
  | Ok () -> ()
  | Error e -> fail (Kirin.tls_error_string e)

let test_tls_from_pem () =
  let cert = "-----BEGIN CERTIFICATE-----\ntest\n-----END CERTIFICATE-----" in
  let key = "-----BEGIN PRIVATE KEY-----\ntest\n-----END PRIVATE KEY-----" in
  let config = Kirin.tls_from_pem ~certificate:cert ~private_key:key () in
  match Kirin.tls_validate config with
  | Ok () -> ()
  | Error e -> fail (Kirin.tls_error_string e)

let test_tls_invalid_cert () =
  let config = Kirin.tls_from_pem ~certificate:"invalid" ~private_key:"invalid" () in
  match Kirin.tls_validate config with
  | Ok () -> fail "Expected validation error"
  | Error _ -> ()  (* Expected *)

let test_tls_alpn () =
  let cert = "-----BEGIN CERTIFICATE-----\ntest\n-----END CERTIFICATE-----" in
  let key = "-----BEGIN PRIVATE KEY-----\ntest\n-----END PRIVATE KEY-----" in
  let config = Kirin.tls_from_pem ~certificate:cert ~private_key:key
    ~alpn:["h2"; "http/1.1"] () in
  check (list string) "alpn protocols" ["h2"; "http/1.1"]
    config.Kirin.Tls_config.alpn_protocols

let tests = [
  test_case "dev config" `Quick test_tls_dev_config;
  test_case "from pem" `Quick test_tls_from_pem;
  test_case "invalid cert" `Quick test_tls_invalid_cert;
  test_case "alpn protocols" `Quick test_tls_alpn;
]
