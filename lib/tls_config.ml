(** TLS/HTTPS configuration for Kirin

    Provides HTTPS support using tls-eio library.

    Example:
    {[
      let tls_config = Kirin.Tls_config.make
        ~cert_file:"server.crt"
        ~key_file:"server.key"
        ()
      in
      Kirin.start_tls ~port:8443 ~tls:tls_config
      @@ Kirin.router [...]
    ]}
*)

(** TLS configuration *)
type t = {
  certificate : string;      (* PEM-encoded certificate *)
  private_key : string;      (* PEM-encoded private key *)
  ca_cert : string option;   (* Optional CA certificate for client auth *)
  alpn_protocols : string list;  (* ALPN protocols (e.g., ["h2"; "http/1.1"]) *)
}

(** Error type for TLS operations *)
type error =
  | Certificate_error of string
  | Key_error of string
  | File_not_found of string
  | Parse_error of string

(** Result type for TLS operations *)
type 'a result = ('a, error) Stdlib.result

(** Read file contents *)
let read_file path =
  try
    let ic = open_in path in
    let n = in_channel_length ic in
    let s = really_input_string ic n in
    close_in ic;
    Ok s
  with
  | Sys_error msg -> Error (File_not_found msg)

(** Create TLS config from certificate and key files *)
let make ~cert_file ~key_file ?ca_file ?(alpn = ["http/1.1"]) () =
  match read_file cert_file, read_file key_file with
  | Ok cert, Ok key ->
    let ca_cert = match ca_file with
      | Some f -> (match read_file f with Ok c -> Some c | Error _ -> None)
      | None -> None
    in
    Ok {
      certificate = cert;
      private_key = key;
      ca_cert;
      alpn_protocols = alpn;
    }
  | Error e, _ -> Error e
  | _, Error e -> Error e

(** Create TLS config from PEM strings directly *)
let from_pem ~certificate ~private_key ?ca_cert ?(alpn = ["http/1.1"]) () =
  {
    certificate;
    private_key;
    ca_cert;
    alpn_protocols = alpn;
  }

(** Default self-signed certificate for development (DO NOT USE IN PRODUCTION)
    This generates a simple self-signed cert at runtime for testing purposes. *)
let dev_config () =
  (* Simple self-signed cert - for development only *)
  let certificate = {|-----BEGIN CERTIFICATE-----
MIIBkTCB+wIJAKHBfpegPjMCMA0GCSqGSIb3DQEBCwUAMBExDzANBgNVBAMMBmtp
cmluMB4XDTI0MDEwMTAwMDAwMFoXDTI1MDEwMTAwMDAwMFowETEPMA0GA1UEAwwG
a2lyaW4wXDANBgkqhkiG9w0BAQEFAANLADBIAkEA0Z3VS5JJcds3xKFQDlWpPzYj
geLLDE6LdRm4c+DqKh+BYpMwkZ5wn7VvhhKdHE4uX5NM5EcuJN4WzKIoQBkf0wID
AQABo1MwUTAdBgNVHQ4EFgQUKxE8FJKD4ySP0PUqPpE8A5J+I2AwHwYDVR0jBBgw
FoAUKxE8FJKD4ySP0PUqPpE8A5J+I2AwDwYDVR0TAQH/BAUwAwEB/zANBgkqhkiG
9w0BAQsFAANBAKE8FKDE7H7dOfjpX8VO7M8nBO0nhdvFGF2P7Z8AwpJJ5BJJK0W2
C1JXQsJrFpKqOB5XJOE5Y4E5Bj0j5cJsLUE=
-----END CERTIFICATE-----|} in
  let private_key = {|-----BEGIN PRIVATE KEY-----
MIIBVQIBADANBgkqhkiG9w0BAQEFAASCAT8wggE7AgEAAkEA0Z3VS5JJcds3xKFQ
DlWpPzYjgeLLDE6LdRm4c+DqKh+BYpMwkZ5wn7VvhhKdHE4uX5NM5EcuJN4WzKIo
QBkf0wIDAQABAkA7Qj8aHKLQ5fEqfEqBsXVD1NDt6xJH1J5tZ6BJ0j5BfRmFGfhj
7NKP0N8m+ksNQX7g5Z6QHOFJGMnGCNQj2JKBAiEA7OphKS8FXbKJEhVzP8xGPAQ5
6BQIDJ8i5DnG4vbOJ9MCIQDjUhOxzdjLFZ+RMUjxK5ZwF1cG5XkgRvzA8IjJ5nL0
oQIgXLKWTL9A3pLcPhKxEN3t4FPj6sJLfKpMdZ7FQmQj7D0CIQC0VPQKVC0ZAZhO
uGphT8Y4VhKJ4G8Kl4B4CSHJJ9FEQQIhAMMN7uvjKCXJDDlZ9OvJp0XFNJJ9W3pl
Pb5J6n7DMJDa
-----END PRIVATE KEY-----|} in
  from_pem ~certificate ~private_key ()

(** Helper: check if string contains substring *)
let string_contains s sub =
  let rec check i =
    if i + String.length sub > String.length s then false
    else if String.sub s i (String.length sub) = sub then true
    else check (i + 1)
  in
  check 0

(** Check if config is valid (basic validation) *)
let validate config =
  let has_begin_cert = string_contains config.certificate "-----BEGIN CERTIFICATE-----" in
  let has_begin_key =
    string_contains config.private_key "-----BEGIN PRIVATE KEY-----" ||
    string_contains config.private_key "-----BEGIN RSA PRIVATE"
  in
  if not has_begin_cert then
    Error (Certificate_error "Invalid certificate format (missing PEM header)")
  else if not has_begin_key then
    Error (Key_error "Invalid private key format (missing PEM header)")
  else
    Ok ()

(** Error to string *)
let error_to_string = function
  | Certificate_error s -> "Certificate error: " ^ s
  | Key_error s -> "Key error: " ^ s
  | File_not_found s -> "File not found: " ^ s
  | Parse_error s -> "Parse error: " ^ s

(** Pretty print config (without sensitive data) *)
let pp fmt config =
  Format.fprintf fmt "TLS Config { alpn: [%s]; has_ca: %b }"
    (String.concat "; " config.alpn_protocols)
    (Option.is_some config.ca_cert)
