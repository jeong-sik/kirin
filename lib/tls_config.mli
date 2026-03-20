(** TLS/HTTPS configuration for Kirin.

    Provides HTTPS support with PEM-based certificate and key loading.

    {b Example:}
    {[
      let tls_config = Tls_config.make
        ~cert_file:"server.crt"
        ~key_file:"server.key"
        ()
    ]} *)

(** {1 Types} *)

(** TLS configuration. *)
type t = {
  certificate : string;             (** PEM-encoded certificate. *)
  private_key : string;             (** PEM-encoded private key. *)
  ca_cert : string option;          (** Optional CA certificate for client auth. *)
  alpn_protocols : string list;     (** ALPN protocols (e.g., ["h2"; "http/1.1"]). *)
}

(** TLS operation errors. *)
type error =
  | Certificate_error of string
  | Key_error of string
  | File_not_found of string
  | Parse_error of string

(** Result type for TLS operations. *)
type 'a result = ('a, error) Stdlib.result

(** {1 Creation} *)

(** [make ~cert_file ~key_file ?ca_file ?alpn ()] creates a TLS config
    by loading PEM files from disk. Returns [Error] if files cannot be read.
    @param alpn ALPN protocol list (default: [["http/1.1"]]). *)
val make :
  cert_file:string ->
  key_file:string ->
  ?ca_file:string ->
  ?alpn:string list ->
  unit ->
  t result

(** [from_pem ~certificate ~private_key ?ca_cert ?alpn ()] creates a TLS
    config directly from PEM strings.
    @param alpn ALPN protocol list (default: [["http/1.1"]]). *)
val from_pem :
  certificate:string ->
  private_key:string ->
  ?ca_cert:string ->
  ?alpn:string list ->
  unit ->
  t

(** [dev_config ()] returns a self-signed TLS config for development.
    Do not use in production. *)
val dev_config : unit -> t

(** {1 Validation} *)

(** [validate config] checks that the certificate and private key
    contain valid PEM headers. Returns [Error] on invalid format. *)
val validate : t -> unit result

(** {1 Utilities} *)

(** [error_to_string error] converts a TLS error to a human-readable string. *)
val error_to_string : error -> string

(** [pp fmt config] pretty-prints the TLS config (without sensitive data). *)
val pp : Format.formatter -> t -> unit
