(** Response compression middleware.

    Supports gzip and deflate compression. Automatically negotiates encoding
    via the Accept-Encoding header. Skips already-compressed content types
    (images, video, audio, archives) and payloads below a minimum size.

    @since 1.0.0
    @status stable *)

(** {1 Types} *)

(** Compression algorithms. *)
type algorithm =
  | Gzip
  | Deflate

(** {1 Encoding Negotiation} *)

(** [parse_accept_encoding header_value] parses an Accept-Encoding header
    and returns the preferred algorithm (gzip > deflate). *)
val parse_accept_encoding : string -> algorithm option

(** [skip_compression_for content_type] returns [true] for content types
    that should not be compressed (images, video, audio, archives). *)
val skip_compression_for : string -> bool

(** {1 Compression} *)

(** [compress_gzip input] compresses a string using gzip. *)
val compress_gzip : string -> string

(** [compress_deflate input] compresses a string using deflate (zlib). *)
val compress_deflate : string -> string

(** [compress algo input] compresses using the specified algorithm. *)
val compress : algorithm -> string -> string

(** [content_encoding_of algo] returns the Content-Encoding header value
    for the given algorithm (["gzip"] or ["deflate"]). *)
val content_encoding_of : algorithm -> string

(** {1 Configuration} *)

(** Minimum body size in bytes to apply compression (default: 1024).
    Payloads smaller than this are not compressed. *)
val min_compress_size : int

(** {1 Middleware} *)

(** [middleware ?min_size handler] applies response compression.
    Compresses string bodies above [min_size] bytes when the client
    supports it. Streaming and producer bodies are passed through unchanged. *)
val middleware : ?min_size:int -> (Request.t -> Response.t) -> Request.t -> Response.t
