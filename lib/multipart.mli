(** Multipart form-data parsing for file uploads.

    Parses multipart/form-data request bodies into structured parts,
    supporting both form fields and file uploads.

    @since 1.0.0
    @status stable *)

(** {1 Types} *)

(** A single part of multipart data. *)
type part = {
  name : string;                    (** Form field name. *)
  filename : string option;         (** Original filename (for file uploads). *)
  content_type : string option;     (** MIME type (for file uploads). *)
  content : string;                 (** Raw content. *)
}

(** Parsed multipart form data. *)
type t

(** {1 Parsing} *)

(** Default cap on the number of parts [parse] will accumulate
    before refusing the whole body (1000).  Real forms never come
    close; the cap exists to bound DoS shapes that drive the
    splitter into many [String.sub] allocations. *)
val default_max_parts : int

(** [extract_boundary content_type] extracts the boundary string
    from a Content-Type header value.

    Returns [None] when the boundary parameter is absent, empty,
    longer than RFC 2046's 70-char limit, or contains bytes
    outside the spec's allowed character class.  A double-quoted
    boundary (e.g. [boundary="abc"]) is unquoted before the
    character-class check. *)
val extract_boundary : string -> string option

(** [parse ?max_parts ~boundary body] parses a multipart body
    using the given boundary delimiter.

    [max_parts] (default {!default_max_parts}) caps the number of
    fragments the splitter accumulates.  Once the cap is hit the
    entire result is discarded and an empty multipart is returned
    — *not* a truncated partial parse — because an attacker who
    drove the parser past the cap could otherwise steer
    downstream code through a partial-view shape. *)
val parse : ?max_parts:int -> boundary:string -> string -> t

(** [from_request req] parses multipart form data from a request.
    Returns [None] if the Content-Type is not multipart/form-data
    or the boundary cannot be extracted. *)
val from_request : Request.t -> t option

(** {1 Accessors} *)

(** [parts t] returns all parsed parts. *)
val parts : t -> part list

(** [field name t] returns the value of a non-file field by name. *)
val field : string -> t -> string option

(** [file name t] returns the file part with the given field name. *)
val file : string -> t -> part option

(** [files t] returns all parts that have a filename (file uploads). *)
val files : t -> part list

(** [fields t] returns all non-file fields as [(name, value)] pairs. *)
val fields : t -> (string * string) list
