(** Static file serving middleware.

    Serves files from a local directory under a URL prefix.
    Includes MIME type detection and directory traversal protection.

    @since 1.0.0
    @status stable *)

(** {1 MIME Types} *)

(** [mime_of_ext ext] returns the MIME type for a file extension
    (e.g., [".html"] -> ["text/html; charset=utf-8"]).
    Returns ["application/octet-stream"] for unknown extensions. *)
val mime_of_ext : string -> string

(** [get_mime_type path] returns the MIME type based on the file's extension. *)
val get_mime_type : string -> string

(** {1 Path Safety} *)

(** [is_safe_path path] returns [true] if the path does not contain [..] segments.
    Used to prevent directory traversal attacks. *)
val is_safe_path : string -> bool

(** {1 Middleware} *)

(** [static prefix ~dir next req] serves static files from [dir] for requests
    whose path starts with [prefix]. Falls through to [next] if the path
    does not match or the file does not exist.

    Returns 403 Forbidden if the path contains directory traversal attempts.

    {b Example:}
    {[
      static "/assets" ~dir:"./public" next req
    ]} *)
val static : string -> dir:string -> (Request.t -> Response.t) -> Request.t -> Response.t
