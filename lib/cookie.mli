(** Cookie handling module.

    Provides parsing, setting, and deletion of HTTP cookies,
    including HMAC-signed cookies for tamper detection.

    @since 1.0.0
    @status stable *)

(** {1 Types} *)

(** Cookie attributes for the Set-Cookie header. *)
type attributes =
  { max_age : int option
  ; expires : string option
  ; domain : string option
  ; path : string option
  ; secure : bool
  ; http_only : bool
  ; same_site : [ `Strict | `Lax | `None ] option
  }

(** Default attributes: path=[/], httpOnly, sameSite=Lax. *)
val default_attributes : attributes

(** {1 Reading Cookies} *)

(** [parse_cookies header_value] parses a Cookie header into [(name, value)] pairs. *)
val parse_cookies : string -> (string * string) list

(** [get name req] returns the value of cookie [name] from the request. *)
val get : string -> Request.t -> string option

(** [get_all req] returns all cookies from the request. *)
val get_all : Request.t -> (string * string) list

(** {1 Setting Cookies} *)

(** [build_set_cookie name value attrs] builds a Set-Cookie header value. *)
val build_set_cookie : string -> string -> attributes -> string

(** [set ?attrs name value resp] adds a Set-Cookie header to the response. *)
val set : ?attrs:attributes -> string -> string -> Response.t -> Response.t

(** [delete ?path name resp] deletes a cookie by setting it to expire immediately. *)
val delete : ?path:string -> string -> Response.t -> Response.t

(** {1 Signed Cookies} *)

(** [set_secret key] sets the HMAC signing key (min 32 characters).
    @raise Failure if the key is too short. *)
val set_secret : string -> unit

(** [sign value] produces an HMAC-SHA256 signature for the given value.
    @raise Failure if the secret key is not set. *)
val sign : string -> string

(** [verify signed_value] verifies and extracts the original value
    from a signed cookie. Returns [None] if verification fails. *)
val verify : string -> string option

(** [get_signed name req] retrieves and verifies a signed cookie. *)
val get_signed : string -> Request.t -> string option

(** [set_signed ?attrs name value resp] sets a signed cookie on the response. *)
val set_signed : ?attrs:attributes -> string -> string -> Response.t -> Response.t
