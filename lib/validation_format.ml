(** Validation Format Patterns and Error Formatting

    Format validation patterns, validate_format, and error formatting
    functions extracted from Validation. *)

open Validation_schema

(** {1 Format Validators} *)

(* Note: OCaml Str module doesn't support \d, use [0-9] instead *)
let email_pattern = "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z][a-zA-Z]+$"
let uuid_pattern = "^[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]-[0-9a-f][0-9a-f][0-9a-f][0-9a-f]-[0-9a-f][0-9a-f][0-9a-f][0-9a-f]-[0-9a-f][0-9a-f][0-9a-f][0-9a-f]-[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]$"
(* The old pattern was "^https?://[^ /$.?#]" — anchored at the start
   but with no end anchor, so [Str.string_match re value 0] returned
   true on any URI-shaped *prefix*. Together with the multiline [$]
   trap, "https://x.com\nINJECTED" was a passing URI. The new pattern
   requires every byte of the value to be inside a single
   whitespace-free URI shape:

     - scheme: http or https + "://"
     - one host-leading char that is not whitespace, not "/", and not
       one of $.?# (preserves the original spirit of the first-char
       gate)
     - the rest of the string, also whitespace-free (this is what
       lets [Str.match_end ()] cover the entire input rather than a
       9-char prefix). *)
let uri_pattern = "^https?://[^ \t\r\n/$.?#][^ \t\r\n]*$"
let date_pattern = "^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]$"
let datetime_pattern = "^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]T[0-9][0-9]:[0-9][0-9]:[0-9][0-9]"

let validate_format format value =
  let pattern = match format with
    | "email" -> Some email_pattern
    | "uuid" -> Some uuid_pattern
    | "uri" | "url" -> Some uri_pattern
    | "date" -> Some date_pattern
    | "date-time" | "datetime" -> Some datetime_pattern
    | _ -> None
  in
  match pattern with
  | None -> Ok ()
  | Some p ->
    (* Two OCaml [Str] traps live in this function and both produce
       false-accepts that downstream code can turn into CR/LF
       injection or open-redirects:

       1. [Str]'s [^] and [$] anchors are *multiline* — they match
          at the start/end of the string OR at newline boundaries.
          So "^email$" accepts "user@x.com\nSet-Cookie: evil=1":
          [^] matches the string start, the email body matches, and
          [$] matches the position before the embedded "\n". A
          caller that puts the validated value into a header / log
          line / email subject inherits the injection.

       2. [Str.string_match re value 0] returns true on any match
          *starting at* position 0 — it does not require the match
          to cover the entire string. Combined with [$]'s multiline
          semantics, a pattern meant to gate the whole input can
          accept a prefix.

       Defense for (1): reject embedded CR / LF / NUL up front.
       Defense for (2): require [Str.match_end ()] to equal
       [String.length value] — the regex must consume the entire
       input. Together they make "^...$" behave the way a reader
       intuitively expects. *)
    if String.contains value '\n'
       || String.contains value '\r'
       || String.contains value '\x00'
    then Error (Printf.sprintf "Invalid %s format" format)
    else
      let re = Str.regexp p in
      if Str.string_match re value 0
         && Str.match_end () = String.length value
      then Ok ()
      else Error (Printf.sprintf "Invalid %s format" format)

(** {1 Error Formatting} *)

(** Format error path *)
let format_path path =
  match path with
  | [] -> "root"
  | _ -> String.concat "." path

(** Error to JSON *)
let error_to_json err =
  `Assoc [
    ("path", `String (format_path err.path));
    ("message", `String err.message);
    ("code", `String err.code);
  ]

(** Errors to JSON *)
let errors_to_json errors =
  `Assoc [
    ("errors", `List (List.map error_to_json errors));
  ]

(** Error to string *)
let error_to_string err =
  Printf.sprintf "%s: %s (%s)" (format_path err.path) err.message err.code

(** Errors to string *)
let errors_to_string errors =
  String.concat "\n" (List.map error_to_string errors)
