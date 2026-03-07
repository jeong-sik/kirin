(** Validation Format Patterns and Error Formatting

    Format validation patterns, validate_format, and error formatting
    functions extracted from Validation. *)

open Validation_schema

(** {1 Format Validators} *)

(* Note: OCaml Str module doesn't support \d, use [0-9] instead *)
let email_pattern = "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z][a-zA-Z]+$"
let uuid_pattern = "^[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]-[0-9a-f][0-9a-f][0-9a-f][0-9a-f]-[0-9a-f][0-9a-f][0-9a-f][0-9a-f]-[0-9a-f][0-9a-f][0-9a-f][0-9a-f]-[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]$"
let uri_pattern = "^https?://[^ /$.?#]"
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
    let re = Str.regexp p in
    if Str.string_match re value 0 then Ok ()
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
