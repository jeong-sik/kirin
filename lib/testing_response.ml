(** Test Response

    Extracted from Testing.Test_response. *)

(** Test response *)
type t = {
  status : Http.Status.t;
  headers : (string * string) list;
  body : string;
}

(** Get status *)
let status resp = resp.status

(** Get status code as int *)
let status_code resp = Http.Status.to_int resp.status

(** Get header *)
let header name resp =
  List.assoc_opt (String.lowercase_ascii name)
    (List.map (fun (k, v) -> (String.lowercase_ascii k, v)) resp.headers)

(** Get all headers *)
let headers resp = resp.headers

(** Get body as string *)
let body resp = resp.body

(** Get body as JSON *)
let json resp =
  try Some (Yojson.Safe.from_string resp.body)
  with Yojson.Json_error _ -> None

(** Check if response is success (2xx) *)
let is_success resp =
  let code = Http.Status.to_int resp.status in
  code >= 200 && code < 300

(** Check if response is redirect (3xx) *)
let is_redirect resp =
  let code = Http.Status.to_int resp.status in
  code >= 300 && code < 400

(** Check if response is client error (4xx) *)
let is_client_error resp =
  let code = Http.Status.to_int resp.status in
  code >= 400 && code < 500

(** Check if response is server error (5xx) *)
let is_server_error resp =
  let code = Http.Status.to_int resp.status in
  code >= 500 && code < 600
