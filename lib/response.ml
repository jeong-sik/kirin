(** Response module - HTTP response construction *)

(** Response type *)
type t = {
  status : Http.Status.t;
  headers : Http.Header.t;
  body : string;
}

(** Create a basic response *)
let make ?(status = `OK) ?(headers = Http.Header.init ()) body =
  { status; headers; body }

(** Get status code *)
let status t = t.status

(** Get headers *)
let headers t = t.headers

(** Get body *)
let body t = t.body

(** Add a header to response *)
let with_header name value t =
  let headers = Http.Header.add t.headers name value in
  { t with headers }

(** Add multiple headers *)
let with_headers hs t =
  let headers = List.fold_left (fun h (n, v) -> Http.Header.add h n v) t.headers hs in
  { t with headers }

(** Set status code *)
let with_status status t =
  { t with status }

(** ---- Response Helpers ---- *)

(** Plain text response *)
let text ?(status = `OK) body =
  make ~status body
  |> with_header "content-type" "text/plain; charset=utf-8"

(** HTML response *)
let html ?(status = `OK) ?(doctype = false) body =
  let body = if doctype then "<!DOCTYPE html>\n" ^ body else body in
  make ~status body
  |> with_header "content-type" "text/html; charset=utf-8"

(** JSON response *)
let json ?(status = `OK) body =
  let body = Yojson.Safe.to_string body in
  make ~status body
  |> with_header "content-type" "application/json; charset=utf-8"

(** JSON from string (already serialized) *)
let json_string ?(status = `OK) body =
  make ~status body
  |> with_header "content-type" "application/json; charset=utf-8"

(** Empty response with just status *)
let empty status =
  make ~status ""

(** Redirect response *)
let redirect ?(status = `Found) location =
  make ~status ""
  |> with_header "location" location

(** Permanent redirect (301) *)
let redirect_permanent location =
  redirect ~status:`Moved_permanently location

(** Not found response *)
let not_found ?(body = "Not Found") () =
  text ~status:`Not_found body

(** Bad request response *)
let bad_request ?(body = "Bad Request") () =
  text ~status:`Bad_request body

(** Internal server error response *)
let server_error ?(body = "Internal Server Error") () =
  text ~status:`Internal_server_error body

(** HTMX-specific response with swap headers *)
let htmx ?(status = `OK) ?target ?swap body =
  let resp = html ~status body in
  let resp = match target with
    | Some t -> with_header "HX-Retarget" t resp
    | None -> resp
  in
  match swap with
  | Some s -> with_header "HX-Reswap" s resp
  | None -> resp
