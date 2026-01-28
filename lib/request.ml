(** Request module - Incoming HTTP request representation *)

open Eio

(** Path parameters extracted from route matching *)
type params = (string * string) list

(** Request type with all HTTP request data *)
type t = {
  meth : Http.Method.t;
  uri : Uri.t;
  headers : Http.Header.t;
  body_source : Buf_read.t;
  mutable cached_body : string option;
  params : params;
  (* Internal: raw http request for advanced use *)
  raw : Http.Request.t;
}

(** Create a new request from http components *)
let make ~raw ~body_source =
  let uri = Http.Request.resource raw |> Uri.of_string in
  let headers = Http.Request.headers raw in
  let meth = Http.Request.meth raw in
  { meth; uri; headers; body_source; cached_body = None; params = []; raw }

(** Get HTTP method *)
let meth t = t.meth

(** Get full URI *)
let uri t = t.uri

(** Get request path *)
let path t = Uri.path t.uri

(** Get all headers *)
let headers t = t.headers

(** Get a specific header *)
let header name t = Http.Header.get t.headers name

(** Get request body as string (Reads entire stream and caches it) *)
let body t =
  match t.cached_body with
  | Some b -> b
  | None ->
    let b = try 
      Buf_read.take_all t.body_source
    with End_of_file -> "" 
    in
    t.cached_body <- Some b;
    b

(** Get raw body source for streaming *)
let body_source t = t.body_source

(** Get path parameter by name *)
let param name t =
  List.assoc_opt name t.params

(** Get path parameter, raising if not found *)
let param_exn name t =
  match param name t with
  | Some v -> v
  | None -> failwith ("Missing path parameter: " ^ name)

(** Get query parameter by name *)
let query name t =
  Uri.get_query_param t.uri name

(** Get all query parameters for a key *)
let query_all name t =
  Uri.get_query_param' t.uri name

(** Internal: set path parameters after route matching *)
let with_params params t = { t with params }

(** Parse body as JSON *)
let json_body t =
  try Ok (Yojson.Safe.from_string (body t))
  with Yojson.Json_error msg -> Error (`Json_parse_error msg)

(** Parse body as form data (application/x-www-form-urlencoded) *)
let form_body t =
  Uri.query_of_encoded (body t)

(** Get content-type header *)
let content_type t = header "content-type" t

(** Check if request is JSON *)
let is_json t =
  match content_type t with
  | Some ct -> String.sub ct 0 (min 16 (String.length ct)) = "application/json"
  | None -> false
