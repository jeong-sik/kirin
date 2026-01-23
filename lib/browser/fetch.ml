(** Kirin Browser - Fetch API Bindings

    Direct-style HTTP client using browser Fetch API.
    Uses Promise effects for direct-style async.

    {b Example:}
    {[
      Promise.run (fun () ->
        let response = Fetch.get "/api/users" in
        if Fetch.Response.is_ok response then
          let json = Fetch.Response.json response in
          Console.log json
      )
    ]}
*)

open Js_of_ocaml

(** {1 Types} *)

(** HTTP methods *)
type http_method =
  | GET
  | POST
  | PUT
  | PATCH
  | DELETE
  | HEAD
  | OPTIONS

(** Request init options *)
type request_init = {
  method_ : http_method;
  headers : (string * string) list;
  body : string option;
  credentials : [ `omit | `same_origin | `include_ ];
  mode : [ `cors | `no_cors | `same_origin ];
}

(** Response type *)
type response = {
  ok : bool;
  status : int;
  status_text : string;
  url : string;
  js_response : Js.Unsafe.any;
}

(** {1 Internal Helpers} *)

let method_to_string = function
  | GET -> "GET"
  | POST -> "POST"
  | PUT -> "PUT"
  | PATCH -> "PATCH"
  | DELETE -> "DELETE"
  | HEAD -> "HEAD"
  | OPTIONS -> "OPTIONS"

let credentials_to_string = function
  | `omit -> "omit"
  | `same_origin -> "same-origin"
  | `include_ -> "include"

let mode_to_string = function
  | `cors -> "cors"
  | `no_cors -> "no-cors"
  | `same_origin -> "same-origin"

(** {1 Default Options} *)

let default_init = {
  method_ = GET;
  headers = [];
  body = None;
  credentials = `same_origin;
  mode = `cors;
}

(** {1 Response Module} *)

module Response = struct
  (** Get response body as text (direct-style) *)
  let text (resp : response) : string =
    let promise = Js.Unsafe.meth_call resp.js_response "text" [||] in
    let result = Promise.await (Promise.of_js promise) in
    Js.to_string result

  (** Get response as JSON (returns Yojson.Safe.t) *)
  let json (resp : response) : Yojson.Safe.t =
    let body = text resp in
    Yojson.Safe.from_string body

  (** Get response as raw JS value *)
  let json_raw (resp : response) : Js.Unsafe.any =
    let promise = Js.Unsafe.meth_call resp.js_response "json" [||] in
    Promise.await (Promise.of_js promise)

  (** Check if response is OK (2xx status) *)
  let is_ok resp = resp.ok

  (** Get status code *)
  let status resp = resp.status

  (** Get status text *)
  let status_text resp = resp.status_text

  (** Get final URL (after redirects) *)
  let url resp = resp.url

  (** Get a header value *)
  let header name (resp : response) : string option =
    let headers_obj = Js.Unsafe.get resp.js_response (Js.string "headers") in
    let result = Js.Unsafe.meth_call headers_obj "get" [| Js.Unsafe.inject (Js.string name) |] in
    if Js.Opt.test (Js.Opt.return result) && not (Js.Unsafe.equals result Js.null) then
      Some (Js.to_string result)
    else
      None
end

(** {1 Fetch Functions} *)

(** Low-level fetch with full options (direct-style) *)
let fetch_raw url (init : request_init) : response =
  (* Build headers object *)
  let headers_obj = Js.Unsafe.obj [||] in
  List.iter (fun (k, v) ->
    Js.Unsafe.set headers_obj (Js.string k) (Js.string v)
  ) init.headers;

  (* Build init object *)
  let init_obj = Js.Unsafe.obj [|
    ("method", Js.Unsafe.inject (Js.string (method_to_string init.method_)));
    ("headers", Js.Unsafe.inject headers_obj);
    ("credentials", Js.Unsafe.inject (Js.string (credentials_to_string init.credentials)));
    ("mode", Js.Unsafe.inject (Js.string (mode_to_string init.mode)));
  |] in

  (* Add body if present *)
  (match init.body with
   | Some body -> Js.Unsafe.set init_obj (Js.string "body") (Js.string body)
   | None -> ());

  (* Call fetch and await *)
  let promise = Js.Unsafe.global##fetch (Js.string url) init_obj in
  let js_response = Promise.await (Promise.of_js promise) in

  (* Extract response fields *)
  {
    ok = Js.to_bool (Js.Unsafe.get js_response (Js.string "ok"));
    status = Js.Unsafe.get js_response (Js.string "status");
    status_text = Js.to_string (Js.Unsafe.get js_response (Js.string "statusText"));
    url = Js.to_string (Js.Unsafe.get js_response (Js.string "url"));
    js_response;
  }

(** Fetch with default options *)
let fetch ?(init = default_init) url =
  fetch_raw url init

(** GET request *)
let get ?headers url =
  let headers = Option.value ~default:[] headers in
  fetch_raw url { default_init with method_ = GET; headers }

(** POST request with body *)
let post ?headers ~body url =
  let headers = ("Content-Type", "application/json") :: Option.value ~default:[] headers in
  fetch_raw url { default_init with method_ = POST; headers; body = Some body }

(** PUT request with body *)
let put ?headers ~body url =
  let headers = ("Content-Type", "application/json") :: Option.value ~default:[] headers in
  fetch_raw url { default_init with method_ = PUT; headers; body = Some body }

(** PATCH request with body *)
let patch ?headers ~body url =
  let headers = ("Content-Type", "application/json") :: Option.value ~default:[] headers in
  fetch_raw url { default_init with method_ = PATCH; headers; body = Some body }

(** DELETE request *)
let delete ?headers url =
  let headers = Option.value ~default:[] headers in
  fetch_raw url { default_init with method_ = DELETE; headers }

(** {1 JSON Helpers} *)

(** POST JSON and get JSON response *)
let post_json ?headers ~body url =
  let body_str = Yojson.Safe.to_string body in
  let resp = post ?headers ~body:body_str url in
  if Response.is_ok resp then
    Ok (Response.json resp)
  else
    Error (resp.status, resp.status_text)

(** GET and parse JSON response *)
let get_json ?headers url =
  let resp = get ?headers url in
  if Response.is_ok resp then
    Ok (Response.json resp)
  else
    Error (resp.status, resp.status_text)

(** PUT JSON and get JSON response *)
let put_json ?headers ~body url =
  let body_str = Yojson.Safe.to_string body in
  let resp = put ?headers ~body:body_str url in
  if Response.is_ok resp then
    Ok (Response.json resp)
  else
    Error (resp.status, resp.status_text)

(** DELETE and get JSON response *)
let delete_json ?headers url =
  let resp = delete ?headers url in
  if Response.is_ok resp then
    Ok (Response.json resp)
  else
    Error (resp.status, resp.status_text)
