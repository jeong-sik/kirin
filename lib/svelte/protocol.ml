(** Svelte SSR Protocol

    JSON-RPC 2.0 protocol for Svelte SSR worker communication. *)

(** {1 Protocol Types} *)

(** Request ID counter *)
let request_id = ref 0

(** Generate next request ID *)
let next_id () =
  incr request_id;
  !request_id

(** Render request *)
type render_request = {
  url: string;
  props: Yojson.Safe.t;
  route_id: string option;
  cookies: (string * string) list;
  headers: (string * string) list;
}

(** Render response *)
type render_response = {
  html: string;
  head: string;
  css: string option;
  error: string option;
  status: int;
  redirect: string option;
  data: Yojson.Safe.t option;
}

(** Protocol message *)
type message =
  | Request of int * string * Yojson.Safe.t
  | Response of int * Yojson.Safe.t
  | Error of int * int * string
  | Notification of string * Yojson.Safe.t

(** {1 Request Encoding} *)

(** Encode render request to JSON-RPC *)
let encode_render_request req =
  let id = next_id () in
  let params = `Assoc [
    ("url", `String req.url);
    ("props", req.props);
    ("routeId", match req.route_id with Some r -> `String r | None -> `Null);
    ("cookies", `Assoc (List.map (fun (k, v) -> (k, `String v)) req.cookies));
    ("headers", `Assoc (List.map (fun (k, v) -> (k, `String v)) req.headers));
  ] in
  let json = `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int id);
    ("method", `String "render");
    ("params", params);
  ] in
  (id, Yojson.Safe.to_string json ^ "\n")

(** Encode generic request *)
let encode_request method_ params =
  let id = next_id () in
  let json = `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int id);
    ("method", `String method_);
    ("params", params);
  ] in
  (id, Yojson.Safe.to_string json ^ "\n")

(** Encode notification (no response expected) *)
let encode_notification method_ params =
  let json = `Assoc [
    ("jsonrpc", `String "2.0");
    ("method", `String method_);
    ("params", params);
  ] in
  Yojson.Safe.to_string json ^ "\n"

(** {1 Response Decoding} *)

(** Decode render response *)
let decode_render_response json =
  let open Yojson.Safe.Util in
  let result = json |> member "result" in
  if result = `Null then
    let err_obj = json |> member "error" in
    let message = err_obj |> member "message" |> to_string in
    Result.Error (`String message)
  else
    let html = result |> member "html" |> to_string_option |> Option.value ~default:"" in
    let head = result |> member "head" |> to_string_option |> Option.value ~default:"" in
    let css = result |> member "css" |> to_string_option in
    let err_field = result |> member "error" |> to_string_option in
    let status = result |> member "status" |> to_int_option |> Option.value ~default:200 in
    let redirect = result |> member "redirect" |> to_string_option in
    let data = result |> member "data" in
    let data = if data = `Null then None else Some data in
    Result.Ok { html; head; css; error = err_field; status; redirect; data }

(** Decode generic response *)
let decode_response json =
  let open Yojson.Safe.Util in
  let id = json |> member "id" |> to_int in
  let result = json |> member "result" in
  if result <> `Null then
    Response (id, result)
  else
    let error = json |> member "error" in
    let code = error |> member "code" |> to_int in
    let message = error |> member "message" |> to_string in
    Error (id, code, message)

(** {1 Health Check} *)

(** Encode health check request *)
let encode_health_request () =
  encode_request "health" (`Assoc [])

(** Decode health response *)
let decode_health_response json =
  let open Yojson.Safe.Util in
  let result = json |> member "result" in
  if result = `Null then false
  else result |> member "ok" |> to_bool_option |> Option.value ~default:false

(** {1 Preload Request} *)

(** Encode preload request *)
let encode_preload_request urls =
  encode_request "preload" (`List (List.map (fun u -> `String u) urls))

(** {1 Invalidate Request} *)

(** Encode invalidation request *)
let encode_invalidate_request deps =
  encode_request "invalidate" (`List (List.map (fun d -> `String d) deps))

(** {1 SSR Context} *)

(** Encode context setting *)
let encode_set_context key value =
  encode_notification "setContext" (`Assoc [(key, value)])

(** {1 Error Codes} *)

(** JSON-RPC error codes *)
module ErrorCode = struct
  let parse_error = -32700
  let invalid_request = -32600
  let method_not_found = -32601
  let invalid_params = -32602
  let internal_error = -32603
  let render_error = -32000
  let timeout_error = -32001
end

(** Create error response *)
let error_response id code message =
  `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int id);
    ("error", `Assoc [
      ("code", `Int code);
      ("message", `String message);
    ]);
  ]

(** {1 Batch Requests} *)

(** Encode batch request *)
let encode_batch requests =
  let jsons = List.map (fun (method_, params) ->
    let id = next_id () in
    `Assoc [
      ("jsonrpc", `String "2.0");
      ("id", `Int id);
      ("method", `String method_);
      ("params", params);
    ]
  ) requests in
  Yojson.Safe.to_string (`List jsons) ^ "\n"

(** Decode batch response *)
let decode_batch json =
  let open Yojson.Safe.Util in
  json |> to_list |> List.map decode_response
