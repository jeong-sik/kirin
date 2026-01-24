(** Qwik SSR Protocol

    JSON-RPC 2.0 for Qwik SSR worker communication. *)

(** {1 Request Types} *)

(** Render request *)
type render_request = {
  url: string;
  base: string;
  headers: (string * string) list;
  container_attrs: (string * string) list;
  server_data: Yojson.Safe.t option;
}

(** {1 Response Types} *)

(** Render response *)
type render_response = {
  html: string;
  timing: timing;
  head: Meta.t option;
  container_state: Container.t option;
}

and timing = {
  render_ms: float;
  serialize_ms: float;
}

(** Protocol response *)
type response =
  | Success of { id: int; result: render_response }
  | Failure of { id: int; code: int; message: string }

(** {1 Request Construction} *)

(** Create render request *)
let render_request ~url ?(base="/") ?(headers=[]) ?(container_attrs=[]) ?server_data () = {
  url;
  base;
  headers;
  container_attrs;
  server_data;
}

(** {1 Encoding} *)

(** Encode render request to JSON-RPC *)
let encode_render ~id request =
  let params = `Assoc [
    ("url", `String request.url);
    ("base", `String request.base);
    ("headers", `Assoc (List.map (fun (k, v) -> (k, `String v)) request.headers));
    ("containerAttrs", `Assoc (List.map (fun (k, v) -> (k, `String v)) request.container_attrs));
    ("serverData", match request.server_data with Some d -> d | None -> `Null);
  ] in
  let rpc = `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int id);
    ("method", `String "render");
    ("params", params);
  ] in
  Yojson.Safe.to_string rpc

(** Encode prefetch request *)
let encode_prefetch ~id ~qrls =
  let params = `Assoc [
    ("qrls", `List (List.map (fun q -> `String q) qrls));
  ] in
  let rpc = `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int id);
    ("method", `String "prefetch");
    ("params", params);
  ] in
  Yojson.Safe.to_string rpc

(** {1 Decoding} *)

(** Decode timing from JSON *)
let decode_timing json =
  let open Yojson.Safe.Util in
  {
    render_ms = json |> member "renderMs" |> to_float;
    serialize_ms = json |> member "serializeMs" |> to_float;
  }

(** Decode response from JSON-RPC string *)
let decode_response str =
  try
    let json = Yojson.Safe.from_string str in
    let open Yojson.Safe.Util in
    let id = json |> member "id" |> to_int in

    match json |> member "error" with
    | `Null ->
      let result = json |> member "result" in
      let html = result |> member "html" |> to_string in
      let timing = result |> member "timing" |> decode_timing in
      Success {
        id;
        result = {
          html;
          timing;
          head = None;  (* Would parse from result if present *)
          container_state = None;  (* Would parse from result if present *)
        };
      }
    | error ->
      let code = error |> member "code" |> to_int in
      let message = error |> member "message" |> to_string in
      Failure { id; code; message }
  with e ->
    Failure { id = 0; code = -32700; message = Printexc.to_string e }

(** {1 Health Check} *)

(** Encode health request *)
let encode_health ~id =
  let rpc = `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int id);
    ("method", `String "health");
    ("params", `Assoc []);
  ] in
  Yojson.Safe.to_string rpc

(** {1 Batch Requests} *)

(** Encode batch request *)
let encode_batch requests =
  let json = `List (List.map (fun (id, req) ->
    Yojson.Safe.from_string (encode_render ~id req)
  ) requests) in
  Yojson.Safe.to_string json

(** Decode batch response *)
let decode_batch str =
  try
    let json = Yojson.Safe.from_string str in
    match json with
    | `List items ->
      List.map (fun item -> decode_response (Yojson.Safe.to_string item)) items
    | _ -> [Failure { id = 0; code = -32600; message = "Invalid batch response" }]
  with e ->
    [Failure { id = 0; code = -32700; message = Printexc.to_string e }]

(** {1 Serialization} *)

(** Response to JSON *)
let response_to_json = function
  | Success { id; result } ->
    `Assoc [
      ("jsonrpc", `String "2.0");
      ("id", `Int id);
      ("result", `Assoc [
        ("html", `String result.html);
        ("timing", `Assoc [
          ("renderMs", `Float result.timing.render_ms);
          ("serializeMs", `Float result.timing.serialize_ms);
        ]);
      ]);
    ]
  | Failure { id; code; message } ->
    `Assoc [
      ("jsonrpc", `String "2.0");
      ("id", `Int id);
      ("error", `Assoc [
        ("code", `Int code);
        ("message", `String message);
      ]);
    ]
