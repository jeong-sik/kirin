(** Preact SSR Protocol

    JSON-RPC 2.0 protocol for Preact server-side rendering.
    Used for communication between OCaml server and Node.js worker. *)

(** {1 Protocol Types} *)

(** Request ID *)
type request_id = int

(** JSON-RPC request *)
type request = {
  id: request_id;
  method_: string;
  params: Yojson.Safe.t;
}

(** JSON-RPC success response *)
type success_response = {
  success_id: request_id;
  result: Yojson.Safe.t;
}

(** JSON-RPC error detail *)
type error_detail = {
  code: int;
  message: string;
  data: Yojson.Safe.t option;
}

(** JSON-RPC error response *)
type failure_response = {
  failure_id: request_id option;
  error: error_detail;
}

(** JSON-RPC response *)
type response =
  | RpcSuccess of success_response
  | RpcError of failure_response

(** {1 Error Codes} *)

(** Standard JSON-RPC error codes *)
let parse_error = -32700
let invalid_request = -32600
let method_not_found = -32601
let invalid_params = -32602
let internal_error = -32603

(** Custom error codes *)
let render_error = -32000
let timeout_error = -32001
let component_error = -32002
let signals_error = -32003

(** {1 Request Construction} *)

(** Counter for request IDs *)
let request_counter = ref 0

(** Generate next request ID *)
let next_id () =
  incr request_counter;
  !request_counter

(** Create render request *)
let render_request ~url ?(props = `Null) ?(signals = `Null) () =
  let params = `Assoc [
    ("url", `String url);
    ("props", props);
    ("signals", signals);
  ] in
  { id = next_id (); method_ = "render"; params }

(** Create health check request *)
let health_request () =
  { id = next_id (); method_ = "health"; params = `Null }

(** Create shutdown request *)
let shutdown_request () =
  { id = next_id (); method_ = "shutdown"; params = `Null }

(** {1 Encoding} *)

(** Encode request to JSON string *)
let encode_request req =
  let json = `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int req.id);
    ("method", `String req.method_);
    ("params", req.params);
  ] in
  Yojson.Safe.to_string json

(** Encode error detail to JSON *)
let encode_error_detail err =
  let base = [
    ("code", `Int err.code);
    ("message", `String err.message);
  ] in
  let with_data = match err.data with
    | Some d -> base @ [("data", d)]
    | None -> base
  in
  `Assoc with_data

(** Encode response to JSON string *)
let encode_response resp =
  let json = match resp with
    | RpcSuccess s ->
      `Assoc [
        ("jsonrpc", `String "2.0");
        ("id", `Int s.success_id);
        ("result", s.result);
      ]
    | RpcError e ->
      `Assoc [
        ("jsonrpc", `String "2.0");
        ("id", match e.failure_id with Some i -> `Int i | None -> `Null);
        ("error", encode_error_detail e.error);
      ]
  in
  Yojson.Safe.to_string json

(** {1 Decoding} *)

(** Decode response from JSON string *)
let decode_response str =
  try
    let json = Yojson.Safe.from_string str in
    let open Yojson.Safe.Util in
    let id = json |> member "id" |> to_int_option in
    let result = json |> member "result" in
    let err = json |> member "error" in
    match result, err with
    | `Null, `Null ->
      let fail = { failure_id = id; error = { code = parse_error; message = "Invalid response"; data = None } } in
      Ok (RpcError fail)
    | _, `Null ->
      (match id with
       | Some i -> Ok (RpcSuccess { success_id = i; result })
       | None ->
         let fail = { failure_id = None; error = { code = invalid_request; message = "Missing id"; data = None } } in
         Ok (RpcError fail))
    | `Null, error_json ->
      let code = error_json |> member "code" |> to_int in
      let message = error_json |> member "message" |> to_string in
      let data = match error_json |> member "data" with
        | `Null -> None
        | d -> Some d
      in
      Ok (RpcError { failure_id = id; error = { code; message; data } })
    | _, _ ->
      let fail = { failure_id = id; error = { code = parse_error; message = "Ambiguous response"; data = None } } in
      Ok (RpcError fail)
  with
  | Yojson.Json_error msg ->
    let fail = { failure_id = None; error = { code = parse_error; message = msg; data = None } } in
    Ok (RpcError fail)
  | _ ->
    let fail = { failure_id = None; error = { code = internal_error; message = "Unknown error"; data = None } } in
    Ok (RpcError fail)

(** {1 Result Extraction} *)

(** Extract HTML from render result *)
let extract_html result =
  let open Yojson.Safe.Util in
  result |> member "html" |> to_string_option

(** Extract head tags from render result *)
let extract_head result =
  let open Yojson.Safe.Util in
  result |> member "head" |> to_string_option

(** Extract signals state from render result *)
let extract_signals result =
  let open Yojson.Safe.Util in
  match result |> member "signals" with
  | `Null -> None
  | s -> Some s

(** {1 Serialization} *)

(** Request to JSON *)
let request_to_json req =
  `Assoc [
    ("id", `Int req.id);
    ("method", `String req.method_);
    ("params", req.params);
  ]

(** Response to JSON *)
let response_to_json = function
  | RpcSuccess s ->
    `Assoc [
      ("type", `String "success");
      ("id", `Int s.success_id);
      ("result", s.result);
    ]
  | RpcError e ->
    `Assoc [
      ("type", `String "error");
      ("id", match e.failure_id with Some i -> `Int i | None -> `Null);
      ("error", encode_error_detail e.error);
    ]
