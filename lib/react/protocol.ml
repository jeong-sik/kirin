(** JSON-RPC 2.0 Protocol for SSR Communication

    Protocol between Kirin and Node.js SSR workers.
    Uses stdio for communication (shell injection safe).
*)

(** JSON-RPC request *)
type request = {
  id: int;
  method_: string;
  params: Yojson.Safe.t;
}

(** JSON-RPC response variants *)
type response =
  | Success of { id: int; result: Yojson.Safe.t }
  | Error of { id: int; code: int; message: string; data: Yojson.Safe.t option }

(** Standard JSON-RPC error codes *)
module Error_code = struct
  let parse_error = -32700
  let invalid_request = -32600
  let method_not_found = -32601
  let invalid_params = -32602
  let internal_error = -32603

  (* Custom error codes for SSR *)
  let render_error = -32000
  let timeout_error = -32001
  let memory_error = -32002
end

(** Encode request to JSON string *)
let encode_request req =
  let json = `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int req.id);
    ("method", `String req.method_);
    ("params", req.params);
  ] in
  Yojson.Safe.to_string json

(** Decode response from JSON string *)
let decode_response s =
  try
    let json = Yojson.Safe.from_string s in
    match json with
    | `Assoc fields ->
      let id = match List.assoc_opt "id" fields with
        | Some (`Int i) -> i
        | Some `Null -> 0
        | _ -> 0
      in
      (match List.assoc_opt "error" fields with
      | Some (`Assoc err_fields) ->
        let code = match List.assoc_opt "code" err_fields with
          | Some (`Int c) -> c
          | _ -> Error_code.internal_error
        in
        let message = match List.assoc_opt "message" err_fields with
          | Some (`String m) -> m
          | _ -> "Unknown error"
        in
        let data = List.assoc_opt "data" err_fields in
        Ok (Error { id; code; message; data })
      | _ ->
        let result = match List.assoc_opt "result" fields with
          | Some r -> r
          | None -> `Null
        in
        Ok (Success { id; result }))
    | _ -> Result.Error "Invalid JSON-RPC response: not an object"
  with
  | Yojson.Json_error msg -> Result.Error ("JSON parse error: " ^ msg)

(** Create render request *)
let render_request ~id ~url ?(props = `Assoc []) () =
  {
    id;
    method_ = "render";
    params = `Assoc [
      ("url", `String url);
      ("props", props);
    ];
  }

(** Create streaming render request *)
let stream_request ~id ~url ?(props = `Assoc []) () =
  {
    id;
    method_ = "stream";
    params = `Assoc [
      ("url", `String url);
      ("props", props);
    ];
  }

(** Create health check request *)
let health_request ~id =
  {
    id;
    method_ = "health";
    params = `Assoc [];
  }

(** Create shutdown request *)
let shutdown_request ~id =
  {
    id;
    method_ = "shutdown";
    params = `Assoc [];
  }

(** Extract HTML from successful render response *)
let extract_html response =
  match response with
  | Success { result; _ } ->
    (match result with
    | `Assoc fields ->
      (match List.assoc_opt "html" fields with
      | Some (`String html) -> Ok html
      | _ -> Result.Error "Missing html field in response")
    | `String html -> Ok html
    | _ -> Result.Error "Invalid result format")
  | Error { message; code; _ } ->
    Result.Error (Printf.sprintf "SSR error %d: %s" code message)

(** Extract head tags from response *)
let extract_head response =
  match response with
  | Success { result; _ } ->
    (match result with
    | `Assoc fields ->
      (match List.assoc_opt "head" fields with
      | Some (`String head) -> Some head
      | _ -> None)
    | _ -> None)
  | Error _ -> None

(** Full render result *)
type render_result = {
  html: string;
  head: string option;
  status: int option;
  redirect: string option;
}

let extract_render_result response =
  match response with
  | Success { result; _ } ->
    (match result with
    | `Assoc fields ->
      let html = match List.assoc_opt "html" fields with
        | Some (`String h) -> h
        | _ -> ""
      in
      let head = match List.assoc_opt "head" fields with
        | Some (`String h) -> Some h
        | _ -> None
      in
      let status = match List.assoc_opt "status" fields with
        | Some (`Int s) -> Some s
        | _ -> None
      in
      let redirect = match List.assoc_opt "redirect" fields with
        | Some (`String r) -> Some r
        | _ -> None
      in
      Ok { html; head; status; redirect }
    | _ -> Result.Error "Invalid result format")
  | Error { message; code; _ } ->
    Result.Error (Printf.sprintf "SSR error %d: %s" code message)

(** Request ID generator *)
let next_id =
  let counter = ref 0 in
  fun () ->
    incr counter;
    !counter

(** Batch request encoding *)
let encode_batch requests =
  let json = `List (List.map (fun req ->
    `Assoc [
      ("jsonrpc", `String "2.0");
      ("id", `Int req.id);
      ("method", `String req.method_);
      ("params", req.params);
    ]
  ) requests) in
  Yojson.Safe.to_string json

(** Timeout wrapper for response *)
let with_timeout ~timeout_ms f =
  (* This would use Eio.Time in actual implementation *)
  ignore timeout_ms;
  f ()
