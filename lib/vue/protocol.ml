(** Vue/Nuxt SSR Protocol

    JSON-RPC 2.0 protocol for communicating with Node.js SSR workers. *)

(** {1 Request Types} *)

(** SSR render request *)
type render_request = {
  url: string;
  headers: (string * string) list;
  payload: Yojson.Safe.t option;
}

(** Request methods *)
type method_ =
  | Render           (* Render page to HTML *)
  | RenderStream     (* Render with streaming *)
  | Prerender        (* Static generation *)
  | Health           (* Health check *)
  | Warmup           (* Warmup worker *)

(** {1 Response Types} *)

(** Render result *)
type render_result = {
  html: string;
  head: string;
  payload_json: string option;
  status: int;
  redirect: string option;
}

(** Stream chunk *)
type stream_chunk =
  | Shell of string         (* Initial shell *)
  | Content of string       (* Streamed content *)
  | Error of string         (* Error message *)
  | Done                    (* Stream complete *)

(** Health check result *)
type health_result = {
  ready: bool;
  requests: int;
  memory_mb: int;
}

(** Response types *)
type response_result =
  | RenderOk of render_result
  | StreamStart of string    (* Stream ID *)
  | HealthOk of health_result
  | WarmupOk

(** {1 JSON-RPC 2.0} *)

(** JSON-RPC request *)
type request = {
  jsonrpc: string;  (* "2.0" *)
  id: int;
  method_name: string;
  params: Yojson.Safe.t;
}

(** JSON-RPC error *)
type rpc_error = {
  code: int;
  message: string;
  data: Yojson.Safe.t option;
}

(** JSON-RPC response *)
type response =
  | Success of {
      id: int;
      result: Yojson.Safe.t;
    }
  | Failure of {
      id: int option;
      error: rpc_error;
    }

(** {1 Standard Error Codes} *)

let parse_error = -32700
let invalid_request = -32600
let method_not_found = -32601
let invalid_params = -32602
let internal_error = -32603

(* Application errors *)
let render_error = -32000
let timeout_error = -32001
let not_found_error = -32002

(** {1 Encoding} *)

(** Method to string *)
let method_to_string = function
  | Render -> "render"
  | RenderStream -> "renderStream"
  | Prerender -> "prerender"
  | Health -> "health"
  | Warmup -> "warmup"

(** String to method *)
let method_of_string = function
  | "render" -> Some Render
  | "renderStream" -> Some RenderStream
  | "prerender" -> Some Prerender
  | "health" -> Some Health
  | "warmup" -> Some Warmup
  | _ -> None

(** Encode render request to JSON *)
let encode_render_request req =
  let base = [
    ("url", `String req.url);
    ("headers", `Assoc (List.map (fun (k, v) -> (k, `String v)) req.headers));
  ] in
  let with_payload = match req.payload with
    | Some p -> ("payload", p) :: base
    | None -> base
  in
  `Assoc with_payload

(** Encode JSON-RPC request *)
let encode_request ~id ~method_ ~params =
  let json = `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int id);
    ("method", `String (method_to_string method_));
    ("params", params);
  ] in
  Yojson.Safe.to_string json

(** Encode render request as JSON-RPC *)
let encode_render ~id req =
  encode_request ~id ~method_:Render ~params:(encode_render_request req)

(** Encode health request *)
let encode_health ~id =
  encode_request ~id ~method_:Health ~params:`Null

(** Encode warmup request *)
let encode_warmup ~id =
  encode_request ~id ~method_:Warmup ~params:`Null

(** {1 Decoding} *)

(** Decode JSON-RPC response *)
let decode_response json_str =
  try
    let json = Yojson.Safe.from_string json_str in
    let open Yojson.Safe.Util in
    match json |> member "error" with
    | `Null ->
      let id = json |> member "id" |> to_int in
      let result = json |> member "result" in
      Success { id; result }
    | error ->
      let id = json |> member "id" |> to_int_option in
      let code = error |> member "code" |> to_int in
      let message = error |> member "message" |> to_string in
      let data = match error |> member "data" with
        | `Null -> None
        | d -> Some d
      in
      Failure { id; error = { code; message; data } }
  with _ ->
    Failure {
      id = None;
      error = {
        code = parse_error;
        message = "Failed to parse JSON-RPC response";
        data = Some (`String json_str);
      }
    }

(** Decode render result from JSON *)
let decode_render_result json =
  let open Yojson.Safe.Util in
  {
    html = json |> member "html" |> to_string;
    head = json |> member "head" |> to_string_option |> Option.value ~default:"";
    payload_json = json |> member "payload" |> to_string_option;
    status = json |> member "status" |> to_int_option |> Option.value ~default:200;
    redirect = json |> member "redirect" |> to_string_option;
  }

(** Decode health result *)
let decode_health_result json =
  let open Yojson.Safe.Util in
  {
    ready = json |> member "ready" |> to_bool;
    requests = json |> member "requests" |> to_int;
    memory_mb = json |> member "memoryMb" |> to_int;
  }

(** {1 Stream Protocol} *)

(** Encode stream chunk *)
let encode_stream_chunk = function
  | Shell html -> Printf.sprintf "shell:%s" html
  | Content html -> Printf.sprintf "content:%s" html
  | Error msg -> Printf.sprintf "error:%s" msg
  | Done -> "done:"

(** Decode stream chunk *)
let decode_stream_chunk line =
  if String.length line < 2 then Done
  else
    let colon_idx = String.index_opt line ':' in
    match colon_idx with
    | None -> Done
    | Some idx ->
      let prefix = String.sub line 0 idx in
      let content = String.sub line (idx + 1) (String.length line - idx - 1) in
      match prefix with
      | "shell" -> Shell content
      | "content" -> Content content
      | "error" -> Error content
      | "done" -> Done
      | _ -> Done

(** {1 Batch Requests} *)

(** Encode batch request *)
let encode_batch requests =
  let json = `List (List.map (fun (id, method_, params) ->
    `Assoc [
      ("jsonrpc", `String "2.0");
      ("id", `Int id);
      ("method", `String (method_to_string method_));
      ("params", params);
    ]
  ) requests) in
  Yojson.Safe.to_string json

(** {1 Convenience} *)

(** Create render request *)
let render_request ~url ?(headers=[]) ?payload () =
  { url; headers; payload }

(** Check if response is success *)
let is_success = function
  | Success _ -> true
  | Failure _ -> false

(** Extract result from success response *)
let get_result = function
  | Success { result; _ } -> Some result
  | Failure _ -> None

(** Extract error from failure response *)
let get_error = function
  | Failure { error; _ } -> Some error
  | Success _ -> None
