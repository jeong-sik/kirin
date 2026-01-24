(** Angular SSR Protocol

    JSON-RPC 2.0 protocol for Angular SSR worker communication. *)

(** {1 Request Types} *)

(** Render request *)
type render_request = {
  url: string;
  headers: (string * string) list;
  document: string option;  (* Optional custom document *)
  providers: string list;   (* Additional providers *)
}

(** Health check request *)
type health_request = {
  include_memory: bool;
}

(** Prerender request *)
type prerender_request = {
  routes: string list;
  output_dir: string;
}

(** {1 Response Types} *)

(** Render result *)
type render_result = {
  html: string;
  transfer_state: Yojson.Safe.t option;
  styles: string list;
  scripts: string list;
  title: string option;
}

(** Health result *)
type health_result = {
  status: string;
  version: string;
  memory_mb: int option;
  uptime_s: float;
}

(** Prerender result *)
type prerender_result = {
  rendered_count: int;
  failed_routes: string list;
  output_files: string list;
}

(** Response result *)
type response_result =
  | RenderOk of render_result
  | HealthOk of health_result
  | PrerenderOk of prerender_result

(** {1 JSON-RPC 2.0} *)

(** JSON-RPC request *)
type request = {
  jsonrpc: string;
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

(** Angular-specific errors *)
let hydration_mismatch = -32001
let zone_error = -32002
let di_error = -32003
let render_timeout = -32004

(** {1 Request Building} *)

(** Create render request *)
let render_request ~url ?(headers=[]) ?document ?(providers=[]) () =
  { url; headers; document; providers }

(** Create health request *)
let health_request ?(include_memory=true) () =
  { include_memory }

(** Create prerender request *)
let prerender_request ~routes ~output_dir =
  { routes; output_dir }

(** {1 Encoding} *)

(** Encode render request to JSON *)
let encode_render_request req =
  `Assoc [
    ("url", `String req.url);
    ("headers", `Assoc (List.map (fun (k, v) -> (k, `String v)) req.headers));
    ("document", match req.document with Some d -> `String d | None -> `Null);
    ("providers", `List (List.map (fun p -> `String p) req.providers));
  ]

(** Encode JSON-RPC request *)
let encode_request ~id ~method_name params =
  let json = `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int id);
    ("method", `String method_name);
    ("params", params);
  ] in
  Yojson.Safe.to_string json

(** Encode render request with id *)
let encode_render ~id req =
  encode_request ~id ~method_name:"render" (encode_render_request req)

(** Encode health request with id *)
let encode_health ~id req =
  let params = `Assoc [
    ("includeMemory", `Bool req.include_memory);
  ] in
  encode_request ~id ~method_name:"health" params

(** Encode prerender request with id *)
let encode_prerender ~id req =
  let params = `Assoc [
    ("routes", `List (List.map (fun r -> `String r) req.routes));
    ("outputDir", `String req.output_dir);
  ] in
  encode_request ~id ~method_name:"prerender" params

(** {1 Decoding} *)

(** Decode render result from JSON *)
let decode_render_result json =
  let open Yojson.Safe.Util in
  {
    html = json |> member "html" |> to_string;
    transfer_state = (match json |> member "transferState" with
      | `Null -> None
      | ts -> Some ts);
    styles = json |> member "styles" |> to_list |> List.map to_string;
    scripts = json |> member "scripts" |> to_list |> List.map to_string;
    title = json |> member "title" |> to_string_option;
  }

(** Decode health result from JSON *)
let decode_health_result json =
  let open Yojson.Safe.Util in
  {
    status = json |> member "status" |> to_string;
    version = json |> member "version" |> to_string;
    memory_mb = json |> member "memoryMb" |> to_int_option;
    uptime_s = json |> member "uptimeS" |> to_float;
  }

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

(** {1 Error Helpers} *)

(** Create error response *)
let error_response ?id ~code ~message ?data () =
  Failure { id; error = { code; message; data } }

(** Check if response is success *)
let is_success = function
  | Success _ -> true
  | Failure _ -> false

(** Get result from success response *)
let get_result = function
  | Success { result; _ } -> Some result
  | Failure _ -> None

(** Get error from failure response *)
let get_error = function
  | Failure { error; _ } -> Some error
  | Success _ -> None
