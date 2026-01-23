(** Solid.js SSR Protocol

    JSON-RPC protocol for Solid.js SSR worker communication. *)

(** {1 Request Types} *)

(** SSR render request *)
type render_request = {
  url: string;
  props: Yojson.Safe.t;
  meta: Meta.builder option;
}

(** {1 Response Types} *)

(** SSR render response *)
type render_response = {
  html: string;
  head: string;
  status: int;
  redirect: string option;
  resources: (string * Yojson.Safe.t) list;
}

(** {1 JSON-RPC Protocol} *)

(** Request ID counter *)
let request_id = ref 0

(** Generate next request ID *)
let next_id () =
  incr request_id;
  !request_id

(** Encode render request to JSON-RPC *)
let encode_render_request req =
  let id = next_id () in
  let params = `Assoc [
    ("url", `String req.url);
    ("props", req.props);
  ] in
  let json = `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int id);
    ("method", `String "render");
    ("params", params);
  ] in
  (id, Yojson.Safe.to_string json ^ "\n")

(** Encode stream request *)
let encode_stream_request ~url ~props =
  let id = next_id () in
  let params = `Assoc [
    ("url", `String url);
    ("props", props);
  ] in
  let json = `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int id);
    ("method", `String "renderStream");
    ("params", params);
  ] in
  (id, Yojson.Safe.to_string json ^ "\n")

(** Encode health check request *)
let encode_health_request () =
  let id = next_id () in
  let json = `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int id);
    ("method", `String "health");
    ("params", `Assoc []);
  ] in
  (id, Yojson.Safe.to_string json ^ "\n")

(** {1 Response Decoding} *)

(** Parse render response from JSON *)
let decode_render_response json_str =
  try
    let json = Yojson.Safe.from_string json_str in
    match json with
    | `Assoc fields ->
      (match List.assoc_opt "error" fields with
       | Some (`Assoc err_fields) ->
         let msg = match List.assoc_opt "message" err_fields with
           | Some (`String m) -> m
           | _ -> "Unknown error"
         in
         Error msg
       | _ ->
         match List.assoc_opt "result" fields with
         | Some (`Assoc result_fields) ->
           let html = match List.assoc_opt "html" result_fields with
             | Some (`String h) -> h
             | _ -> ""
           in
           let head = match List.assoc_opt "head" result_fields with
             | Some (`String h) -> h
             | _ -> ""
           in
           let status = match List.assoc_opt "status" result_fields with
             | Some (`Int s) -> s
             | _ -> 200
           in
           let redirect = match List.assoc_opt "redirect" result_fields with
             | Some (`String r) -> Some r
             | _ -> None
           in
           let resources = match List.assoc_opt "resources" result_fields with
             | Some (`Assoc res) ->
               List.map (fun (k, v) -> (k, v)) res
             | _ -> []
           in
           Ok { html; head; status; redirect; resources }
         | _ -> Error "Missing result")
    | _ -> Error "Invalid JSON response"
  with
  | Yojson.Json_error msg -> Error (Printf.sprintf "JSON parse error: %s" msg)
  | _ -> Error "Unknown parse error"

(** {1 Stream Chunk Types} *)

(** Stream chunk *)
type stream_chunk =
  | Html of string
  | Script of string
  | Complete
  | Error of string

(** Parse stream chunk *)
let decode_stream_chunk json_str =
  try
    let json = Yojson.Safe.from_string json_str in
    match json with
    | `Assoc fields ->
      (match List.assoc_opt "type" fields with
       | Some (`String "html") ->
         (match List.assoc_opt "content" fields with
          | Some (`String c) -> Html c
          | _ -> Error "Missing html content")
       | Some (`String "script") ->
         (match List.assoc_opt "content" fields with
          | Some (`String c) -> Script c
          | _ -> Error "Missing script content")
       | Some (`String "complete") -> Complete
       | Some (`String "error") ->
         (match List.assoc_opt "message" fields with
          | Some (`String m) -> Error m
          | _ -> Error "Unknown stream error")
       | _ -> Error "Unknown chunk type")
    | _ -> Error "Invalid chunk JSON"
  with _ -> Error "Chunk parse error"

(** {1 Health Check Response} *)

(** Worker health status *)
type health_status = {
  ok: bool;
  memory_mb: int;
  uptime_s: int;
  renders: int;
}

(** Decode health response *)
let decode_health_response json_str =
  try
    let json = Yojson.Safe.from_string json_str in
    match json with
    | `Assoc fields ->
      (match List.assoc_opt "result" fields with
       | Some (`Assoc result) ->
         let ok = match List.assoc_opt "ok" result with
           | Some (`Bool b) -> b
           | _ -> false
         in
         let memory_mb = match List.assoc_opt "memoryMB" result with
           | Some (`Int m) -> m
           | _ -> 0
         in
         let uptime_s = match List.assoc_opt "uptimeSeconds" result with
           | Some (`Int u) -> u
           | _ -> 0
         in
         let renders = match List.assoc_opt "renders" result with
           | Some (`Int r) -> r
           | _ -> 0
         in
         Ok { ok; memory_mb; uptime_s; renders }
       | _ -> Error "Missing result")
    | _ -> Error "Invalid JSON"
  with _ -> Error "Parse error"
