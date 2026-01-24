(** Astro SSR Protocol

    JSON-RPC 2.0 for communication with Astro SSR workers. *)

(** {1 Protocol Types} *)

(** Request ID *)
type request_id = int

(** JSON-RPC request *)
type request = {
  id: request_id;
  method_: string;
  params: Yojson.Safe.t;
}

(** JSON-RPC response *)
type response =
  | Success of { id: request_id; result: Yojson.Safe.t }
  | Error of { id: request_id; code: int; message: string; data: Yojson.Safe.t option }

(** {1 Standard Error Codes} *)

let parse_error = -32700
let invalid_request = -32600
let method_not_found = -32601
let invalid_params = -32602
let internal_error = -32603
let render_timeout = -32000
let component_error = -32001
let hydration_error = -32002

(** {1 Request Construction} *)

let next_id = ref 0

let gen_id () =
  incr next_id;
  !next_id

(** Create render request *)
let render_request ~url ?(props=`Assoc []) ?(islands=[]) () = {
  id = gen_id ();
  method_ = "render";
  params = `Assoc [
    ("url", `String url);
    ("props", props);
    ("islands", `List (List.map Island.to_json islands));
  ];
}

(** Create render island request *)
let render_island_request ~island_id ~component ?(props=`Assoc []) () = {
  id = gen_id ();
  method_ = "renderIsland";
  params = `Assoc [
    ("islandId", `String island_id);
    ("component", `String component);
    ("props", props);
  ];
}

(** Create content request *)
let content_request ~collection ~slug () = {
  id = gen_id ();
  method_ = "getContent";
  params = `Assoc [
    ("collection", `String collection);
    ("slug", `String slug);
  ];
}

(** Create health check request *)
let health_request () = {
  id = gen_id ();
  method_ = "health";
  params = `Assoc [];
}

(** {1 Request Encoding} *)

(** Encode request to JSON string *)
let encode_request req =
  let json = `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int req.id);
    ("method", `String req.method_);
    ("params", req.params);
  ] in
  Yojson.Safe.to_string json ^ "\n"

(** {1 Response Decoding} *)

(** Decode response from JSON string *)
let decode_response s =
  try
    let json = Yojson.Safe.from_string s in
    let open Yojson.Safe.Util in
    let id = json |> member "id" |> to_int in
    match json |> member "error" with
    | `Null ->
      let result = json |> member "result" in
      Success { id; result }
    | error ->
      let code = error |> member "code" |> to_int in
      let message = error |> member "message" |> to_string in
      let data = match error |> member "data" with
        | `Null -> None
        | d -> Some d
      in
      Error { id; code; message; data }
  with e ->
    Error { id = 0; code = parse_error; message = Printexc.to_string e; data = None }

(** {1 Response Construction} *)

(** Create success response *)
let success_response ~id result =
  Success { id; result }

(** Create error response *)
let error_response ~id ~code ~message ?data () =
  Error { id; code; message; data }

(** {1 Response Encoding} *)

(** Encode response to JSON string *)
let encode_response = function
  | Success { id; result } ->
    let json = `Assoc [
      ("jsonrpc", `String "2.0");
      ("id", `Int id);
      ("result", result);
    ] in
    Yojson.Safe.to_string json ^ "\n"
  | Error { id; code; message; data } ->
    let error_obj = [
      ("code", `Int code);
      ("message", `String message);
    ] @ (match data with Some d -> [("data", d)] | None -> [])
    in
    let json = `Assoc [
      ("jsonrpc", `String "2.0");
      ("id", `Int id);
      ("error", `Assoc error_obj);
    ] in
    Yojson.Safe.to_string json ^ "\n"

(** {1 Render Result} *)

(** Render result from response *)
type render_result = {
  html: string;
  head: string;
  islands: (string * string) list;  (* id -> html *)
  scripts: string list;
}

(** Parse render result *)
let parse_render_result json =
  let open Yojson.Safe.Util in
  let html = json |> member "html" |> to_string in
  let head = json |> member "head" |> to_string_option |> Option.value ~default:"" in
  let islands = json |> member "islands" |> to_assoc
    |> List.map (fun (k, v) -> (k, to_string v)) in
  let scripts = json |> member "scripts" |> to_list
    |> List.map to_string in
  { html; head; islands; scripts }

(** {1 Batch Support} *)

(** Batch request *)
type batch_request = request list

(** Batch response *)
type batch_response = response list

(** Encode batch request *)
let encode_batch_request requests =
  let json = `List (List.map (fun req ->
    `Assoc [
      ("jsonrpc", `String "2.0");
      ("id", `Int req.id);
      ("method", `String req.method_);
      ("params", req.params);
    ]
  ) requests) in
  Yojson.Safe.to_string json ^ "\n"

(** Decode batch response *)
let decode_batch_response s =
  try
    let json = Yojson.Safe.from_string s in
    let open Yojson.Safe.Util in
    json |> to_list |> List.map (fun item ->
      let id = item |> member "id" |> to_int in
      match item |> member "error" with
      | `Null ->
        let result = item |> member "result" in
        Success { id; result }
      | error ->
        let code = error |> member "code" |> to_int in
        let message = error |> member "message" |> to_string in
        let data = match error |> member "data" with
          | `Null -> None
          | d -> Some d
        in
        Error { id; code; message; data }
    )
  with _ -> []
