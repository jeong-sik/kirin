(** tRPC Batch Request Handling

    Process multiple procedure calls in a single HTTP request. *)

(** {1 Request Types} *)

(** Single call in a batch *)
type call = {
  id: int;
  path: string;
  input: Yojson.Safe.t;
}

(** Parse a single call from JSON *)
let parse_call json =
  match json with
  | `Assoc fields ->
    let id = match List.assoc_opt "id" fields with
      | Some (`Int i) -> i
      | _ -> 0
    in
    let path = match List.assoc_opt "path" fields with
      | Some (`String s) -> s
      | _ -> ""
    in
    let input = match List.assoc_opt "input" fields with
      | Some i -> i
      | None -> `Null
    in
    Some { id; path; input }
  | _ -> None

(** {1 Response Types} *)

(** Single response in a batch *)
type response = {
  id: int;
  result: (Yojson.Safe.t, error) result;
}

and error = {
  code: int;
  message: string;
  data: Yojson.Safe.t option;
}

(** Error codes *)
module ErrorCode = struct
  let parse_error = -32700
  let invalid_request = -32600
  let method_not_found = -32601
  let invalid_params = -32602
  let internal_error = -32603
end

(** Create success response *)
let success ~id result = {
  id;
  result = Ok result;
}

(** Create error response *)
let error ~id ?(code = ErrorCode.internal_error) ?(data : Yojson.Safe.t option) message = {
  id;
  result = Error { code; message; data };
}

(** Serialize response to JSON *)
let response_to_json resp =
  let base = [("id", `Int resp.id)] in
  match resp.result with
  | Ok result ->
    `Assoc (base @ [("result", result)])
  | Error err ->
    let error_obj = [
      ("code", `Int err.code);
      ("message", `String err.message);
    ] in
    let error_obj = match err.data with
      | Some d -> error_obj @ [("data", d)]
      | None -> error_obj
    in
    `Assoc (base @ [("error", `Assoc error_obj)])

(** {1 Batch Processing} *)

(** Parse batch request *)
let parse_batch json =
  match json with
  | `List items -> List.filter_map parse_call items
  | single -> Option.to_list (parse_call single)

(** Check if request is a batch *)
let is_batch json =
  match json with
  | `List _ -> true
  | _ -> false

(** Process a batch of calls *)
let process_batch ~execute ctx calls =
  List.map (fun call ->
    match execute call.path ctx call.input with
    | Ok result -> success ~id:call.id result
    | Error msg -> error ~id:call.id msg
  ) calls

(** Serialize batch response *)
let batch_response_to_json responses =
  match responses with
  | [single] -> response_to_json single
  | multiple -> `List (List.map response_to_json multiple)

(** {1 Request/Response Helpers} *)

(** Create a batch request JSON *)
let create_batch_request calls =
  `List (List.map (fun (id, path, input) ->
    `Assoc [
      ("id", `Int id);
      ("path", `String path);
      ("input", input);
    ]
  ) calls)

(** Create a single request JSON *)
let create_request ~id ~path ~input =
  `Assoc [
    ("id", `Int id);
    ("path", `String path);
    ("input", input);
  ]
