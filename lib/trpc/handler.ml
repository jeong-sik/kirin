(** tRPC Handler

    Kirin route handler integration for tRPC. *)

open Kirin

(** {1 Handler Types} *)

(** Handler configuration *)
type config = {
  prefix: string;
  enable_batching: bool;
  max_batch_size: int;
}

let default_config = {
  prefix = "/trpc";
  enable_batching = true;
  max_batch_size = 10;
}

(** {1 Request Parsing} *)

(** Parse procedure path from URL *)
let parse_path ~prefix url =
  let url_path = Uri.path (Uri.of_string url) in
  if String.length url_path > String.length prefix &&
     String.sub url_path 0 (String.length prefix) = prefix then
    let rest = String.sub url_path (String.length prefix)
        (String.length url_path - String.length prefix) in
    (* Remove leading slash *)
    if String.length rest > 0 && String.get rest 0 = '/' then
      Some (String.sub rest 1 (String.length rest - 1))
    else if rest = "" then
      None
    else
      Some rest
  else
    None

(** Parse input from query string (for GET queries) *)
let parse_query_input req =
  let uri = Uri.of_string (Request.path req) in
  match Uri.get_query_param uri "input" with
  | Some encoded ->
    (try
       let decoded = Uri.pct_decode encoded in
       Some (Yojson.Safe.from_string decoded)
     with _ -> None)
  | None -> Some `Null

(** Parse input from request body (for POST mutations) *)
let parse_body_input body =
  if body = "" then
    Ok `Null
  else
    try Ok (Yojson.Safe.from_string body)
    with Yojson.Json_error msg -> Error msg

(** {1 Response Building} *)

(** Build success response *)
let success_response result =
  let json = `Assoc [
    ("result", `Assoc [
      ("data", result)
    ])
  ] in
  Response.json json

(** Build error response *)
let error_response ?(code = -32603) message =
  let json = `Assoc [
    ("error", `Assoc [
      ("message", `String message);
      ("code", `Int code);
    ])
  ] in
  Response.json json
  |> Response.with_status `Internal_server_error

(** Build not found response *)
let not_found_response path =
  let json = `Assoc [
    ("error", `Assoc [
      ("message", `String (Printf.sprintf "No procedure found for path: %s" path));
      ("code", `Int Batch.ErrorCode.method_not_found);
    ])
  ] in
  Response.json json
  |> Response.with_status `Not_found

(** {1 Query Handler} *)

(** Handle a query (GET request) *)
let handle_query router ctx_factory path req =
  let ctx = ctx_factory req in
  match parse_query_input req with
  | None ->
    error_response ~code:Batch.ErrorCode.parse_error "Invalid input JSON"
  | Some input ->
    match Trpc_router.execute_procedure path ctx input router with
    | Ok result -> success_response result
    | Error msg -> error_response msg

(** {1 Mutation Handler} *)

(** Handle a mutation (POST request) *)
let handle_mutation router ctx_factory path body req =
  let ctx = ctx_factory req in
  match parse_body_input body with
  | Error msg ->
    error_response ~code:Batch.ErrorCode.parse_error msg
  | Ok input ->
    match Trpc_router.execute_procedure path ctx input router with
    | Ok result -> success_response result
    | Error msg -> error_response msg

(** {1 Batch Handler} *)

(** Handle batch request *)
let handle_batch ~config router ctx_factory body req =
  match parse_body_input body with
  | Error msg ->
    error_response ~code:Batch.ErrorCode.parse_error msg
  | Ok json ->
    let calls = Batch.parse_batch json in
    if List.length calls > config.max_batch_size then
      error_response ~code:Batch.ErrorCode.invalid_request
        (Printf.sprintf "Batch size exceeds maximum of %d" config.max_batch_size)
    else
      let ctx = ctx_factory req in
      let execute path _ctx input = Trpc_router.execute_procedure path ctx input router in
      let responses = Batch.process_batch ~execute ctx calls in
      let json = Batch.batch_response_to_json responses in
      Response.json json

(** {1 Main Handler} *)

(** Create tRPC route handler *)
let create_handler ?(config = default_config) ~ctx_factory router =
  fun req ->
    let url = Request.path req in
    let method_ = Request.meth req in

    match parse_path ~prefix:config.prefix url with
    | None ->
      not_found_response url
    | Some path ->
      match method_ with
      | `GET ->
        (* Query *)
        handle_query router ctx_factory path req
      | `POST ->
        (* Could be mutation or batch *)
        let body = Request.body req in
        if config.enable_batching && Batch.is_batch (
          try Yojson.Safe.from_string body with _ -> `Null
        ) then
          handle_batch ~config router ctx_factory body req
        else
          handle_mutation router ctx_factory path body req
      | _ ->
        error_response ~code:Batch.ErrorCode.invalid_request "Method not allowed"
        |> Response.with_status `Method_not_allowed

(** {1 Route Registration} *)

(** Create routes for tRPC *)
let routes ?(config = default_config) ~ctx_factory router =
  let handler = create_handler ~config ~ctx_factory router in
  let pattern = config.prefix ^ "/*" in
  [
    get pattern handler;
    post pattern handler;
  ]

(** Create a single catch-all route *)
let route ?(config = default_config) ~ctx_factory router =
  let handler = create_handler ~config ~ctx_factory router in
  get (config.prefix ^ "/*") handler
