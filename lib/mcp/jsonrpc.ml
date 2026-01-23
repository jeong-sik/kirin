(** Kirin MCP - JSON-RPC 2.0 Implementation

    Standard JSON-RPC 2.0 protocol for MCP communication.
    Handles request/response/notification message encoding and decoding.
*)

(** {1 Types} *)

(** JSON-RPC message ID *)
type id =
  | Int of int
  | String of string
  | Null

(** JSON-RPC error codes *)
type error_code =
  | Parse_error        (** -32700 *)
  | Invalid_request    (** -32600 *)
  | Method_not_found   (** -32601 *)
  | Invalid_params     (** -32602 *)
  | Internal_error     (** -32603 *)
  | Custom of int

(** JSON-RPC error object *)
type error = {
  code : error_code;
  message : string;
  data : Yojson.Safe.t option;
}

(** JSON-RPC request *)
type request = {
  id : id;
  method_ : string;
  params : Yojson.Safe.t option;
}

(** JSON-RPC response *)
type response = {
  id : id;
  result : Yojson.Safe.t option;
  error : error option;
}

(** JSON-RPC notification (no id, no response expected) *)
type notification = {
  method_ : string;
  params : Yojson.Safe.t option;
}

(** Any JSON-RPC message *)
type message =
  | Request of request
  | Response of response
  | Notification of notification

(** {1 Error Code Conversion} *)

let error_code_to_int = function
  | Parse_error -> -32700
  | Invalid_request -> -32600
  | Method_not_found -> -32601
  | Invalid_params -> -32602
  | Internal_error -> -32603
  | Custom code -> code

let error_code_of_int = function
  | -32700 -> Parse_error
  | -32600 -> Invalid_request
  | -32601 -> Method_not_found
  | -32602 -> Invalid_params
  | -32603 -> Internal_error
  | code -> Custom code

(** {1 Encoding} *)

let id_to_json = function
  | Int i -> `Int i
  | String s -> `String s
  | Null -> `Null

let error_to_json err =
  let base = [
    "code", `Int (error_code_to_int err.code);
    "message", `String err.message;
  ] in
  let with_data = match err.data with
    | Some d -> base @ ["data", d]
    | None -> base
  in
  `Assoc with_data

let encode_request (req : request) =
  let base = [
    "jsonrpc", `String "2.0";
    "id", id_to_json req.id;
    "method", `String req.method_;
  ] in
  let with_params = match req.params with
    | Some p -> base @ ["params", p]
    | None -> base
  in
  `Assoc with_params

let encode_response (resp : response) =
  let base = [
    "jsonrpc", `String "2.0";
    "id", id_to_json resp.id;
  ] in
  let with_result = match resp.result, resp.error with
    | Some r, None -> base @ ["result", r]
    | None, Some e -> base @ ["error", error_to_json e]
    | Some r, Some _ -> base @ ["result", r]  (* Result takes precedence *)
    | None, None -> base @ ["result", `Null]
  in
  `Assoc with_result

let encode_notification (notif : notification) =
  let base = [
    "jsonrpc", `String "2.0";
    "method", `String notif.method_;
  ] in
  let with_params = match notif.params with
    | Some p -> base @ ["params", p]
    | None -> base
  in
  `Assoc with_params

let encode = function
  | Request req -> encode_request req
  | Response resp -> encode_response resp
  | Notification notif -> encode_notification notif

(** {1 Decoding} *)

let id_of_json = function
  | `Int i -> Int i
  | `String s -> String s
  | `Null -> Null
  | _ -> Null

let error_of_json json =
  let open Yojson.Safe.Util in
  let code = json |> member "code" |> to_int |> error_code_of_int in
  let message = json |> member "message" |> to_string in
  let data = match json |> member "data" with
    | `Null -> None
    | d -> Some d
  in
  { code; message; data }

let decode_request json : request =
  let open Yojson.Safe.Util in
  let id = json |> member "id" |> id_of_json in
  let method_ = json |> member "method" |> to_string in
  let params = match json |> member "params" with
    | `Null -> None
    | p -> Some p
  in
  { id; method_; params }

let decode_response json : response =
  let open Yojson.Safe.Util in
  let id = json |> member "id" |> id_of_json in
  let result = match json |> member "result" with
    | `Null -> None
    | r -> Some r
  in
  let error = match json |> member "error" with
    | `Null -> None
    | e -> Some (error_of_json e)
  in
  { id; result; error }

let decode_notification json : notification =
  let open Yojson.Safe.Util in
  let method_ = json |> member "method" |> to_string in
  let params = match json |> member "params" with
    | `Null -> None
    | p -> Some p
  in
  { method_; params }

let decode json =
  let open Yojson.Safe.Util in
  let has_id = match json |> member "id" with
    | `Null -> false
    | _ -> true
  in
  let has_method = match json |> member "method" with
    | `Null -> false
    | _ -> true
  in
  match has_id, has_method with
  | true, true -> Request (decode_request json)
  | true, false -> Response (decode_response json)
  | false, true -> Notification (decode_notification json)
  | false, false -> Response (decode_response json)

(** {1 Helpers} *)

let make_request ~id ~method_ ?params () =
  { id; method_; params }

let make_response ~id ?result ?error () =
  { id; result; error }

let make_notification ~method_ ?params () =
  { method_; params }

let make_error ~code ~message ?data () =
  { code; message; data }

let success_response ~id result =
  { id; result = Some result; error = None }

let error_response ~id err =
  { id; result = None; error = Some err }
