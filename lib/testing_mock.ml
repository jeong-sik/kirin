(** Mock Server

    Extracted from Testing.Mock. *)

(** Mock endpoint *)
type endpoint = {
  meth : Http.Method.t option;  (* None = any method *)
  path : string;
  response : Testing_response.t;
  mutable call_count : int;
}

(** Mock server *)
type t = {
  mutable endpoints : endpoint list;
  mutable unmatched_requests : Testing_request.t list;
}

(** Create mock server *)
let create () = { endpoints = []; unmatched_requests = [] }

(** Add mock endpoint *)
let on ?(meth = None) ~path ~status ?(headers = []) ?(body = "") t =
  let endpoint = {
    meth;
    path;
    response = { Testing_response.status; headers; body };
    call_count = 0;
  } in
  t.endpoints <- endpoint :: t.endpoints;
  t

(** Add mock endpoint with JSON response *)
let on_json ?(meth = None) ~path ~status ?(headers = []) json t =
  let body = Yojson.Safe.to_string json in
  let headers = ("Content-Type", "application/json") :: headers in
  on ~meth ~path ~status ~headers ~body t

(** Handle request *)
let handle t req =
  let matching = List.find_opt (fun ep ->
    ep.path = req.Testing_request.path &&
    (Option.is_none ep.meth || ep.meth = Some req.meth)
  ) t.endpoints in
  match matching with
  | Some ep ->
    ep.call_count <- ep.call_count + 1;
    ep.response
  | None ->
    t.unmatched_requests <- req :: t.unmatched_requests;
    { Testing_response.status = `Not_found; headers = []; body = "Not Found" }

(** Get call count for endpoint *)
let call_count ~path t =
  match List.find_opt (fun ep -> ep.path = path) t.endpoints with
  | Some ep -> ep.call_count
  | None -> 0

(** Get unmatched requests *)
let unmatched t = t.unmatched_requests

(** Reset mock server *)
let reset t =
  List.iter (fun ep -> ep.call_count <- 0) t.endpoints;
  t.unmatched_requests <- []

(** Assert endpoint was called *)
let assert_called ~path t =
  if call_count ~path t = 0 then
    Testing_assert.fail (Printf.sprintf "Expected endpoint '%s' to be called" path)

(** Assert endpoint was called n times *)
let assert_called_times ~path ~times t =
  let actual = call_count ~path t in
  if actual <> times then
    Testing_assert.fail (Printf.sprintf "Expected endpoint '%s' to be called %d times but was called %d times"
      path times actual)

(** Assert no unmatched requests *)
let assert_no_unmatched t =
  if t.unmatched_requests <> [] then
    Testing_assert.fail (Printf.sprintf "Found %d unmatched requests"
      (List.length t.unmatched_requests))
