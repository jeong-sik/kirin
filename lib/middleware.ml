(** Middleware module - Handler composition *)

(** Middleware type: transforms a handler into another handler *)
type t = Router.handler -> Router.handler

(** Identity middleware - does nothing *)
let id : t = fun handler -> handler

(** Compose two middlewares *)
let compose (m1 : t) (m2 : t) : t =
  fun handler -> m1 (m2 handler)

(** Compose a list of middlewares (left to right application) *)
let pipeline middlewares =
  List.fold_right compose middlewares id

(** Apply middleware to a handler - same as function application *)
let apply (mw : t) (handler : Router.handler) = mw handler

(** Infix operator for middleware composition (same as @@) *)
let ( >> ) = compose

(** Logger middleware - logs request and response *)
let logger : t = fun handler req ->
  let start_time = Time_compat.now () in
  let meth = Http.Method.to_string (Request.meth req) in
  let path = Request.path req in
  Printf.eprintf "[%s] %s %s\n%!"
    (let tm = Unix.localtime start_time in
     Printf.sprintf "%02d:%02d:%02d" tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec)
    meth path;

  let response = handler req in

  let elapsed = Time_compat.now () -. start_time in
  let status = Http.Status.to_int (Response.status response) in
  Printf.eprintf "[%s] %s %s -> %d (%.3fms)\n%!"
    (let tm = Unix.localtime (Time_compat.now ()) in
     Printf.sprintf "%02d:%02d:%02d" tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec)
    meth path status (elapsed *. 1000.0);

  response

(** Error catcher middleware *)
let catch (error_handler : exn -> Request.t -> Response.t) : t = fun handler req ->
  try handler req
  with exn -> error_handler exn req

(** Default error handler *)
let default_error_handler exn _req =
  Printf.eprintf "Error: %s\n%!" (Printexc.to_string exn);
  Response.server_error ()

(** Catch with default error handler *)
let catch_default : t = catch default_error_handler

(** CORS middleware *)
type cors_config = {
  origins : string list;  (* Allowed origins, or ["*"] for any *)
  methods : string list;
  headers : string list;
  credentials : bool;
  max_age : int option;
}

let default_cors_config = {
  origins = ["*"];
  methods = ["GET"; "POST"; "PUT"; "PATCH"; "DELETE"; "OPTIONS"];
  headers = ["Content-Type"; "Authorization"; "X-Requested-With"];
  credentials = false;
  max_age = Some 86400;
}

let cors ?(config = default_cors_config) () : t = fun handler req ->
  let origin = Request.header "origin" req in
  let is_preflight = Request.meth req = `OPTIONS in

  let add_cors_headers resp =
    let origin_header =
      match origin, config.origins with
      | Some o, ["*"] -> o
      | Some o, origins when List.mem o origins -> o
      | _, ["*"] -> "*"
      | _ -> ""
    in
    resp
    |> Response.with_header "Access-Control-Allow-Origin" origin_header
    |> Response.with_header "Access-Control-Allow-Methods" (String.concat ", " config.methods)
    |> Response.with_header "Access-Control-Allow-Headers" (String.concat ", " config.headers)
    |> (fun r -> if config.credentials
                 then Response.with_header "Access-Control-Allow-Credentials" "true" r
                 else r)
    |> (fun r -> match config.max_age with
                 | Some age -> Response.with_header "Access-Control-Max-Age" (string_of_int age) r
                 | None -> r)
  in

  if is_preflight then
    Response.empty `No_content |> add_cors_headers
  else
    handler req |> add_cors_headers

(** Set custom headers middleware *)
let with_headers hs : t = fun handler req ->
  let resp = handler req in
  Response.with_headers hs resp

(** Timing header middleware *)
let timing : t = fun handler req ->
  let start = Time_compat.now () in
  let resp = handler req in
  let elapsed = Time_compat.now () -. start in
  Response.with_header "X-Response-Time" (Printf.sprintf "%.3fms" (elapsed *. 1000.0)) resp
