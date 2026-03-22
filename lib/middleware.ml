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
  let start_time = Unix.gettimeofday () in
  let meth = Http.Method.to_string (Request.meth req) in
  let path = Request.path req in
  Logger.info "%s %s" meth path;
  let response = handler req in
  let elapsed = Unix.gettimeofday () -. start_time in
  let status = Http.Status.to_int (Response.status response) in
  Logger.info "%s %s -> %d (%.3fms)" meth path status (elapsed *. 1000.0);

  response

(** Error catcher middleware *)
let catch (error_handler : exn -> Request.t -> Response.t) : t = fun handler req ->
  try handler req
  with
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | exn -> error_handler exn req

(** Default error handler *)
let default_error_handler exn _req =
  Logger.error "Unhandled: %s" (Printexc.to_string exn);
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
    (* Determine the Allow-Origin value.
       - Never reflect an empty or missing Origin.
       - credentials:true + wildcard origins is invalid per the CORS spec;
         we only reflect the concrete origin when it matches the allow-list.
       - When origin is not allowed, return None so no CORS headers are added. *)
    let origin_value =
      match origin with
      | None | Some "" ->
        (* No Origin header: only send "*" when credentials are off and
           origins are wildcard. Otherwise, deny. *)
        if config.credentials then None
        else (match config.origins with ["*"] -> Some "*" | _ -> None)
      | Some o ->
        (match config.origins with
         | ["*"] -> Some o  (* Always reflect concrete origin, never "*" with a real Origin *)
         | origins -> if List.mem o origins then Some o else None)
    in
    match origin_value with
    | None ->
      (* Origin not allowed: do not add any CORS headers.
         Add common non-CORS headers (methods, max-age) only on preflight. *)
      resp
    | Some ov ->
      let resp = resp
        |> Response.with_header "Access-Control-Allow-Origin" ov
        |> Response.with_header "Access-Control-Allow-Methods" (String.concat ", " config.methods)
        |> Response.with_header "Access-Control-Allow-Headers" (String.concat ", " config.headers)
      in
      (* When reflecting a specific origin (not "*"), add Vary: Origin
         to prevent cache poisoning. *)
      let resp =
        if ov <> "*" then Response.with_header "Vary" "Origin" resp else resp
      in
      (* credentials:true must never be combined with Allow-Origin: "*" *)
      let resp =
        if config.credentials && ov <> "*"
        then Response.with_header "Access-Control-Allow-Credentials" "true" resp
        else resp
      in
      (match config.max_age with
       | Some age -> Response.with_header "Access-Control-Max-Age" (string_of_int age) resp
       | None -> resp)
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
  let start = Unix.gettimeofday () in
  let resp = handler req in
  let elapsed = Unix.gettimeofday () -. start in
  Response.with_header "X-Response-Time" (Printf.sprintf "%.3fms" (elapsed *. 1000.0)) resp
