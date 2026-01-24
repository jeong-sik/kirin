(** Qwik HTTP Handler

    Kirin HTTP handler integration for QwikCity. *)

(** {1 Handler Configuration} *)

(** Render mode *)
type mode =
  | SSR             (* Full server-side rendering *)
  | Static          (* Serve pre-rendered static files *)
  | SPA             (* Client-side only *)

(** Handler configuration *)
type config = {
  mode: mode;
  dist_path: string;
  manifest_path: string;
  ssr_engine: Ssr.t option;
  routes: Route_def.t list;
  fallback_to_spa: bool;
}

(** Default configuration *)
let default_config = {
  mode = SSR;
  dist_path = "dist";
  manifest_path = "q-manifest.json";
  ssr_engine = None;
  routes = [];
  fallback_to_spa = true;
}

(** {1 Request Info} *)

(** Request information *)
type request_info = {
  path: string;
  query: (string * string) list;
  headers: (string * string) list;
  method_: string;
  body: string option;
}

(** Create request info *)
let request_info ~path ?(query=[]) ?(headers=[]) ?(method_="GET") ?body () = {
  path;
  query;
  headers;
  method_;
  body;
}

(** {1 Response Types} *)

(** Response *)
type response = {
  status: int;
  headers: (string * string) list;
  body: string;
}

(** Create HTML response *)
let html_response ?(status=200) ?(headers=[]) body =
  let default_headers = [
    ("Content-Type", "text/html; charset=utf-8");
  ] in
  { status; headers = default_headers @ headers; body }

(** Create redirect response *)
let redirect_response ?(status=302) location =
  { status; headers = [("Location", location)]; body = "" }

(** Create JSON response *)
let json_response ?(status=200) json =
  let body = Yojson.Safe.to_string json in
  { status; headers = [("Content-Type", "application/json")]; body }

(** Create error response *)
let error_response ~status ~message =
  let body = Printf.sprintf {|<!DOCTYPE html>
<html>
<head><title>Error %d</title></head>
<body>
<h1>Error %d</h1>
<p>%s</p>
</body>
</html>|} status status message in
  { status; headers = [("Content-Type", "text/html; charset=utf-8")]; body }

(** {1 Static File Handling} *)

(** Check if path is static asset *)
let is_static_asset path =
  let ext = Filename.extension path in
  List.mem ext [".js"; ".mjs"; ".css"; ".png"; ".jpg"; ".jpeg"; ".gif"; ".svg";
                ".ico"; ".woff"; ".woff2"; ".ttf"; ".eot"; ".map"; ".json"]

(** Serve static file *)
let serve_static config path =
  let file_path = Filename.concat config.dist_path path in
  if Sys.file_exists file_path && not (Sys.is_directory file_path) then
    let ic = open_in_bin file_path in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    let content_type = match Filename.extension path with
      | ".js" | ".mjs" -> "application/javascript"
      | ".css" -> "text/css"
      | ".png" -> "image/png"
      | ".jpg" | ".jpeg" -> "image/jpeg"
      | ".gif" -> "image/gif"
      | ".svg" -> "image/svg+xml"
      | ".ico" -> "image/x-icon"
      | ".woff" -> "font/woff"
      | ".woff2" -> "font/woff2"
      | ".ttf" -> "font/ttf"
      | ".map" | ".json" -> "application/json"
      | _ -> "application/octet-stream"
    in
    Some { status = 200; headers = [("Content-Type", content_type)]; body = content }
  else
    None

(** {1 Handler Logic} *)

(** Handle SSR request *)
let handle_ssr config request =
  match config.ssr_engine with
  | None ->
    if config.fallback_to_spa then
      (* Serve index.html for SPA fallback *)
      let index_path = Filename.concat config.dist_path "index.html" in
      if Sys.file_exists index_path then
        let ic = open_in index_path in
        let content = really_input_string ic (in_channel_length ic) in
        close_in ic;
        html_response content
      else
        error_response ~status:500 ~message:"SSR engine not configured"
    else
      error_response ~status:500 ~message:"SSR engine not configured"
  | Some engine ->
    match Ssr.render engine ~url:request.path ~headers:request.headers () with
    | Ok html -> html_response html
    | Error msg ->
      if config.fallback_to_spa then
        let index_path = Filename.concat config.dist_path "index.html" in
        if Sys.file_exists index_path then
          let ic = open_in index_path in
          let content = really_input_string ic (in_channel_length ic) in
          close_in ic;
          html_response content
        else
          error_response ~status:500 ~message:msg
      else
        error_response ~status:500 ~message:msg

(** Handle static request *)
let handle_static config request =
  let path = if request.path = "/" then "/index.html" else request.path ^ ".html" in
  let html_path = Filename.concat config.dist_path path in
  if Sys.file_exists html_path then
    let ic = open_in html_path in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    html_response content
  else
    handle_ssr config request  (* Fallback to SSR *)

(** Handle SPA request *)
let handle_spa config =
  let index_path = Filename.concat config.dist_path "index.html" in
  if Sys.file_exists index_path then
    let ic = open_in index_path in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    html_response content
  else
    error_response ~status:404 ~message:"index.html not found"

(** Main request handler *)
let handle ~config ~request () =
  (* First check for static assets *)
  if is_static_asset request.path then
    match serve_static config request.path with
    | Some response -> response
    | None -> error_response ~status:404 ~message:"File not found"
  else
    (* Handle based on mode *)
    match config.mode with
    | SPA -> handle_spa config
    | Static -> handle_static config request
    | SSR -> handle_ssr config request

(** Catch-all handler *)
let catch_all_handler config request =
  handle ~config ~request ()

(** {1 Loader/Action Handling} *)

(** Handle loader request *)
let handle_loader ~config ~request ~loader_name () =
  (* In real impl, would look up loader and execute *)
  let _ = config in
  let _ = request in
  json_response (`Assoc [
    ("loader", `String loader_name);
    ("data", `Assoc []);
  ])

(** Handle action request *)
let handle_action ~config ~request ~action_name () =
  (* In real impl, would look up action and execute *)
  let _ = config in
  let _ = request in
  json_response (`Assoc [
    ("action", `String action_name);
    ("result", `Assoc []);
  ])

(** {1 Q-Data Endpoint} *)

(** Handle q-data.json request *)
let handle_q_data ~config ~request () =
  let _ = config in
  json_response (`Assoc [
    ("url", `String request.path);
    ("loaders", `Assoc []);
    ("actions", `Assoc []);
  ])

(** {1 Serialization} *)

(** Mode to string *)
let mode_to_string = function
  | SSR -> "ssr"
  | Static -> "static"
  | SPA -> "spa"

(** Config to JSON *)
let config_to_json config =
  `Assoc [
    ("mode", `String (mode_to_string config.mode));
    ("distPath", `String config.dist_path);
    ("manifestPath", `String config.manifest_path);
    ("fallbackToSpa", `Bool config.fallback_to_spa);
  ]
