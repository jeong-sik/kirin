(** Angular SSR Handler

    Kirin HTTP handler integration for Angular Universal. *)

(** {1 Handler Configuration} *)

(** Render mode for handler *)
type mode =
  | SSR              (* Full server-side rendering *)
  | CSR              (* Client-side only (serve index.html) *)
  | Hybrid           (* Route-based SSR/CSR *)
  | Prerender        (* Static pre-rendered pages *)

(** Handler configuration *)
type config = {
  mode: mode;
  dist_path: string;
  index_html: string;
  ssr_engine: Ssr.t option;
  server_routes: Route_def.t list;
  fallback_to_csr: bool;
  cache_control: string option;
}

(** Default configuration *)
let default_config = {
  mode = SSR;
  dist_path = "dist/browser";
  index_html = "index.html";
  ssr_engine = None;
  server_routes = [];
  fallback_to_csr = true;
  cache_control = Some "public, max-age=3600";
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

(** Response type *)
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

(** {1 Route Matching} *)

(** Check if route matches path *)
let route_matches route path =
  (* Simple path matching - in real impl would use proper router *)
  route.Route_def.path = path ||
  (route.Route_def.path = "**") ||
  (String.length route.Route_def.path > 0 &&
   route.Route_def.path.[String.length route.Route_def.path - 1] = '*' &&
   String.sub route.Route_def.path 0 (String.length route.Route_def.path - 1) =
   String.sub path 0 (min (String.length path) (String.length route.Route_def.path - 1)))

(** Find matching route *)
let find_route routes path =
  List.find_opt (fun r -> route_matches r path) routes

(** Get render mode for path *)
let get_render_mode config path =
  match find_route config.server_routes path with
  | Some route -> route.Route_def.render_mode
  | None -> Route_def.Server  (* Default to SSR *)

(** {1 Handler Logic} *)

(** Handle SSR request *)
let handle_ssr config request =
  match config.ssr_engine with
  | None ->
    if config.fallback_to_csr then
      (* Fallback to CSR *)
      let index_path = Filename.concat config.dist_path config.index_html in
      if Sys.file_exists index_path then
        let ic = open_in index_path in
        let content = really_input_string ic (in_channel_length ic) in
        close_in ic;
        html_response content
      else
        error_response ~status:500 ~message:"SSR engine not configured and index.html not found"
    else
      error_response ~status:500 ~message:"SSR engine not configured"
  | Some engine ->
    match Ssr.render engine ~url:request.path ~headers:request.headers () with
    | Ok html ->
      let headers = match config.cache_control with
        | Some cc -> [("Cache-Control", cc)]
        | None -> []
      in
      html_response ~headers html
    | Error msg ->
      if config.fallback_to_csr then
        let index_path = Filename.concat config.dist_path config.index_html in
        if Sys.file_exists index_path then
          let ic = open_in index_path in
          let content = really_input_string ic (in_channel_length ic) in
          close_in ic;
          html_response content
        else
          error_response ~status:500 ~message:msg
      else
        error_response ~status:500 ~message:msg

(** Handle CSR request *)
let handle_csr config =
  let index_path = Filename.concat config.dist_path config.index_html in
  if Sys.file_exists index_path then
    let ic = open_in index_path in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    html_response content
  else
    error_response ~status:404 ~message:"index.html not found"

(** Handle prerendered request *)
let handle_prerender config request =
  let path = if request.path = "/" then "/index" else request.path in
  let html_path = Filename.concat config.dist_path (path ^ ".html") in
  if Sys.file_exists html_path then
    let ic = open_in html_path in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    html_response content
  else
    (* Fallback to SSR or CSR *)
    handle_ssr config request

(** Main request handler *)
let handle ~config ~request () =
  match config.mode with
  | CSR -> handle_csr config
  | Prerender -> handle_prerender config request
  | SSR -> handle_ssr config request
  | Hybrid ->
    match get_render_mode config request.path with
    | Route_def.Client -> handle_csr config
    | Route_def.Server -> handle_ssr config request
    | Route_def.Prerender -> handle_prerender config request

(** {1 Static File Handling} *)

(** Check if path is static asset *)
let is_static_asset path =
  let ext = Filename.extension path in
  List.mem ext [".js"; ".css"; ".png"; ".jpg"; ".jpeg"; ".gif"; ".svg"; ".ico"; ".woff"; ".woff2"; ".ttf"; ".eot"; ".map"]

(** Serve static file *)
let serve_static config path =
  let file_path = Filename.concat config.dist_path path in
  if Sys.file_exists file_path && not (Sys.is_directory file_path) then
    let ic = open_in_bin file_path in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    let content_type = match Filename.extension path with
      | ".js" -> "application/javascript"
      | ".css" -> "text/css"
      | ".png" -> "image/png"
      | ".jpg" | ".jpeg" -> "image/jpeg"
      | ".gif" -> "image/gif"
      | ".svg" -> "image/svg+xml"
      | ".ico" -> "image/x-icon"
      | ".woff" -> "font/woff"
      | ".woff2" -> "font/woff2"
      | ".ttf" -> "font/ttf"
      | ".map" -> "application/json"
      | _ -> "application/octet-stream"
    in
    Some { status = 200; headers = [("Content-Type", content_type)]; body = content }
  else
    None

(** Catch-all handler with static file support *)
let catch_all_handler config request =
  if is_static_asset request.path then
    match serve_static config request.path with
    | Some response -> response
    | None -> error_response ~status:404 ~message:"File not found"
  else
    handle ~config ~request ()

(** {1 Serialization} *)

(** Mode to string *)
let mode_to_string = function
  | SSR -> "ssr"
  | CSR -> "csr"
  | Hybrid -> "hybrid"
  | Prerender -> "prerender"

(** Config to JSON *)
let config_to_json config =
  `Assoc [
    ("mode", `String (mode_to_string config.mode));
    ("distPath", `String config.dist_path);
    ("indexHtml", `String config.index_html);
    ("fallbackToCsr", `Bool config.fallback_to_csr);
  ]
