(** Astro HTTP Handler

    Kirin HTTP handler integration for Astro. *)

(** {1 Handler Configuration} *)

(** Output mode *)
type output =
  | Static        (* Pre-render everything at build time *)
  | Server        (* On-demand SSR *)
  | Hybrid        (* Mix of static and server *)

(** Handler configuration *)
type config = {
  output: output;
  dist_path: string;
  pages_path: string;
  public_path: string;
  ssr_engine: Ssr.t option;
  routes: Route_def.t list;
  integrations: Integration.t list;
  view_transitions: View_transitions.config;
  dev_mode: bool;
  dev_port: int;
}

(** Default configuration *)
let default_config = {
  output = Hybrid;
  dist_path = "dist";
  pages_path = "src/pages";
  public_path = "public";
  ssr_engine = None;
  routes = [];
  integrations = [];
  view_transitions = View_transitions.default_config;
  dev_mode = false;
  dev_port = 4321;
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
                ".ico"; ".woff"; ".woff2"; ".ttf"; ".eot"; ".map"; ".json";
                ".webp"; ".avif"; ".mp4"; ".webm"; ".wasm"]

(** Serve static file *)
let serve_static config path =
  (* Try dist first, then public *)
  let try_paths = [
    Filename.concat config.dist_path path;
    Filename.concat config.public_path path;
  ] in
  let rec try_serve = function
    | [] -> None
    | file_path :: rest ->
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
          | ".webp" -> "image/webp"
          | ".avif" -> "image/avif"
          | ".mp4" -> "video/mp4"
          | ".webm" -> "video/webm"
          | ".wasm" -> "application/wasm"
          | _ -> "application/octet-stream"
        in
        Some { status = 200; headers = [("Content-Type", content_type)]; body = content }
      else
        try_serve rest
  in
  try_serve try_paths

(** {1 Handler Logic} *)

(** Find matching route *)
let find_route config path =
  List.find_opt (fun route ->
    match Route_def.matches route path with
    | Some _ -> true
    | None -> false
  ) config.routes

(** Handle static output *)
let handle_static config request =
  let path = if request.path = "/" then "/index.html"
    else if Filename.extension request.path = "" then request.path ^ ".html"
    else request.path in
  let html_path = Filename.concat config.dist_path path in
  if Sys.file_exists html_path then
    let ic = open_in html_path in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    html_response content
  else
    error_response ~status:404 ~message:"Page not found"

(** Handle server output *)
let handle_server config request =
  match config.ssr_engine with
  | None -> error_response ~status:500 ~message:"SSR engine not configured"
  | Some engine ->
    let route = find_route config request.path in
    let islands = match route with
      | Some r -> r.Route_def.islands
      | None -> []
    in
    match Ssr.render engine ~url:request.path ~islands () with
    | Ok html ->
      let html = if config.dev_mode then
        Ssr.inject_dev_scripts html config.dev_port
      else html in
      html_response html
    | Error msg -> error_response ~status:500 ~message:msg

(** Handle hybrid output *)
let handle_hybrid config request =
  match find_route config request.path with
  | Some route when route.Route_def.prerender = Route_def.Static ->
    handle_static config request
  | Some route when route.Route_def.prerender = Route_def.OnDemand ->
    handle_server config request
  | _ ->
    (* Default: try static first, fall back to server *)
    let static_result = handle_static config request in
    if static_result.status = 404 then
      handle_server config request
    else
      static_result

(** Main request handler *)
let handle ~config ~request () =
  (* First check for static assets *)
  if is_static_asset request.path then
    match serve_static config request.path with
    | Some response -> response
    | None -> error_response ~status:404 ~message:"File not found"
  else
    (* Handle based on output mode *)
    match config.output with
    | Static -> handle_static config request
    | Server -> handle_server config request
    | Hybrid -> handle_hybrid config request

(** Catch-all handler *)
let catch_all_handler config request =
  handle ~config ~request ()

(** {1 API Routes} *)

(** Handle API route *)
let handle_api ~config ~request ~endpoint () =
  let _ = config in
  json_response (`Assoc [
    ("endpoint", `String endpoint);
    ("method", `String request.method_);
  ])

(** {1 Serialization} *)

(** Output to string *)
let output_to_string = function
  | Static -> "static"
  | Server -> "server"
  | Hybrid -> "hybrid"

(** Config to JSON *)
let config_to_json config =
  `Assoc [
    ("output", `String (output_to_string config.output));
    ("distPath", `String config.dist_path);
    ("pagesPath", `String config.pages_path);
    ("publicPath", `String config.public_path);
    ("devMode", `Bool config.dev_mode);
    ("devPort", `Int config.dev_port);
  ]
