(** Vite Dev Server Integration

    Provides dev server proxy for HMR support and production static serving.
    Automatically detects dev vs production mode.
*)

(** Check if Vite dev server is running on given port *)
let is_dev_server_running ?(host = "localhost") ~port () =
  try
    let addr = Unix.inet_addr_of_string "127.0.0.1" in
    let sockaddr = Unix.ADDR_INET (addr, port) in
    let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.set_nonblock sock;
    (try Unix.connect sock sockaddr with Unix.Unix_error _ -> ());
    Unix.close sock;
    ignore host;
    true
  with _ -> false

(** Check if running in development mode *)
let is_dev () =
  match Sys.getenv_opt "NODE_ENV" with
  | Some "production" -> false
  | Some "development" -> true
  | _ ->
    (* Default: check if vite dev server port is open *)
    is_dev_server_running ~port:5173 ()

(** Vite dev server configuration *)
type dev_config = {
  host: string;
  port: int;
  https: bool;
}

let default_dev_config = {
  host = "localhost";
  port = 5173;
  https = false;
}

(** Build Vite dev server URL *)
let dev_server_url ?(config = default_dev_config) path =
  let scheme = if config.https then "https" else "http" in
  Printf.sprintf "%s://%s:%d%s" scheme config.host config.port path

(** Create middleware that proxies to Vite dev server *)
let dev_proxy_middleware ?(_config = default_dev_config) () =
  fun (handler : Kirin.handler) (req : Kirin.Request.t) ->
    let path = Kirin.Request.path req in
    (* Check if this is a Vite-related request *)
    let is_vite_request =
      String.length path >= 7 && String.sub path 0 7 = "/@vite/"
      || String.length path >= 5 && String.sub path 0 5 = "/@fs/"
      || String.length path >= 12 && String.sub path 0 12 = "/@react-ref"
      || String.length path >= 4 && String.sub path 0 4 = "/src"
      || String.length path >= 12 && String.sub path 0 12 = "/node_module"
      || Filename.check_suffix path ".tsx"
      || Filename.check_suffix path ".ts"
      || Filename.check_suffix path ".jsx"
    in
    if is_vite_request then
      (* Would proxy to Vite - for now, pass through *)
      handler req
    else
      handler req

(** Production static serving configuration *)
type prod_config = {
  manifest_path: string;
  dist_dir: string;
  base_url: string;
  index_fallback: bool;  (* SPA fallback to index.html *)
}

let default_prod_config = {
  manifest_path = "dist/.vite/manifest.json";
  dist_dir = "dist";
  base_url = "";
  index_fallback = true;
}

(** Load manifest from production config *)
let load_manifest config =
  match Manifest.load config.manifest_path with
  | Ok m -> m
  | Error _ -> []

(** Determine MIME type from file extension *)
let mime_type_of path =
  match Filename.extension path |> String.lowercase_ascii with
  | ".html" | ".htm" -> "text/html; charset=utf-8"
  | ".css" -> "text/css; charset=utf-8"
  | ".js" | ".mjs" -> "application/javascript; charset=utf-8"
  | ".json" -> "application/json; charset=utf-8"
  | ".png" -> "image/png"
  | ".jpg" | ".jpeg" -> "image/jpeg"
  | ".gif" -> "image/gif"
  | ".svg" -> "image/svg+xml"
  | ".webp" -> "image/webp"
  | ".woff" -> "font/woff"
  | ".woff2" -> "font/woff2"
  | ".ttf" -> "font/ttf"
  | ".ico" -> "image/x-icon"
  | _ -> "application/octet-stream"

(** Serve a static file *)
let serve_file file_path =
  try
    let content = In_channel.(with_open_bin file_path input_all) in
    let mime = mime_type_of file_path in
    Kirin.Response.make ~status:`OK content
    |> Kirin.Response.with_header "Content-Type" mime
    |> Kirin.Response.with_header "Cache-Control" "public, max-age=31536000, immutable"
  with Sys_error _ ->
    Kirin.Response.not_found ()

(** Create handler for serving static Vite build *)
let static_handler ~config ~manifest req =
  let path = Kirin.Request.path req in
  let file_path =
    if path = "/" then
      Filename.concat config.dist_dir "index.html"
    else
      Filename.concat config.dist_dir (String.sub path 1 (String.length path - 1))
  in
  if Sys.file_exists file_path && not (Sys.is_directory file_path) then
    serve_file file_path
  else if config.index_fallback then
    (* SPA fallback *)
    let index_path = Filename.concat config.dist_dir "index.html" in
    if Sys.file_exists index_path then
      serve_file index_path
    else
      Kirin.Response.not_found ()
  else begin
    ignore manifest;
    Kirin.Response.not_found ()
  end

(** Create routes for static serving *)
let static_routes ?(config = default_prod_config) () =
  let manifest = load_manifest config in
  [
    Kirin.get "/*" (fun req -> static_handler ~config ~manifest req);
  ]

(** Unified routes: dev proxy or static depending on mode *)
let routes ?(dev_config = default_dev_config) ?(prod_config = default_prod_config) () =
  if is_dev () then begin
    (* In dev mode, return empty routes - Vite serves directly *)
    ignore dev_config;
    []
  end
  else
    static_routes ~config:prod_config ()

(** Generate script tag for development mode (direct Vite serve) *)
let dev_script_tag ?(config = default_dev_config) ~entry () =
  let url = dev_server_url ~config ("/" ^ entry) in
  Printf.sprintf {|<script type="module" src="%s"></script>|} url

(** Generate the Vite client script for HMR *)
let hmr_script ?(config = default_dev_config) () =
  let url = dev_server_url ~config "/@vite/client" in
  Printf.sprintf {|<script type="module" src="%s"></script>|} url

(** Generate React refresh preamble for HMR *)
let react_refresh_preamble ?(config = default_dev_config) () =
  let base_url = dev_server_url ~config "" in
  let refresh_url = base_url ^ "/@react-refresh" in
  let lines = [
    "<script type=\"module\">";
    "import RefreshRuntime from '" ^ refresh_url ^ "'";
    "RefreshRuntime.injectIntoGlobalHook(window)";
    "window.$RefreshReg$ = () => {}";
    "window.$RefreshSig$ = () => (type) => type";
    "window.__vite_plugin_react_preamble_installed__ = true";
    "</script>";
  ] in
  String.concat "\n" lines

(** Complete dev mode head scripts *)
let dev_head_scripts ?(config = default_dev_config) () =
  hmr_script ~config () ^ "\n" ^ react_refresh_preamble ~config ()

(** Detect appropriate script tag based on mode *)
let script_tag ~manifest ~entry () =
  if is_dev () then
    dev_script_tag ~entry ()
  else
    Assets.script_tag ~manifest entry

(** Detect appropriate CSS handling based on mode *)
let css_tags ~manifest ~entry () =
  if is_dev () then
    ""  (* Vite injects CSS in dev mode *)
  else
    Assets.css_tags ~manifest entry

(** Build info for debugging *)
type build_info = {
  mode: [ `Dev | `Prod ];
  manifest_loaded: bool;
  entry_count: int;
  vite_version: string option;
}

let get_build_info ?(prod_config = default_prod_config) () =
  let mode = if is_dev () then `Dev else `Prod in
  let manifest = load_manifest prod_config in
  {
    mode;
    manifest_loaded = manifest <> [];
    entry_count = List.length (Manifest.entries manifest);
    vite_version = None;  (* Would parse from package.json *)
  }

let build_info_to_string info =
  let mode_str = match info.mode with `Dev -> "development" | `Prod -> "production" in
  Printf.sprintf "Vite Build Info:\n  Mode: %s\n  Manifest: %s\n  Entries: %d"
    mode_str
    (if info.manifest_loaded then "loaded" else "not found")
    info.entry_count
