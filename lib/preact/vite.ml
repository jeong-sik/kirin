(** Preact Vite Integration

    Vite dev server integration for Preact development.
    Handles HMR, aliasing, and build configuration. *)

(** {1 Vite Configuration} *)

(** Vite config options *)
type config = {
  port: int;
  host: string;
  manifest_path: string;
  dist_dir: string;
  prefix: string;
  hmr: bool;
  compat_mode: bool;
}

(** Default config *)
let default_config = {
  port = 5173;
  host = "localhost";
  manifest_path = "dist/.vite/manifest.json";
  dist_dir = "dist";
  prefix = "/";
  hmr = true;
  compat_mode = false;
}

(** {1 Development Mode} *)

(** Check if in development mode *)
let is_dev () =
  match Sys.getenv_opt "NODE_ENV" with
  | Some "production" -> false
  | _ ->
    match Sys.getenv_opt "PREACT_DEV" with
    | Some "true" -> true
    | Some "1" -> true
    | _ -> false

(** Dev server URL *)
let dev_url config =
  "http://" ^ config.host ^ ":" ^ string_of_int config.port

(** Generate dev script tag *)
let dev_script_tag config entry =
  let base = dev_url config in
  "<script type=\"module\" src=\"" ^ base ^ "/" ^ entry ^ "\"></script>"

(** Generate Vite client script for HMR *)
let hmr_script config =
  if config.hmr then
    let base = dev_url config in
    "<script type=\"module\" src=\"" ^ base ^ "/@vite/client\"></script>"
  else
    ""

(** {1 Preact Plugin Config} *)

(** Generate Vite config with Preact *)
let generate_vite_config config =
  let alias_config = if config.compat_mode then
    "    alias: {\n" ^
    "      'react': 'preact/compat',\n" ^
    "      'react-dom': 'preact/compat',\n" ^
    "      'react/jsx-runtime': 'preact/jsx-runtime',\n" ^
    "    },\n"
  else
    ""
  in
  "import { defineConfig } from 'vite';\n" ^
  "import preact from '@preact/preset-vite';\n\n" ^
  "export default defineConfig({\n" ^
  "  plugins: [preact()],\n" ^
  "  resolve: {\n" ^
  alias_config ^
  "  },\n" ^
  "  server: {\n" ^
  "    port: " ^ string_of_int config.port ^ ",\n" ^
  "    host: '" ^ config.host ^ "',\n" ^
  "  },\n" ^
  "  build: {\n" ^
  "    manifest: true,\n" ^
  "    outDir: '" ^ config.dist_dir ^ "',\n" ^
  "  },\n" ^
  "});\n"

(** {1 Static Serving} *)

(** Create static routes for production *)
let static_routes ?config () =
  let cfg = Option.value ~default:default_config config in
  (* In real implementation, would return Kirin routes *)
  [Kirin.get (cfg.prefix ^ "*") (fun _req ->
    Kirin.Response.html "<h1>Static Preact</h1>"
  )]

(** {1 Manifest Integration} *)

(** Load manifest and generate HTML *)
let with_manifest config f =
  match Manifest.load ~base_url:config.prefix config.manifest_path with
  | Ok manifest -> f manifest
  | Error msg -> Error msg

(** Generate production HTML with manifest *)
let production_html config ~entry ~title =
  with_manifest config (fun manifest ->
    let script = Manifest.script_tag manifest entry in
    let css = Manifest.css_tags manifest entry in
    let preload = Manifest.preload_hints manifest entry in
    Ok (
      "<!DOCTYPE html>\n" ^
      "<html>\n" ^
      "<head>\n" ^
      "  <title>" ^ title ^ "</title>\n" ^
      "  " ^ preload ^ "\n" ^
      "  " ^ css ^ "\n" ^
      "</head>\n" ^
      "<body>\n" ^
      "  <div id=\"app\"></div>\n" ^
      "  " ^ script ^ "\n" ^
      "</body>\n" ^
      "</html>"
    )
  )

(** Generate development HTML *)
let development_html config ~entry ~title =
  let hmr = hmr_script config in
  let script = dev_script_tag config entry in
  Ok (
    "<!DOCTYPE html>\n" ^
    "<html>\n" ^
    "<head>\n" ^
    "  <title>" ^ title ^ "</title>\n" ^
    "</head>\n" ^
    "<body>\n" ^
    "  <div id=\"app\"></div>\n" ^
    "  " ^ hmr ^ "\n" ^
    "  " ^ script ^ "\n" ^
    "</body>\n" ^
    "</html>"
  )

(** Generate HTML based on environment *)
let html config ~entry ~title =
  if is_dev () then
    development_html config ~entry ~title
  else
    production_html config ~entry ~title

(** {1 Preact Refresh} *)

(** Generate Preact Refresh preamble for HMR *)
let refresh_preamble config =
  if is_dev () && config.hmr then
    "<script type=\"module\">\n" ^
    "import RefreshRuntime from '" ^ dev_url config ^ "/@react-refresh';\n" ^
    "RefreshRuntime.injectIntoGlobalHook(window);\n" ^
    "window.$RefreshReg$ = () => {};\n" ^
    "window.$RefreshSig$ = () => (type) => type;\n" ^
    "window.__vite_plugin_react_preamble_installed__ = true;\n" ^
    "</script>"
  else
    ""

(** {1 Serialization} *)

(** Config to JSON *)
let config_to_json c =
  `Assoc [
    ("port", `Int c.port);
    ("host", `String c.host);
    ("manifestPath", `String c.manifest_path);
    ("distDir", `String c.dist_dir);
    ("prefix", `String c.prefix);
    ("hmr", `Bool c.hmr);
    ("compatMode", `Bool c.compat_mode);
  ]
