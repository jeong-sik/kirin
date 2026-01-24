(** Vue/Nuxt Kirin Handler Integration

    Kirin route handlers for Vue/Nuxt SSR. *)

(** {1 Handler Configuration} *)

(** Handler mode *)
type mode =
  | SPA              (* Client-side only *)
  | Hydration        (* Server shell + client hydrate *)
  | SSR              (* Full server rendering *)
  | Streaming        (* Progressive SSR *)

(** Handler config *)
type config = {
  mode: mode;
  manifest_path: string;
  entry_point: string;
  ssr_config: Ssr.config option;
  streaming_config: Streaming.config option;
  dev_mode: bool;
  dev_port: int;
}

(** Default config *)
let default_config = {
  mode = SSR;
  manifest_path = "dist/.vite/manifest.json";
  entry_point = "src/entry-client.ts";
  ssr_config = None;
  streaming_config = None;
  dev_mode = false;
  dev_port = 3000;
}

(** {1 Request Context} *)

(** Request info *)
type request_info = {
  path: string;
  query: (string * string) list;
  headers: (string * string) list;
  method_: string;
  body: string option;
}

(** Create request info from path *)
let request_info ~path ?(query=[]) ?(headers=[]) ?(method_="GET") ?body () = {
  path;
  query;
  headers;
  method_;
  body;
}

(** {1 Response Building} *)

(** Response type *)
type response = {
  status: int;
  headers: (string * string) list;
  body: string;
}

(** Create HTML response *)
let html_response ?(status=200) ?(headers=[]) body = {
  status;
  headers = ("Content-Type", "text/html; charset=utf-8") :: headers;
  body;
}

(** Create redirect response *)
let redirect_response ~location ?(status=302) () = {
  status;
  headers = [("Location", location)];
  body = "";
}

(** Create error response *)
let error_response ?(status=500) message = {
  status;
  headers = [("Content-Type", "text/html; charset=utf-8")];
  body = Hydrate.error_page ~status ~message;
}

(** {1 SPA Handler} *)

(** Handle SPA request *)
let handle_spa ~config ~title () =
  let entry_script = Printf.sprintf "/_nuxt/%s" config.entry_point in
  let html = Hydrate.spa ~title ~entry_script () in
  html_response html

(** {1 Hydration Handler} *)

(** Handle hydration request *)
let handle_hydration ~title ~payload ~entry_script () =
  let options = {
    Hydrate.default_options with
    title;
    scripts = [entry_script];
  } in
  let html = Hydrate.render ~options ~ssr_html:"" ~payload:(Data.payload_script payload) ~entry_script in
  html_response html

(** {1 SSR Handler} *)

(** Handle SSR request *)
let handle_ssr ~engine ~url () =
  match Ssr.render engine ~url () with
  | Ok result ->
    {
      status = result.Ssr.status;
      headers = result.Ssr.headers;
      body = result.Ssr.html;
    }
  | Error msg ->
    error_response ~status:500 msg

(** Handle SSR with fallback *)
let handle_ssr_with_fallback ~engine ~url ~fallback () =
  let html = Ssr.render_with_fallback engine ~url ~fallback in
  html_response html

(** {1 Streaming Handler} *)

(** Streaming response builder *)
type streaming_response = {
  headers: (string * string) list;
  write_chunk: string -> unit;
  finish: unit -> unit;
}

(** Handle streaming SSR request *)
let handle_streaming ~ctx ~shell ~chunks () =
  (* Start with shell *)
  Streaming.start ctx;
  let shell_chunk = Streaming.shell_chunk ~html:shell in
  let initial = Streaming.send_chunk ctx shell_chunk in

  (* Send additional chunks *)
  let content_chunks = List.map (fun c -> Streaming.send_chunk ctx c) chunks in

  (* Finish *)
  let end_marker = Streaming.finish ctx in

  html_response ~headers:(Streaming.streaming_headers ())
    (String.concat "" (initial :: content_chunks @ [end_marker]))

(** {1 Dev Mode Handler} *)

(** Handle dev mode proxy *)
let handle_dev_proxy ~port ~path () =
  (* In real implementation, this would proxy to Vite dev server *)
  let msg = Printf.sprintf "Proxying to http://localhost:%d%s" port path in
  html_response ~status:200 msg

(** {1 API Handler} *)

(** Handle API route *)
let handle_api ~handler ~request () =
  match handler request with
  | Action.ActionData json ->
    {
      status = 200;
      headers = [("Content-Type", "application/json")];
      body = Yojson.Safe.to_string json;
    }
  | Action.ActionRedirect (status, url) ->
    redirect_response ~location:url ~status ()
  | Action.ActionError (status, message) ->
    {
      status;
      headers = [("Content-Type", "application/json")];
      body = Yojson.Safe.to_string (`Assoc [
        ("error", `String message);
        ("status", `Int status);
      ]);
    }
  | Action.ActionEmpty ->
    { status = 204; headers = []; body = "" }
  | Action.ActionStream _ ->
    { status = 200; headers = [("Content-Type", "text/event-stream")]; body = "" }

(** {1 Unified Handler} *)

(** Main request handler *)
let handle ~config ~request () =
  let url = request.path in

  (* Check if API route *)
  if String.length url >= 4 && String.sub url 0 4 = "/api" then
    handle_api ~handler:(fun _ -> Action.ActionData (`Assoc [("message", `String "OK")])) ~request ()

  (* Dev mode *)
  else if config.dev_mode then
    handle_dev_proxy ~port:config.dev_port ~path:url ()

  (* Based on mode *)
  else match config.mode with
  | SPA ->
    handle_spa ~config ~title:"Vue App" ()

  | Hydration ->
    let payload = Data.empty_payload ~route_path:url in
    let entry = Printf.sprintf "/_nuxt/%s" config.entry_point in
    handle_hydration ~title:"Vue App" ~payload ~entry_script:entry ()

  | SSR ->
    let engine = Ssr.create (Option.value ~default:Ssr.default_config config.ssr_config) in
    handle_ssr ~engine ~url ()

  | Streaming ->
    let ctx = Streaming.create_context
      ?config:config.streaming_config () in
    let shell = Hydrate.spa ~title:"Vue App" () in
    handle_streaming ~ctx ~shell ~chunks:[] ()

(** {1 Route Definitions} *)

(** Route matcher result *)
type route_match = {
  params: (string * string) list;
  route_name: string option;
  handler_fn: request_info -> response;
}

(** Create catch-all handler for Vue app *)
let catch_all_handler config =
  fun request ->
    handle ~config ~request ()

(** {1 Middleware} *)

(** Middleware function *)
type middleware = request_info -> response option

(** Auth middleware *)
let auth_middleware ~check_auth =
  fun request ->
    if check_auth request.headers then None
    else Some (error_response ~status:401 "Unauthorized")

(** Logging middleware *)
let logging_middleware ~log =
  fun request ->
    log (Printf.sprintf "[%s] %s" request.method_ request.path);
    None

(** Apply middleware stack *)
let with_middleware middlewares handler request =
  let rec apply = function
    | [] -> handler request
    | mw :: rest ->
      match mw request with
      | Some response -> response
      | None -> apply rest
  in
  apply middlewares

(** {1 Serialization} *)

(** Mode to string *)
let mode_to_string = function
  | SPA -> "spa"
  | Hydration -> "hydration"
  | SSR -> "ssr"
  | Streaming -> "streaming"

(** Config to JSON *)
let config_to_json c =
  `Assoc [
    ("mode", `String (mode_to_string c.mode));
    ("manifestPath", `String c.manifest_path);
    ("entryPoint", `String c.entry_point);
    ("devMode", `Bool c.dev_mode);
    ("devPort", `Int c.dev_port);
  ]

(** Response to JSON *)
let response_to_json r =
  `Assoc [
    ("status", `Int r.status);
    ("headers", `Assoc (List.map (fun (k, v) -> (k, `String v)) r.headers));
    ("bodyLength", `Int (String.length r.body));
  ]
