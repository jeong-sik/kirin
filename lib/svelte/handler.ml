(** Svelte SSR Handler

    Kirin route handler for SvelteKit SSR. *)

(* Note: Don't `open Kirin` to avoid shadowing local modules *)

(** {1 Handler Configuration} *)

(** SSR handler options *)
type options = {
  ssr_engine: Ssr.t option;
  manifest: Manifest.t option;
  fallback_html: string;
  dev_mode: bool;
  vite_port: int;
}

(** Default options *)
let default_options = {
  ssr_engine = None;
  manifest = None;
  fallback_html = "<div id=\"svelte\">Loading...</div>";
  dev_mode = false;
  vite_port = 5173;
}

(** {1 Response Helpers} *)

(** Create HTML response from SSR result *)
let html_response ~options:_ result =
  let status = match result.Protocol.status with
    | 200 -> `OK
    | 301 -> `Moved_permanently
    | 302 -> `Found
    | 400 -> `Bad_request
    | 404 -> `Not_found
    | 500 -> `Internal_server_error
    | _ -> `OK
  in

  match result.Protocol.redirect with
  | Some url ->
    Kirin.Response.redirect ~status url
  | None ->
    (* Build full HTML with head tags *)
    let html = Hydrate.with_ssr
      ~title:""  (* Title should be in SSR HTML *)
      ~entry_script:"/src/entry-client.ts"
      ~ssr_html:result.Protocol.html
      ()
    in
    Kirin.Response.html ~status html

(** Create fallback HTML response *)
let fallback_response ~options:_ =
  let html = Hydrate.spa
    ~title:"Loading..."
    ~entry_script:"/src/entry-client.ts"
    ()
  in
  Kirin.Response.html html

(** {1 Page Handlers} *)

(** SSR page handler *)
let page_handler ~options req =
  match options.ssr_engine with
  | None ->
    (* No SSR engine - return SPA shell *)
    fallback_response ~options
  | Some engine ->
    let url = Kirin.Request.path req in
    let props = match Kirin.Request.json_body req with
      | Ok json -> json
      | Error _ -> `Assoc []
    in
    match Ssr.render engine ~url ~props () with
    | Ok result ->
      html_response ~options result
    | Error _ ->
      (* SSR failed - return fallback *)
      fallback_response ~options

(** {1 Load Handler} *)

(** Data loader handler for +page.server.ts style loading *)
let load_handler ~loader req =
  let url = Kirin.Request.path req in
  let params = [] in  (* TODO: Extract from route context *)

  let ctx = Loader.create_context
    ~url ~params ~route_id:url
    ~cookies:[] ~headers:[]
    ()
  in

  match loader ctx with
  | Loader.LoadData data ->
    Kirin.Response.json (`Assoc [
      ("type", `String "data");
      ("data", data);
    ])
  | Loader.LoadRedirect (status, location) ->
    Kirin.Response.json (`Assoc [
      ("type", `String "redirect");
      ("status", `Int status);
      ("location", `String location);
    ])
  | Loader.LoadError (status, message) ->
    let status_code = match status with
      | 400 -> `Bad_request
      | 401 -> `Unauthorized
      | 403 -> `Forbidden
      | 404 -> `Not_found
      | 500 -> `Internal_server_error
      | _ -> `Internal_server_error
    in
    Kirin.Response.json ~status:status_code (`Assoc [
      ("type", `String "error");
      ("status", `Int status);
      ("message", `String message);
    ])
  | Loader.LoadNotFound ->
    Kirin.Response.json ~status:`Not_found (`Assoc [
      ("type", `String "notFound");
    ])

(** {1 Action Handler} *)

(** Form action handler for +page.server.ts style actions *)
let action_handler ~actions req =
  let url = Kirin.Request.path req in

  (* Extract action name from URL query param *)
  let action_name = Kirin.Request.query "action" req in

  (* Parse form data - form_body returns (string * string list) list *)
  let raw_form = Kirin.Request.form_body req in
  let form_data =
    List.concat_map (fun (k, values) ->
      match values with
      | [v] -> [(k, Action.Text v)]
      | vs -> [(k, Action.Multiple (List.map (fun v -> Action.Text v) vs))]
    ) raw_form
  in

  (* Find and execute action *)
  let action = Action.find_action actions action_name in
  match action with
  | None ->
    Kirin.Response.json ~status:`Not_found (`Assoc [
      ("type", `String "error");
      ("message", `String "Action not found");
    ])
  | Some a ->
    let ctx = Action.create_context
      ~url ~route_id:url
      ~headers:[] ~cookies:[]
      ()
    in
    match a.Action.handler ctx form_data with
    | Action.ActionSuccess data ->
      Kirin.Response.json (`Assoc [
        ("type", `String "success");
        ("data", data);
      ])
    | Action.ActionFail (status, data) ->
      let status_code = match status with
        | 400 -> `Bad_request
        | 422 -> `Unprocessable_entity
        | _ -> `Bad_request
      in
      Kirin.Response.json ~status:status_code (`Assoc [
        ("type", `String "fail");
        ("status", `Int status);
        ("data", data);
      ])
    | Action.ActionRedirect (status, location) ->
      Kirin.Response.json (`Assoc [
        ("type", `String "redirect");
        ("status", `Int status);
        ("location", `String location);
      ])
    | Action.ActionError (status, message) ->
      Kirin.Response.json ~status:`Internal_server_error (`Assoc [
        ("type", `String "error");
        ("status", `Int status);
        ("message", `String message);
      ])

(** {1 Health Handler} *)

(** Health check handler *)
let health_handler ~options _req =
  match options.ssr_engine with
  | None ->
    Kirin.Response.json (`Assoc [
      ("ok", `Bool true);
      ("ssr", `Bool false);
    ])
  | Some engine ->
    let stats = Ssr.get_stats engine in
    Kirin.Response.json (`Assoc [
      ("ok", `Bool (stats.Ssr.workers_ready > 0));
      ("ssr", `Bool true);
      ("stats", Ssr.stats_to_json stats);
    ])

(** {1 Vite Dev Proxy} *)

(** Check if request is for Vite dev server *)
let is_vite_request path =
  String.length path > 0 &&
  (path.[0] = '@' ||
   List.exists (fun ext -> Filename.check_suffix path ext)
     [".svelte"; ".ts"; ".js"; ".css"; ".svg"; ".png"; ".jpg"])

(** Proxy request to Vite dev server *)
let vite_proxy ~port path =
  let vite_url = Printf.sprintf "http://localhost:%d%s" port path in
  Kirin.Response.redirect ~status:`Temporary_redirect vite_url

(** {1 Route Setup} *)

(** Create Kirin routes for SvelteKit app *)
let routes ~options =
  let page = page_handler ~options in
  let health = health_handler ~options in

  (* Base routes *)
  let base = [
    Kirin.get "/_health" health;
  ] in

  (* Dev mode routes *)
  let dev_routes = if options.dev_mode then [
    Kirin.get "/@vite/*" (fun req ->
      vite_proxy ~port:options.vite_port (Kirin.Request.path req)
    );
    Kirin.get "/src/*" (fun req ->
      vite_proxy ~port:options.vite_port (Kirin.Request.path req)
    );
    Kirin.get "/@svelte/*" (fun req ->
      vite_proxy ~port:options.vite_port (Kirin.Request.path req)
    );
  ] else [] in

  (* Page catch-all *)
  let page_route = Kirin.get "/*" page in

  base @ dev_routes @ [page_route]

(** Create load data routes *)
let load_routes ~loaders =
  List.map (fun (path, loader) ->
    Kirin.get (path ^ "/__data.json") (load_handler ~loader)
  ) loaders

(** Create action routes *)
let action_routes ~actions_map =
  List.map (fun (path, actions) ->
    Kirin.post path (action_handler ~actions)
  ) actions_map
