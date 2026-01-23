(** Solid.js SSR Handler

    Kirin route handler for Solid.js SSR. *)

(* Note: Don't `open Kirin` to avoid shadowing local Router module *)

(** {1 Handler Configuration} *)

(** SSR handler options *)
type options = {
  ssr_engine: Ssr.t option;
  routes: Router.route list;
  fallback_html: string;
  dev_mode: bool;
  vite_port: int;
}

(** Default options *)
let default_options = {
  ssr_engine = None;
  routes = [];
  fallback_html = "<div id=\"app\">Loading...</div>";
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
      ~entry_script:"/src/entry-client.tsx"
      ~ssr_html:result.Protocol.html
      ()
    in
    Kirin.Response.html ~status html

(** Create fallback HTML response *)
let fallback_response ~options:_ =
  let html = Hydrate.spa
    ~title:"Loading..."
    ~entry_script:"/src/entry-client.tsx"
    ()
  in
  Kirin.Response.html html

(** {1 Handlers} *)

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

(** Data loader handler *)
let data_handler ~options:_ ~loader req =
  let url = Kirin.Request.path req in
  (* Note: params are extracted by Kirin's router and available via Request.param *)
  let params = [] in  (* TODO: Get from route context when available *)
  match loader ~url ~params with
  | Router.Data data ->
    Kirin.Response.json (`Assoc [
      ("type", `String "data");
      ("data", data);
    ])
  | Router.Redirect (redirect_url, status) ->
    Kirin.Response.json (`Assoc [
      ("type", `String "redirect");
      ("url", `String redirect_url);
      ("status", `Int status);
    ])
  | Router.NotFound ->
    Kirin.Response.json ~status:`Not_found (`Assoc [
      ("type", `String "notFound");
    ])
  | Router.ServerError msg ->
    Kirin.Response.json ~status:`Internal_server_error (`Assoc [
      ("type", `String "error");
      ("message", `String msg);
    ])

(** Health check handler *)
let health_handler ~options _req =
  match options.ssr_engine with
  | None ->
    Kirin.Response.json (`Assoc [
      ("ok", `Bool true);
      ("ssr", `Bool false);
    ])
  | Some engine ->
    let stats = Ssr.stats engine in
    Kirin.Response.json (`Assoc [
      ("ok", `Bool (stats.workers_ready > 0));
      ("ssr", `Bool true);
      ("stats", `Assoc [
        ("totalRenders", `Int stats.total_renders);
        ("errors", `Int stats.errors);
        ("cacheSize", `Int stats.cache_size);
        ("workersReady", `Int stats.workers_ready);
        ("workersTotal", `Int stats.workers_total);
      ]);
    ])

(** {1 Vite Dev Proxy} *)

(** Check if request is for Vite dev server *)
let is_vite_request path =
  String.length path > 0 &&
  (path.[0] = '@' ||
   List.exists (fun ext -> Filename.check_suffix path ext)
     [".tsx"; ".ts"; ".jsx"; ".js"; ".css"; ".svg"; ".png"; ".jpg"])

(** Proxy request to Vite dev server *)
let vite_proxy ~port path =
  (* In real implementation, this would make HTTP request to Vite *)
  (* For now, return redirect hint *)
  let vite_url = Printf.sprintf "http://localhost:%d%s" port path in
  Kirin.Response.redirect ~status:`Temporary_redirect vite_url

(** {1 Route Setup} *)

(** Create Kirin routes for Solid.js app *)
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
  ] else [] in

  (* Page catch-all *)
  let page_route = Kirin.get "/*" page in

  base @ dev_routes @ [page_route]

(** Create data routes for loaders *)
let data_routes ~loaders =
  List.map (fun (path, loader) ->
    Kirin.get (path ^ ".data") (data_handler ~options:default_options ~loader)
  ) loaders
