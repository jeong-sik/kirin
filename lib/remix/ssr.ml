(** Remix SSR Engine

    Server-side rendering with data loading and hydration.
    Supports streaming, deferred data, and error boundaries. *)

(** {1 SSR Configuration} *)

(** SSR config *)
type ssr_config = {
  entry_client: string;
  entry_server: string;
  public_path: string;
  enable_streaming: bool;
  defer_timeout: float;
}

(** Default config *)
let default_config = {
  entry_client = "/build/entry.client.js";
  entry_server = "/build/entry.server.js";
  public_path = "/build/";
  enable_streaming = true;
  defer_timeout = 5.0;
}

(** {1 Render Context} *)

(** Render context *)
type render_context = {
  url: string;
  matches: Route.match_result list;
  loader_data: (string * Yojson.Safe.t) list;
  action_data: Yojson.Safe.t option;
  errors: (string * string) list;
}

(** Create render context *)
let create_context ~url ~matches ~loader_data ?action_data ?(errors = []) () =
  { url; matches; loader_data; action_data; errors }

(** {1 Data Loading} *)

(** Load all data for matches *)
let load_data matches loader_ctx =
  matches |> List.filter_map (fun (m : Route.match_result) ->
    match m.route.loader with
    | None -> None
    | Some loader ->
      let ctx = Loader.with_params m.params loader_ctx in
      match loader ctx with
      | Loader.Data json -> Some (m.route.path, json)
      | Loader.Redirect _ -> None  (* Handle redirects separately *)
      | Loader.NotFound -> None
      | Loader.ServerError _ -> None
  )

(** Check for redirects in loader results *)
let check_redirects matches loader_ctx =
  matches |> List.find_map (fun (m : Route.match_result) ->
    match m.route.loader with
    | None -> None
    | Some loader ->
      let ctx = Loader.with_params m.params loader_ctx in
      match loader ctx with
      | Loader.Redirect (url, status) -> Some (url, status)
      | _ -> None
  )

(** {1 HTML Generation} *)

(** Generate script for hydration data *)
let hydration_script ~loader_data ~action_data =
  let data = `Assoc [
    ("loaderData", `Assoc loader_data);
    ("actionData", match action_data with Some d -> d | None -> `Null);
  ] in
  Printf.sprintf {|<script>
window.__remixContext = %s;
</script>|} (Yojson.Safe.to_string data)

(** Generate entry script *)
let entry_script config =
  Printf.sprintf {|<script type="module" src="%s"></script>|} config.entry_client

(** Generate meta tags from loader data *)
let meta_tags ~loader_data =
  (* Look for meta in loader data *)
  match List.assoc_opt "root" loader_data with
  | Some (`Assoc pairs) ->
    (match List.assoc_opt "meta" pairs with
     | Some (`List metas) ->
       metas |> List.filter_map (function
         | `Assoc m ->
           let name = match List.assoc_opt "name" m with
             | Some (`String n) -> Some n | _ -> None in
           let content = match List.assoc_opt "content" m with
             | Some (`String c) -> Some c | _ -> None in
           (match name, content with
            | Some n, Some c ->
              Some (Printf.sprintf {|<meta name="%s" content="%s">|} n c)
            | _ -> None)
         | _ -> None
       ) |> String.concat "\n"
     | _ -> "")
  | _ -> ""

(** Generate links from loader data *)
let link_tags ~loader_data =
  match List.assoc_opt "root" loader_data with
  | Some (`Assoc pairs) ->
    (match List.assoc_opt "links" pairs with
     | Some (`List links) ->
       links |> List.filter_map (function
         | `Assoc l ->
           let rel = match List.assoc_opt "rel" l with
             | Some (`String r) -> r | _ -> "stylesheet" in
           let href = match List.assoc_opt "href" l with
             | Some (`String h) -> Some h | _ -> None in
           (match href with
            | Some h ->
              Some (Printf.sprintf {|<link rel="%s" href="%s">|} rel h)
            | None -> None)
         | _ -> None
       ) |> String.concat "\n"
     | _ -> "")
  | _ -> ""

(** {1 Full Page Rendering} *)

(** Render full HTML document *)
let render_document ~config ~context ~body =
  let meta = meta_tags ~loader_data:context.loader_data in
  let links = link_tags ~loader_data:context.loader_data in
  let hydration = hydration_script
    ~loader_data:context.loader_data
    ~action_data:context.action_data in
  let entry = entry_script config in

  Printf.sprintf {|<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
%s
%s
</head>
<body>
  <div id="root">%s</div>
%s
%s
</body>
</html>|} meta links body hydration entry

(** {1 Streaming SSR} *)

(** Stream chunk type *)
type stream_chunk =
  | Shell of string
  | DeferredData of string * Yojson.Safe.t
  | Complete

(** Render with streaming (shell first, then deferred data) *)
let render_streaming ~config:_ ~context ~render_shell ~on_chunk =
  (* Send shell immediately *)
  let shell = render_shell context in
  on_chunk (Shell shell);

  (* Send deferred data as it resolves *)
  (* In real implementation, would resolve deferred loaders *)
  on_chunk Complete

(** {1 Error Handling} *)

(** Render error boundary *)
let render_error_boundary ~error ~route =
  Printf.sprintf {|<div class="remix-error-boundary" data-route="%s">
  <h1>Error</h1>
  <p>%s</p>
</div>|} route error

(** Render catch boundary (404, etc) *)
let render_catch_boundary ~status ~message =
  Printf.sprintf {|<div class="remix-catch-boundary">
  <h1>%d</h1>
  <p>%s</p>
</div>|} status message

(** {1 Kirin Integration} *)

(** Create SSR handler *)
let handler ~config ~routes ~render_component =
  fun req ->
    let url = Kirin.Request.path req in
    let path_parts = String.split_on_char '/' url
      |> List.filter (fun s -> s <> "") in

    (* Find matching routes *)
    let matches = Route.find_matches routes path_parts [] in

    match matches with
    | [] ->
      Kirin.Response.html (render_catch_boundary ~status:404 ~message:"Page not found")
    | _ ->
      (* Create loader context *)
      let loader_ctx = Loader.context_of_request req in

      (* Check for redirects *)
      (match check_redirects matches loader_ctx with
       | Some (url, status) ->
         Kirin.Response.redirect ~status:(Http.Status.of_int status) url
       | None ->
         (* Load all data *)
         let loader_data = load_data matches loader_ctx in
         let context = create_context ~url ~matches ~loader_data () in

         (* Render component *)
         let body = render_component context in
         let html = render_document ~config ~context ~body in
         Kirin.Response.html html)

(** {1 Serialization} *)

(** Config to JSON *)
let config_to_json config =
  `Assoc [
    ("entryClient", `String config.entry_client);
    ("entryServer", `String config.entry_server);
    ("publicPath", `String config.public_path);
    ("enableStreaming", `Bool config.enable_streaming);
    ("deferTimeout", `Float config.defer_timeout);
  ]

(** Context to JSON *)
let context_to_json ctx =
  `Assoc [
    ("url", `String ctx.url);
    ("matches", `List (List.map Route.match_to_json ctx.matches));
    ("loaderData", `Assoc ctx.loader_data);
    ("actionData", match ctx.action_data with Some d -> d | None -> `Null);
    ("errors", `Assoc (List.map (fun (k, v) -> (k, `String v)) ctx.errors));
  ]
