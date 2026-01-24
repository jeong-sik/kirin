(** Kirin Svelte SSR

    SvelteKit-style server-side rendering integration for Kirin. *)

(** {1 Re-exports} *)

(** Route definitions *)
module Route_def = Route_def

(** File-based routing *)
module File_router = File_router

(** Load functions *)
module Loader = Loader

(** Form actions *)
module Action = Action

(** Route manifest *)
module Manifest = Manifest

(** Route preloading *)
module Preload = Preload

(** Meta tags *)
module Meta = Meta

(** Initial data *)
module Data = Data

(** Hydration *)
module Hydrate = Hydrate

(** SSR protocol *)
module Protocol = Protocol

(** SSR workers *)
module Worker = Worker

(** SSR engine *)
module Ssr = Ssr

(** Streaming SSR *)
module Streaming = Streaming

(** Route handler *)
module Handler = Handler

(** Code generation *)
module Codegen = Codegen

(** {1 Quick Start} *)

(** Create SvelteKit-style handler with SSR *)
let handler ~bundle ?num_workers ?cache_ttl () =
  let config = Ssr.{
    default_config with
    bundle;
    num_workers = Option.value ~default:4 num_workers;
    cache_ttl = Option.value ~default:60 cache_ttl;
  } in
  let engine = Ssr.create config in
  let options = Handler.{
    default_options with
    ssr_engine = Some engine;
    dev_mode = false;
  } in
  Handler.routes ~options

(** Create development handler (no SSR, Vite proxy) *)
let dev_handler ?(vite_port=5173) () =
  let options = Handler.{
    default_options with
    ssr_engine = None;
    dev_mode = true;
    vite_port;
  } in
  Handler.routes ~options

(** {1 Manifest Loading} *)

(** Load manifest from file *)
let load_manifest path =
  Manifest.load_file path

(** Build manifest from routes directory *)
let scan_routes _dir =
  (* Simplified - would recursively scan directory *)
  let files = ["index", ["+page.svelte"]] in
  let discovered = List.filter_map (fun (path, fs) ->
    File_router.discover_route path fs
  ) files in
  Manifest.from_discovered_routes discovered

(** {1 Route Helpers} *)

(** Create page route with loader *)
let page ~path ~loader =
  Route_def.page path ~loader:(Route_def.server_loader loader)

(** Create page route with actions *)
let page_with_actions ~path ~loader ~actions =
  let actions = List.map (fun (name, handler) ->
    if name = "default" then
      Route_def.default_action handler
    else
      Route_def.named_action name handler
  ) actions in
  Route_def.page path
    ~loader:(Route_def.server_loader loader)
    ~actions

(** {1 Response Helpers} *)

(** Return load data *)
let data json = Loader.data json

(** Return redirect *)
let redirect ?(status=303) url = Loader.redirect ~status url

(** Return error *)
let error status message = Loader.error status message

(** Return not found *)
let not_found = Loader.not_found

(** {1 Action Helpers} *)

(** Return action success *)
let success ?(data=`Assoc []) () = Action.success ~data ()

(** Return action failure *)
let fail ?(status=400) errors = Action.fail ~status errors

(** Return action redirect *)
let action_redirect url = Action.redirect url

(** {1 Meta Helpers} *)

(** Create page meta *)
let meta ~title ?description ?image ?canonical () =
  Meta.page ~title ?description:description ?og_image:image ?canonical_url:canonical ()

(** {1 Preload Helpers} *)

(** Create preload link *)
let preload_link ~href ?strategy text =
  let options = match strategy with
    | Some `Hover -> Some Preload.{ Preload.default_options with strategy = Hover }
    | Some `Tap -> Some Preload.{ Preload.default_options with strategy = Tap }
    | Some `Off -> Some Preload.no_preload
    | None -> None
  in
  Preload.link ~href ?options text

(** {1 Streaming} *)

(** Create streaming response *)
let stream ~title ~entry_script content =
  let stream = Streaming.create () in
  Streaming.add_chunk stream (Streaming.Html content);
  Streaming.complete stream;
  Streaming.streaming_response stream ~title ~entry_script

(** {1 Example Usage}

{[
  (* Basic SvelteKit-style setup *)
  let routes = Kirin_svelte.handler ~bundle:"dist/server/entry.js" ()

  (* With custom loader *)
  let user_loader ctx =
    let id = Loader.param_exn ctx "id" in
    Kirin_svelte.data (`Assoc [("user", fetch_user id)])

  let user_routes = Handler.load_routes ~loaders:[
    ("/users/[id]", user_loader);
  ]

  (* With form action *)
  let login_action ctx form =
    let email = Action.get_text_exn form "email" in
    let password = Action.get_text_exn form "password" in
    if authenticate email password then
      Kirin_svelte.action_redirect "/dashboard"
    else
      Kirin_svelte.fail (`Assoc [("email", `String "Invalid credentials")])

  let action_routes = Handler.action_routes ~actions_map:[
    ("/login", [Action.default login_action]);
  ]
]}
*)
