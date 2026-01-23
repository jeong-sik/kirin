(** Kirin React Integration

    React support for Kirin web framework.
    Provides three levels of integration:

    - Level 1: Static (Vite build serving, HMR proxy)
    - Level 2: Hydration (HTML shell + __INITIAL_DATA__)
    - Level 3: Full SSR (Node.js worker pool)

    @see <https://github.com/jeong-sik/kirin>
*)

(** {1 Level 1: Static/Vite Integration} *)

(** Vite manifest parsing *)
module Manifest = Manifest

(** Asset URL resolution with cache busting *)
module Assets = Assets

(** Vite dev server integration *)
module Vite = Vite

(** {1 Level 2: Hydration} *)

(** SEO meta tag helpers *)
module Meta = Meta

(** Initial data serialization (XSS-safe) *)
module Data = Data

(** HTML shell generation for hydration *)
module Hydrate = Hydrate

(** {1 Level 3: Full SSR} *)

(** JSON-RPC protocol for SSR *)
module Protocol = Protocol

(** Worker interface and utilities *)
module Worker = Worker

(** Node.js subprocess worker *)
module Node_worker = Node_worker

(** SSR engine with worker pool *)
module Ssr = Ssr

(** React 18 streaming SSR *)
module Streaming = Streaming

(** {1 Convenience Functions} *)

(** Create routes for static React serving *)
let static_routes ?config () =
  Vite.static_routes ?config ()

(** Generate hydration shell HTML *)
let hydrate
    ~title
    ?(meta = [])
    ?(initial_data = None)
    ~manifest
    ~entry
    () =
  Hydrate.shell ~title ~meta ~initial_data ~manifest ~entry ()

(** Generate Kirin response with hydration *)
let hydrate_response
    ~title
    ?(meta = [])
    ?(initial_data = None)
    ~manifest
    ~entry
    () =
  let html = hydrate ~title ~meta ~initial_data ~manifest ~entry () in
  Kirin.Response.html html

(** Create SSR engine *)
let create_ssr ?(config = Ssr.default_config) () =
  Ssr.create config

(** Render URL with SSR *)
let ssr ~engine ~url ?props () =
  Ssr.render engine ~url ?props ()

(** Create SSR handler for Kirin router *)
let ssr_handler engine =
  Ssr.handler engine

(** Create SSR handler with fallback *)
let ssr_handler_with_fallback ~fallback engine =
  Ssr.handler_with_fallback ~fallback engine

(** {1 Quick Start Examples}

    {2 Level 1: Static React App}
    {[
      let () =
        match Manifest.load "dist/.vite/manifest.json" with
        | Ok manifest ->
          let routes = static_routes () @ [
            Kirin.get "/api/users" api_users;
          ] in
          Kirin.run ~port:3000 routes
        | Error msg ->
          failwith ("Manifest error: " ^ msg)
    ]}

    {2 Level 2: Hydration with Initial Data}
    {[
      let handler req =
        let user = fetch_user () in
        let manifest = ... in
        hydrate_response
          ~title:"User Profile"
          ~meta:[("og:title", "User Profile")]
          ~initial_data:(Some (`Assoc [("user", user_to_json user)]))
          ~manifest
          ~entry:"index.html"
          ()
    ]}

    {2 Level 3: Full SSR}
    {[
      let () =
        let engine = create_ssr ~config:{
          Ssr.default_config with
          bundle = "dist/server/entry-server.js";
          workers = 4;
        } () in
        let routes = [
          Kirin.get "/api/*" api_handler;
          Kirin.get "/*" (ssr_handler engine);
        ] in
        Kirin.run ~port:3000 routes
    ]}
*)
