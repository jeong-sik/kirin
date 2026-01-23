(** Kirin Solid.js Integration

    Server-side rendering for Solid.js applications. *)

(** {1 Core Modules} *)

module Meta = Meta
module Data = Data
module Hydrate = Hydrate
module Protocol = Protocol
module Worker = Worker
module Ssr = Ssr
module Streaming = Streaming
module Router = Router
module Handler = Handler

(** {1 Convenience Types} *)

type ssr_engine = Ssr.t
type ssr_config = Ssr.config
type render_response = Protocol.render_response
type meta_builder = Meta.builder
type route = Router.route

(** {1 Quick Setup} *)

(** Create SSR configuration *)
let config = Ssr.default_config

(** Create SSR engine *)
let create_engine config =
  let engine = Ssr.create config in
  Ssr.start engine;
  engine

(** Shutdown SSR engine *)
let shutdown = Ssr.shutdown

(** {1 Rendering} *)

(** Render URL to HTML *)
let render = Ssr.render

(** Render with caching *)
let render_cached = Ssr.render_cached

(** Render with fallback *)
let render_with_fallback = Ssr.render_with_fallback

(** {1 Hydration} *)

(** Create SPA shell (no SSR) *)
let spa_shell = Hydrate.spa

(** Create SSR shell with content *)
let ssr_shell = Hydrate.with_ssr

(** Create shell with Vite manifest *)
let vite_shell = Hydrate.with_vite

(** {1 Meta Tags} *)

(** Create meta builder *)
let meta = Meta.create

(** Build meta tags string *)
let build_meta = Meta.build

(** Quick page meta *)
let page_meta = Meta.page

(** {1 Initial Data} *)

(** Create initial data script tag *)
let data_script = Data.script_tag

(** Create route data script tag *)
let route_data_script = Data.route_data_tag

(** Create store data script tag *)
let store_script = Data.store_data_tag

(** {1 Router} *)

(** Create route *)
let route = Router.route

(** Create index route *)
let index_route = Router.index

(** Create catch-all route *)
let catch_all_route = Router.catch_all

(** Find matching route *)
let match_route = Router.find_route

(** Discover routes from directory *)
let discover_routes = Router.discover_routes

(** {1 Streaming} *)

(** Streaming SSE response *)
let stream_sse = Streaming.sse_stream

(** Progressive hydration marker *)
let progressive_marker = Streaming.progressive_marker

(** Progressive hydration script *)
let progressive_script = Streaming.progressive_script

(** {1 Handler Setup} *)

(** Default handler options *)
let default_options = Handler.default_options

(** Create Kirin routes *)
let routes = Handler.routes

(** Create data routes *)
let data_routes = Handler.data_routes

(** {1 Statistics} *)

(** Get SSR engine statistics *)
let stats = Ssr.stats

(** Clear render cache *)
let clear_cache = Ssr.clear_cache

(** {1 Health} *)

(** Check engine health *)
let health_check = Ssr.health_check

(** Recover unhealthy workers *)
let recover = Ssr.recover_unhealthy
