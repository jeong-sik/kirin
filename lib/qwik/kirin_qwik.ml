(** Kirin Qwik/QwikCity Integration

    Top-level API for Qwik SSR with resumability support in Kirin.

    Qwik is fundamentally different from other frameworks - it uses
    "resumability" instead of hydration. The key concepts are:

    - **Resumability**: Server-rendered HTML is immediately interactive
      without hydration (near-zero JavaScript on initial load)
    - **QRL**: Serialized references to functions that can be lazy loaded
    - **Signals**: Fine-grained reactive state primitives
    - **Component$**: Components are lazy by default ($ suffix convention) *)

(** {1 Re-exports} *)

(** QRL (Qwik URL) *)
module Qrl = Qrl

(** Signal reactivity *)
module Signal = Signal

(** Route definitions *)
module Route_def = Route_def

(** File-based routing *)
module File_router = File_router

(** Container state *)
module Container = Container

(** Data loaders *)
module Loader = Loader

(** Actions *)
module Action = Action

(** Meta tags *)
module Meta = Meta

(** SSR protocol *)
module Protocol = Protocol

(** SSR engine *)
module Ssr = Ssr

(** HTTP handler *)
module Handler = Handler

(** TypeScript codegen *)
module Codegen = Codegen

(** {1 Quick Start Helpers} *)

(** Create development configuration *)
let dev_config ?(port=5173) () =
  { Handler.default_config with
    mode = Handler.SPA;
    dist_path = Printf.sprintf "http://localhost:%d" port;
  }

(** Create SSR configuration *)
let ssr_config ~dist_path ?(workers=4) ?(timeout_s=5.0) () =
  let ssr = Ssr.create {
    Ssr.default_config with
    workers;
    timeout_s;
  } in
  { Handler.default_config with
    mode = Handler.SSR;
    dist_path;
    ssr_engine = Some ssr;
  }

(** Create static configuration *)
let static_config ~dist_path () =
  { Handler.default_config with
    mode = Handler.Static;
    dist_path;
  }

(** {1 Route Building} *)

(** Create SSR route *)
let ssr_route path =
  Route_def.ssr path

(** Create static route *)
let static_route path =
  Route_def.static path

(** Create SPA route *)
let spa_route path =
  Route_def.spa path

(** {1 QRL Helpers} *)

(** Create event handler QRL *)
let on_click ~chunk ~symbol =
  let qrl = Qrl.create ~chunk ~symbol () in
  Qrl.on_click qrl

(** Create input handler QRL *)
let on_input ~chunk ~symbol =
  let qrl = Qrl.create ~chunk ~symbol () in
  Qrl.on_input qrl

(** Create submit handler QRL *)
let on_submit ~chunk ~symbol =
  let qrl = Qrl.create ~chunk ~symbol () in
  Qrl.on_submit qrl

(** {1 Signal Helpers} *)

(** Create signal *)
let signal value =
  Signal.create value

(** Create computed signal *)
let computed fn =
  Signal.computed fn

(** Create resource signal *)
let resource track_fn =
  Signal.resource track_fn

(** {1 Container Helpers} *)

(** Create container *)
let create_container () =
  Container.create ()

(** Serialize container to script tag *)
let container_script container =
  Container.script_tag container

(** {1 Loader Helpers} *)

(** Create data loader *)
let loader ~name f =
  Loader.data ~name f

(** Execute loader *)
let run_loader loader ctx =
  Loader.execute loader ctx

(** {1 Action Helpers} *)

(** Create route action *)
let action ~name f =
  Action.route_action ~name f

(** Execute action *)
let run_action action ctx =
  Action.execute action ctx

(** {1 Meta Helpers} *)

(** Create SEO head *)
let seo ~title ?description ?canonical ?og_image () =
  Meta.seo ~title ?description ?canonical ?og_image ()

(** Create head with title *)
let head ~title () =
  Meta.empty |> Meta.with_title title

(** Render head to HTML *)
let render_head head =
  Meta.render head

(** {1 Handler Helpers} *)

(** Create catch-all Qwik handler *)
let qwik_handler config =
  fun request -> Handler.catch_all_handler config request

(** Handle single request *)
let handle ~config ~path ?query ?headers ?method_ ?body () =
  let request = Handler.request_info
    ~path
    ?query
    ?headers
    ?method_
    ?body
    ()
  in
  Handler.handle ~config ~request ()

(** {1 SSR Engine Helpers} *)

(** Create SSR engine *)
let create_ssr_engine ?(config=Ssr.default_config) () =
  Ssr.create config

(** Render with SSR *)
let render_ssr engine ~url () =
  Ssr.render engine ~url ()

(** Get SSR stats *)
let ssr_stats engine =
  Ssr.get_stats engine

(** Shutdown SSR engine *)
let shutdown_ssr engine =
  Ssr.shutdown engine

(** {1 Codegen Helpers} *)

(** Generate TypeScript types for routes *)
let generate_types ~routes =
  Codegen.generate_types_file ~routes

(** Generate routes file *)
let generate_routes ~routes =
  Codegen.generate_routes_file routes

(** {1 File Router Helpers} *)

(** Discover routes from directory *)
let discover_routes ~root_dir () =
  File_router.discover_routes root_dir
