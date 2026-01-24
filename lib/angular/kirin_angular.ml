(** Kirin Angular/Angular Universal Integration

    Top-level API for Angular SSR support in Kirin. *)

(** {1 Re-exports} *)

(** Route definitions *)
module Route_def = Route_def

(** File-based routing *)
module File_router = File_router

(** Transfer state *)
module Transfer_state = Transfer_state

(** Hydration *)
module Hydration = Hydration

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
let dev_config ?(port=4200) () =
  { Handler.default_config with
    mode = Handler.CSR;
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

(** Create hybrid configuration (SSR + CSR by route) *)
let hybrid_config ~dist_path ~server_routes () =
  let ssr = Ssr.create Ssr.default_config in
  { Handler.default_config with
    mode = Handler.Hybrid;
    dist_path;
    ssr_engine = Some ssr;
    server_routes;
  }

(** Create prerender configuration *)
let prerender_config ~dist_path () =
  { Handler.default_config with
    mode = Handler.Prerender;
    dist_path;
  }

(** {1 Route Building} *)

(** Create SSR route *)
let ssr_route path component =
  Route_def.ssr path component

(** Create CSR route *)
let csr_route path component =
  Route_def.csr path component

(** Create SSG route *)
let ssg_route path component =
  Route_def.ssg path component

(** Create redirect route *)
let redirect ~from ~to_ () =
  Route_def.redirect ~from ~to_ ()

(** Create lazy-loaded route *)
let lazy_route path load_children =
  Route_def.lazy_route path load_children

(** {1 Transfer State} *)

(** Create transfer state *)
let create_transfer_state () =
  Transfer_state.create ()

(** Cache HTTP response in transfer state *)
let cache_response state ~method_ ~url response =
  Transfer_state.cache_response state ~method_ ~url response

(** Get cached response *)
let get_cached state ~method_ ~url () =
  Transfer_state.get_cached_response state ~method_ ~url ()

(** Serialize transfer state for HTML *)
let transfer_state_script state =
  Transfer_state.script_tag state

(** {1 Hydration} *)

(** Create hydration context *)
let create_hydration_context ?(mode=Hydration.Full) () =
  Hydration.create_context ~options:{ Hydration.default_options with mode } ()

(** Wrap with hydration boundary *)
let hydration_boundary ~id ?(trigger=Hydration.OnIdle) content =
  Hydration.with_boundary ~id ~trigger content

(** Skip hydration for content *)
let skip_hydration tag content =
  Hydration.with_skip_hydration tag content

(** {1 Meta Tags} *)

(** Create SEO head *)
let seo ~title ?description ?canonical ?og_image () =
  Meta.seo ~title ?description ?canonical ?og_image ()

(** Create head with title *)
let head ~title () =
  Meta.empty |> Meta.with_title title |> Meta.with_charset "UTF-8"

(** Render head to HTML *)
let render_head head =
  Meta.render head

(** {1 Handler Building} *)

(** Create catch-all Angular handler *)
let angular_handler config =
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

(** {1 SSR Engine} *)

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

(** {1 Prerendering} *)

(** Prerender routes to static files *)
let prerender engine ~routes ~output_dir =
  Ssr.prerender engine ~routes ~output_dir

(** {1 Codegen} *)

(** Generate TypeScript types for routes *)
let generate_types ~routes =
  Codegen.generate_types_file ~routes

(** Generate server routes config *)
let generate_server_routes ~routes =
  Codegen.generate_server_routes routes

(** {1 File Router} *)

(** Discover routes from app directory *)
let discover_routes ~app_dir () =
  File_router.discover_routes app_dir
