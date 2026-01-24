(** Kirin Astro Integration

    Top-level API for Astro Islands architecture with partial hydration.

    Astro is unique among frameworks - it ships zero JavaScript by default,
    with "islands" of interactivity that hydrate independently:

    - **Islands Architecture**: Interactive components ("islands") in a sea of static HTML
    - **Partial Hydration**: Only hydrate components that need interactivity
    - **Client Directives**: `client:load`, `client:idle`, `client:visible`, `client:media`
    - **Multi-framework**: Use React, Vue, Svelte, etc. in the same project
    - **Content Collections**: Type-safe content management for Markdown/MDX *)

(** {1 Re-exports} *)

(** Islands *)
module Island = Island

(** Route definitions *)
module Route_def = Route_def

(** File-based routing *)
module File_router = File_router

(** Content collections *)
module Content = Content

(** Meta tags *)
module Meta = Meta

(** Framework integrations *)
module Integration = Integration

(** View transitions *)
module View_transitions = View_transitions

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
let dev_config ?(port=4321) () =
  { Handler.default_config with
    dev_mode = true;
    dev_port = port;
    output = Handler.Server;
  }

(** Create static configuration *)
let static_config ~dist_path () =
  { Handler.default_config with
    output = Handler.Static;
    dist_path;
  }

(** Create server configuration *)
let server_config ~dist_path ?(workers=4) ?(timeout_s=5.0) () =
  let ssr = Ssr.create {
    Ssr.default_config with
    workers;
    timeout_s;
  } in
  { Handler.default_config with
    output = Handler.Server;
    dist_path;
    ssr_engine = Some ssr;
  }

(** Create hybrid configuration *)
let hybrid_config ~dist_path ?(workers=4) () =
  let ssr = Ssr.create {
    Ssr.default_config with
    workers;
  } in
  { Handler.default_config with
    output = Handler.Hybrid;
    dist_path;
    ssr_engine = Some ssr;
  }

(** {1 Island Building} *)

(** Create React island *)
let react_island ~component ?(directive=Island.ClientLoad) ?props () =
  Island.create ~component ~framework:Island.React ~directive ?props ()

(** Create Vue island *)
let vue_island ~component ?(directive=Island.ClientLoad) ?props () =
  Island.create ~component ~framework:Island.Vue ~directive ?props ()

(** Create Svelte island *)
let svelte_island ~component ?(directive=Island.ClientLoad) ?props () =
  Island.create ~component ~framework:Island.Svelte ~directive ?props ()

(** Create Solid island *)
let solid_island ~component ?(directive=Island.ClientLoad) ?props () =
  Island.create ~component ~framework:Island.Solid ~directive ?props ()

(** Create Preact island *)
let preact_island ~component ?(directive=Island.ClientLoad) ?props () =
  Island.create ~component ~framework:Island.Preact ~directive ?props ()

(** {1 Client Directives} *)

(** Load island immediately *)
let client_load = Island.ClientLoad

(** Load island when browser is idle *)
let client_idle = Island.ClientIdle

(** Load island when visible in viewport *)
let client_visible = Island.ClientVisible

(** Load island when media query matches *)
let client_media query = Island.ClientMedia query

(** Client-only (skip SSR) *)
let client_only framework = Island.ClientOnly framework

(** {1 Route Building} *)

(** Create static route *)
let static_route path =
  Route_def.static path

(** Create SSR route *)
let ssr_route path =
  Route_def.ssr path

(** Create hybrid route *)
let hybrid_route path =
  Route_def.hybrid path

(** {1 Content Collection Helpers} *)

(** Define content schema *)
let define_schema name fields =
  Content.define_schema name fields

(** Define content collection *)
let define_collection ~name ~schema =
  Content.define_collection ~name ~schema ()

(** String field *)
let string_field = Content.string_

(** Number field *)
let number_field = Content.number

(** Boolean field *)
let bool_field = Content.bool_

(** Date field *)
let date_field = Content.date

(** Enum field *)
let enum_field = Content.enum

(** Reference field *)
let reference_field = Content.reference

(** Image field *)
let image_field = Content.image

(** {1 Meta Helpers} *)

(** Create SEO head *)
let seo ~title ?description ?og_image ?canonical () =
  Meta.seo ~title ?description ?og_image_url:og_image ?canonical_url:canonical ()

(** Create head with title *)
let head ~title () =
  Meta.empty |> Meta.with_title title

(** Render head to HTML *)
let render_head head =
  Meta.render head

(** {1 Integration Helpers} *)

(** Get React integration *)
let react_integration = Integration.react

(** Get Vue integration *)
let vue_integration = Integration.vue

(** Get Svelte integration *)
let svelte_integration = Integration.svelte

(** Get Solid integration *)
let solid_integration = Integration.solid

(** Get Preact integration *)
let preact_integration = Integration.preact

(** Get Lit integration *)
let lit_integration = Integration.lit

(** Get Alpine integration *)
let alpine_integration = Integration.alpine

(** {1 View Transitions Helpers} *)

(** Enable view transitions *)
let enable_view_transitions config =
  View_transitions.enable config

(** Create transition element *)
let transition_element = View_transitions.element

(** Persist element across navigations *)
let persist_element = View_transitions.persist

(** {1 Handler Helpers} *)

(** Create catch-all Astro handler *)
let astro_handler config =
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
let render_ssr engine ~url ?props ?islands () =
  Ssr.render engine ~url ?props ?islands ()

(** Get SSR stats *)
let ssr_stats engine =
  Ssr.get_stats engine

(** Shutdown SSR engine *)
let shutdown_ssr engine =
  Ssr.shutdown engine

(** {1 Codegen Helpers} *)

(** Generate TypeScript types *)
let generate_types ~routes ~collections =
  Codegen.generate_types_file ~routes ~collections

(** Generate routes file *)
let generate_routes ~routes =
  Codegen.generate_routes_file routes

(** Generate Astro config *)
let generate_config ~integrations ~output =
  Codegen.generate_astro_config ~integrations ~output

(** {1 File Router Helpers} *)

(** Discover routes from pages directory *)
let discover_routes ~pages_dir () =
  File_router.discover_routes pages_dir
