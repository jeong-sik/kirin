(** Kirin Vue/Nuxt Integration

    Top-level API for Vue/Nuxt SSR support in Kirin. *)

(** {1 Re-exports} *)

(** Route definitions *)
module Route_def = Route_def

(** File-based routing *)
module File_router = File_router

(** Data loading *)
module Loader = Loader

(** Server actions *)
module Action = Action

(** Route manifest *)
module Manifest = Manifest

(** Preloading *)
module Preload = Preload

(** Meta tags *)
module Meta = Meta

(** Initial data *)
module Data = Data

(** HTML shell *)
module Hydrate = Hydrate

(** SSR protocol *)
module Protocol = Protocol

(** Worker interface *)
module Worker = Worker

(** SSR engine *)
module Ssr = Ssr

(** Streaming SSR *)
module Streaming = Streaming

(** Kirin handlers *)
module Handler = Handler

(** TypeScript codegen *)
module Codegen = Codegen

(** {1 Quick Start Helpers} *)

(** Create development configuration *)
let dev_config ?(port=3000) () =
  { Handler.default_config with
    dev_mode = true;
    dev_port = port;
    mode = Handler.SPA;
  }

(** Create production SPA configuration *)
let spa_config ~manifest_path ~entry_point () =
  { Handler.default_config with
    mode = Handler.SPA;
    manifest_path;
    entry_point;
  }

(** Create hydration configuration *)
let hydration_config ~manifest_path ~entry_point () =
  { Handler.default_config with
    mode = Handler.Hydration;
    manifest_path;
    entry_point;
  }

(** Create full SSR configuration *)
let ssr_config ~manifest_path ~entry_point ?(workers=4) ?(timeout_s=5.0) () =
  let ssr = { Ssr.default_config with
    workers;
    timeout_s;
  } in
  { Handler.default_config with
    mode = Handler.SSR;
    manifest_path;
    entry_point;
    ssr_config = Some ssr;
  }

(** Create streaming SSR configuration *)
let streaming_config ~manifest_path ~entry_point () =
  { Handler.default_config with
    mode = Handler.Streaming;
    manifest_path;
    entry_point;
    streaming_config = Some Streaming.default_config;
  }

(** {1 Route Building} *)

(** Create a page route *)
let page ~path ?layout ?middleware () =
  let route = Route_def.page path in
  let route = match layout with Some l -> Route_def.with_layout l route | None -> route in
  let route = match middleware with
    | Some mws -> List.fold_left (fun r mw -> Route_def.with_middleware mw r) route mws
    | None -> route
  in route

(** Create a dynamic route with parameter *)
let dynamic ~path ~param_name ?(param_type=Route_def.String) () =
  Route_def.page path
  |> Route_def.with_param param_name param_type

(** Create a catch-all route *)
let catch_all ~path () =
  Route_def.page path
  |> Route_def.with_param "slug" Route_def.Slug

(** {1 Data Loading} *)

(** Create fetch loader options *)
let fetch ~key ?(method_="GET") ?headers ?query () =
  { Loader.default_fetch_options with
    key = Some key;
    method_;
    headers = Option.value ~default:[] headers;
    query = Option.value ~default:[] query;
  }

(** Create async data loader options *)
let async_data ~key ?(server=true) ?(lazy_=false) () =
  { (Loader.default_async_options key) with
    server;
    lazy_;
  }

(** {1 Meta Tags} *)

(** Create SEO meta configuration *)
let seo ~title ?description ?image ?url () =
  Meta.seo ~title ?description ?image ?url ()

(** Create full head configuration *)
let head ~title ?description ?og_title ?og_image ?canonical () =
  Meta.empty
  |> Meta.with_title title
  |> (fun h -> match description with
      | Some d -> Meta.with_meta ~name:"description" ~content:d h
      | None -> h)
  |> (fun h -> match og_title with
      | Some t -> Meta.with_property ~property:"og:title" ~content:t h
      | None -> h)
  |> (fun h -> match og_image with
      | Some img -> Meta.with_property ~property:"og:image" ~content:img h
      | None -> h)
  |> (fun h -> match canonical with
      | Some url -> Meta.with_link ~rel:"canonical" ~href:url h
      | None -> h)

(** {1 Payload Building} *)

(** Create payload with route path *)
let payload ~route_path () =
  Data.empty_payload ~route_path

(** Add data to payload *)
let with_data key value payload =
  Data.with_data key value payload

(** Add state to payload *)
let with_state key value payload =
  Data.with_state key value payload

(** {1 Handler Building} *)

(** Create catch-all Vue handler *)
let vue_handler config =
  Handler.catch_all_handler config

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

(** {1 Development Helpers} *)

(** Check if in development mode *)
let is_dev () =
  match Sys.getenv_opt "NODE_ENV" with
  | Some "production" -> false
  | _ -> true

(** Get Vite dev server URL *)
let vite_dev_url ?(port=3000) path =
  Printf.sprintf "http://localhost:%d%s" port path

(** {1 Codegen Helpers} *)

(** Generate TypeScript types for routes *)
let generate_types ~routes ?api_routes () =
  Codegen.generate_types_file
    ~routes
    ~api_routes:(Option.value ~default:[] api_routes)

(** Generate single route type *)
let generate_route_type route =
  Codegen.generate_route_type route

(** {1 Preloading} *)

(** Create modulepreload hint *)
let modulepreload href =
  Preload.modulepreload href

(** Create prefetch hint *)
let prefetch href =
  Preload.prefetch href

(** Render hints to HTML *)
let render_hints hints =
  Preload.render_hints hints

(** {1 Error Pages} *)

(** Generate 404 page *)
let not_found_page () =
  Hydrate.error_page ~status:404 ~message:"Page not found"

(** Generate 500 page *)
let server_error_page ?(message="Internal server error") () =
  Hydrate.error_page ~status:500 ~message

(** {1 Island Architecture} *)

(** Create island hydration data *)
let island ~id ~component ?props ?priority () =
  Data.island ~id ~component
    ?props
    ?priority
    ()

(** Generate island wrapper HTML *)
let island_wrapper ~id ~component ~priority ~html =
  Hydrate.island_wrapper ~id ~component ~priority ~html

(** {1 Streaming} *)

(** Create streaming context *)
let streaming_context ?config () =
  Streaming.create_context ?config ()

(** Start streaming response *)
let start_streaming ctx =
  Streaming.start ctx

(** Send shell chunk *)
let send_shell ctx ~html =
  let chunk = Streaming.shell_chunk ~html in
  Streaming.send_chunk ctx chunk

(** Send suspense chunk *)
let send_suspense ctx ~id ~html =
  let chunk = Streaming.suspense_chunk ~id ~html in
  Streaming.send_chunk ctx chunk

(** Finish streaming *)
let finish_streaming ctx =
  Streaming.finish ctx

(** {1 File Router} *)

(** Discover routes from pages directory *)
let discover_routes ~pages_dir () =
  File_router.discover_routes pages_dir

(** Create manifest from discovered routes *)
let manifest_from_routes routes =
  Manifest.from_discovered_routes routes
