(** Kirin TanStack Router Integration

    Type-safe routing with TanStack Router patterns. *)

(** {1 Core Modules} *)

module Route_def = Route_def
module File_router = File_router
module Loader = Loader
module Action = Action
module Manifest = Manifest
module Preload = Preload
module Handler = Handler
module Codegen = Codegen

(** {1 Convenience Types} *)

type route_entry = Manifest.route_entry
type match_result = Manifest.match_result
type manifest = Manifest.t

(** {1 Route Building} *)

(** Create a route definition *)
let route ~id ~path ?params ?meta ?loader ?action ?children () =
  Route_def.create ~id ~path ?params ?meta ?loader ?action ?children ()

(** {1 File Router} *)

(** Discover routes from directory *)
let discover_routes = File_router.discover

(** Generate manifest from discovered routes *)
let manifest_from_routes = Manifest.from_discovered_routes

(** {1 Loader Helpers} *)

(** Create successful loader result *)
let loader_ok = Loader.ok

(** Create loader redirect *)
let loader_redirect ?(status=302) url = Loader.redirect ~status url

(** Create loader not found *)
let loader_not_found () = Loader.not_found ()

(** Create loader unauthorized *)
let loader_unauthorized () = Loader.unauthorized ()

(** {1 Action Helpers} *)

(** Create successful action result *)
let action_success = Action.success

(** Create action error *)
let action_error = Action.server_error

(** Create action redirect *)
let action_redirect ?(status=302) url = Action.redirect ~status url

(** Create action with validation error *)
let validation_error = Action.validation_error

(** {1 Preload Hints} *)

(** Preload strategy *)
type preload_strategy = Preload.strategy =
  | Intent
  | Viewport
  | Render
  | None

(** Generate preload hint *)
let preload_hint = Preload.link_hint

(** Generate module preload *)
let module_preload = Preload.module_preload

(** Generate route hints *)
let route_hints = Preload.route_hints

(** {1 Handler Setup} *)

(** Create handler configuration *)
let config = Handler.default_config

(** Create route registry *)
let create_registry = Handler.create_registry

(** Register route handler *)
let register = Handler.register_handler

(** Create routes *)
let routes = Handler.routes

(** Create manifest route *)
let manifest_route = Handler.manifest_route

(** {1 Code Generation} *)

(** Generate TypeScript types file *)
let generate_types = Codegen.generate_types_file

(** Generate router file *)
let generate_router = Codegen.generate_router_file

(** Generate type-safe hooks *)
let generate_hooks () =
  String.concat "\n\n" [
    Codegen.generate_use_params_hook ();
    Codegen.generate_use_loader_data_hook ();
  ]

(** {1 Full Setup} *)

(** Setup TanStack router with all routes *)
let setup ~routes_dir ~ctx_factory =
  let discovered = discover_routes routes_dir in
  let manifest = manifest_from_routes discovered in
  let registry = create_registry () in
  let config = Handler.default_config manifest in
  (manifest, registry, config, ctx_factory)

(** Get all routes for Kirin app *)
let all_routes ~manifest ~registry ~ctx_factory =
  let config = Handler.default_config manifest in
  Handler.manifest_route manifest ::
  Handler.routes ~config ~ctx_factory ~registry
