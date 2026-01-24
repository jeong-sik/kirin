(** Kirin Preact Integration

    Preact support for Kirin web framework.
    Provides full SSR with Signals support and React compatibility.

    Features:
    - Preact Signals for fine-grained reactivity
    - preact/compat for React library compatibility
    - Vite integration with @preact/preset-vite
    - Server-side rendering with signals hydration

    @see <https://github.com/jeong-sik/kirin>
*)

(** {1 Core Modules} *)

(** Preact Signals - fine-grained reactivity *)
module Signals = Signals

(** React compatibility layer *)
module Compat = Compat

(** Vite manifest parsing *)
module Manifest = Manifest

(** Hydration and HTML shell generation *)
module Hydrate = Hydrate

(** JSON-RPC protocol for SSR *)
module Protocol = Protocol

(** SSR engine with worker pool *)
module Ssr = Ssr

(** Vite dev server integration *)
module Vite = Vite

(** {1 Convenience Functions} *)

(** Create static routes for Preact serving *)
let static_routes ?config () =
  Vite.static_routes ?config ()

(** Generate hydration shell HTML *)
let hydrate
    ~title
    ?(meta = [])
    ?(initial_data = None)
    ?(signals_data = [])
    ~manifest
    ~entry
    () =
  Hydrate.shell ~title ~meta ~initial_data ~signals_data ~manifest ~entry ()

(** Generate Kirin response with hydration *)
let hydrate_response
    ~title
    ?(meta = [])
    ?(initial_data = None)
    ?(signals_data = [])
    ~manifest
    ~entry
    () =
  let html = hydrate ~title ~meta ~initial_data ~signals_data ~manifest ~entry () in
  Kirin.Response.html html

(** Create SSR engine *)
let create_ssr ?(config = Ssr.default_config) () =
  Ssr.create config

(** Render URL with SSR *)
let ssr ~engine ~url ?props () =
  Ssr.render engine ~url ?props ()

(** Render with signals *)
let ssr_with_signals ~engine ~url ~signals () =
  Ssr.render_with_signals engine ~url ~signals ()

(** Create SSR handler for Kirin router *)
let ssr_handler engine =
  Ssr.handler engine

(** Create SSR handler with fallback *)
let ssr_handler_with_fallback ~fallback engine =
  Ssr.handler_with_fallback ~fallback engine

(** {1 Signals Helpers} *)

(** Create a signal *)
let signal name initial_value =
  Signals.signal name initial_value

(** Create a computed signal *)
let computed ~name ~dependencies expression =
  Signals.computed ~name ~dependencies expression

(** Generate signal import statement *)
let signals_import imports =
  Signals.generate_import imports

(** {1 Compat Helpers} *)

(** Get all Vite aliases for React compat *)
let compat_aliases () =
  Compat.vite_aliases ()

(** Check library compatibility *)
let check_library_compat lib =
  Compat.find_compat lib

(** {1 Quick Start Examples}

    {2 Basic Preact App with Signals}
    {[
      let count = signal "count" (`Int 0) in
      let double = computed ~name:"double"
        ~dependencies:["count"]
        "count.value * 2" in
      ...
    ]}

    {2 Hydration with Signals Data}
    {[
      let handler _req =
        let manifest = ... in
        hydrate_response
          ~title:"Preact App"
          ~signals_data:[("count", `Int 0)]
          ~manifest
          ~entry:"index.html"
          ()
    ]}

    {2 Full SSR with Signals}
    {[
      let engine = create_ssr () in
      let signals = `Assoc [("count", `Int 5)] in
      let handler req =
        let url = Kirin.Request.path req in
        match ssr_with_signals ~engine ~url ~signals () with
        | Ok html -> Kirin.Response.html html
        | Error _ -> Kirin.Response.html "<h1>Error</h1>"
    ]}

    {2 React Compat Mode}
    {[
      (* In vite.config.ts, add aliases *)
      let aliases = compat_aliases () in
      (* Check if library is compatible *)
      match check_library_compat "react-query" with
      | Some c -> Printf.printf "Compat level: %s" (Compat.level_to_string c.level)
      | None -> Printf.printf "Unknown library"
    ]}
*)
