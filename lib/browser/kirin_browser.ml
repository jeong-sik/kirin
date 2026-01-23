(** Kirin Browser - Client-Side Framework

    Browser-compatible subset of Kirin for building SPAs.
    Compiles to JavaScript via js_of_ocaml.
    Uses OCaml 5 effects for direct-style async.

    {b Features:}
    - Direct-style async with Promise effects (no Lwt!)
    - Client-side routing with History API
    - Fetch API for HTTP requests
    - Query string parsing
    - Hash-based routing fallback

    {b Quick Start:}
    {[
      open Kirin_browser

      (* Define routes *)
      let routes = [
        Router.route "/" (fun _ -> render_home ());
        Router.route "/users/:id" (fun params ->
          let id = List.assoc "id" params in
          render_user id);
      ]

      (* Create and start router *)
      let router = Router.create routes
      let () = Router.start router

      (* Make API calls (direct-style!) *)
      let load_users () =
        Promise.run (fun () ->
          match Fetch.get_json "/api/users" with
          | Ok json -> update_users json
          | Error (status, _) -> show_error status
        )
    ]}
*)

(** Promise effect system for direct-style async *)
module Promise = Promise

(** HTTP client for browser (Fetch API) *)
module Fetch = Fetch

(** History API bindings *)
module History = History

(** Client-side SPA router *)
module Router = Router

(** {1 Quick Access - Promise} *)

(** Await a promise (performs effect) *)
let await = Promise.await

(** Run direct-style async computation *)
let run = Promise.run

(** Sleep for milliseconds *)
let sleep = Promise.sleep

(** {1 Quick Access - Navigation} *)

(** Navigate to a URL *)
let navigate = History.push_state

(** Get current pathname *)
let pathname = History.Location.pathname

(** Get current query params *)
let query_params = History.Location.query_params

(** {1 Quick Access - HTTP} *)

(** Make a GET request *)
let get = Fetch.get

(** Make a POST request *)
let post = Fetch.post

(** GET JSON *)
let get_json = Fetch.get_json

(** POST JSON *)
let post_json = Fetch.post_json
