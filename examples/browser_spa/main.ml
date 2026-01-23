(** Kirin Browser SPA Example

    A simple single-page application demonstrating:
    - Callback-style async with timers
    - Client-side routing with History API
    - HTTP requests with Fetch API

    Build: dune build examples/browser_spa/main.js
    Run: Open index.html in a browser with a local server
*)

open Js_of_ocaml
open Kirin_browser

(** {1 DOM Helpers} *)

let get_element id =
  Dom_html.getElementById id

let set_html el html =
  el##.innerHTML := Js.string html

(** {1 Views} *)

let render_home () =
  let app = get_element "app" in
  set_html app {|
    <h1>Welcome to Kirin Browser!</h1>
    <p>This is a single-page application built with OCaml.</p>
    <nav>
      <a href="/users" onclick="return false;" class="nav-link">Users</a> |
      <a href="/about" onclick="return false;" class="nav-link">About</a>
    </nav>
  |}

let render_users () =
  let app = get_element "app" in
  set_html app {|
    <h1>Users</h1>
    <p>Loading users...</p>
    <nav>
      <a href="/" onclick="return false;" class="nav-link">Home</a>
    </nav>
  |};
  (* Simulate API call with timeout - in real app would use Fetch *)
  let _cancel = set_timeout 500 (fun () ->
    set_html app {|
      <h1>Users</h1>
      <ul>
        <li>Alice</li>
        <li>Bob</li>
        <li>Charlie</li>
      </ul>
      <nav>
        <a href="/" onclick="return false;" class="nav-link">Home</a>
      </nav>
    |}
  ) in
  ()

let render_user params =
  let app = get_element "app" in
  let id = List.assoc_opt "id" params |> Option.value ~default:"unknown" in
  set_html app (Printf.sprintf {|
    <h1>User %s</h1>
    <p>User profile page for ID: %s</p>
    <nav>
      <a href="/users" onclick="return false;" class="nav-link">Back to Users</a>
    </nav>
  |} id id)

let render_about () =
  let app = get_element "app" in
  set_html app {|
    <h1>About</h1>
    <p>Kirin Browser is a client-side framework for OCaml.</p>
    <h2>Features:</h2>
    <ul>
      <li>Direct-style async (no monads!)</li>
      <li>Client-side routing with History API</li>
      <li>Fetch API for HTTP requests</li>
      <li>Compiles to JavaScript via js_of_ocaml</li>
    </ul>
    <nav>
      <a href="/" onclick="return false;" class="nav-link">Home</a>
    </nav>
  |}

let render_not_found () =
  let app = get_element "app" in
  set_html app {|
    <h1>404 Not Found</h1>
    <p>The page you are looking for does not exist.</p>
    <nav>
      <a href="/" onclick="return false;" class="nav-link">Go Home</a>
    </nav>
  |}

(** {1 Router Setup} *)

let router = Router.create
  ~not_found:(fun _ -> render_not_found ())
  [
    Router.route "/" (fun _ -> render_home ());
    Router.route "/users" (fun _ -> render_users ());
    Router.route "/users/:id" (fun params -> render_user params);
    Router.route "/about" (fun _ -> render_about ());
  ]

(** {1 Navigation Link Handler} *)

let setup_navigation () =
  (* Intercept clicks on navigation links *)
  let handler = Dom_html.handler (fun event ->
    let target = Dom_html.eventTarget event in
    let tag_name = Js.to_string target##.tagName in
    if tag_name = "A" then begin
      let href = Js.to_string (Js.Unsafe.get target (Js.string "href")) in
      (* Extract pathname from href using URL API *)
      let url_ctor = Js.Unsafe.global##._URL in
      let url = Js.Unsafe.new_obj url_ctor [| Js.Unsafe.inject (Js.string href) |] in
      let pathname = Js.to_string (Js.Unsafe.get url (Js.string "pathname")) in
      Dom.preventDefault event;
      Router.navigate router pathname
    end;
    Js._true
  ) in
  Dom_html.addEventListener
    Dom_html.document
    Dom_html.Event.click
    handler
    Js._false
  |> ignore

(** {1 Entry Point} *)

let () =
  (* Wait for DOM to be ready *)
  Dom_html.window##.onload := Dom_html.handler (fun _ ->
    setup_navigation ();
    Router.start router;
    Js._true
  )
