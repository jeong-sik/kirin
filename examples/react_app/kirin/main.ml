(** React Integration Example

    Demonstrates all three levels of React integration:
    - Level 1: Static Vite build serving
    - Level 2: Hydration with initial data
    - Level 3: Full SSR with Node.js workers

    Run with: dune exec examples/react_app/kirin/main.exe
*)

(** Sample user data *)
type user = {
  id: int;
  name: string;
  email: string;
}

let users = [
  { id = 1; name = "Alice"; email = "alice@example.com" };
  { id = 2; name = "Bob"; email = "bob@example.com" };
  { id = 3; name = "Charlie"; email = "charlie@example.com" };
]

let user_to_json u =
  `Assoc [
    ("id", `Int u.id);
    ("name", `String u.name);
    ("email", `String u.email);
  ]

(** API Routes *)
let api_users _req =
  Kirin.json (`List (List.map user_to_json users))

let api_user req =
  let id_str = Kirin.param "id" req in
  let id = int_of_string_opt id_str |> Option.value ~default:0 in
  match List.find_opt (fun u -> u.id = id) users with
  | Some user -> Kirin.json (user_to_json user)
  | None -> Kirin.json ~status:`Not_found (`Assoc [("error", `String "User not found")])

(** Level 1: Static serving (SPA mode) *)
let level1_routes () =
  Printf.printf "Level 1: Static mode - serving Vite build from dist/\n%!";
  Kirin_react.static_routes ()

(** Level 2: Hydration with initial data *)
let level2_handler manifest req =
  let path = Kirin.Request.path req in
  let user = List.hd users in (* Example: fetch current user *)
  let initial_data = Some (`Assoc [
    ("currentUser", user_to_json user);
    ("path", `String path);
  ]) in
  Kirin_react.hydrate_response
    ~title:"Kirin + React"
    ~meta:[
      ("description", "Kirin React Integration Demo");
      ("og:title", "Kirin + React");
      ("og:type", "website");
    ]
    ~initial_data
    ~manifest
    ~entry:"index.html"
    ()

(** Level 3: Full SSR with Node.js *)
let level3_handler engine req =
  let url = Kirin.Request.path req in
  let user = List.hd users in
  let props = `Assoc [
    ("currentUser", user_to_json user);
    ("serverTime", `String (string_of_float (Unix.gettimeofday ())));
  ] in
  match Kirin_react.ssr ~engine ~url ~props () with
  | Ok html -> Kirin.html html
  | Error msg -> Kirin.text ~status:`Internal_server_error ("SSR Error: " ^ msg)

(** Main entry point *)
let () =
  (* Determine mode from environment *)
  let mode = Sys.getenv_opt "REACT_MODE" |> Option.value ~default:"static" in
  Printf.printf "Starting Kirin + React server (mode: %s)\n%!" mode;

  let handler = match mode with
  | "static" | "level1" ->
    (* Level 1: Pure static serving *)
    Kirin.router (
      [
        Kirin.get "/api/users" api_users;
        Kirin.get "/api/users/:id" api_user;
      ] @ level1_routes ()
    )

  | "hydration" | "level2" ->
    (* Level 2: Hydration with SEO *)
    let manifest = match Kirin_react.Manifest.load "dist/.vite/manifest.json" with
      | Ok m -> m
      | Error msg -> failwith ("Manifest error: " ^ msg)
    in
    Kirin.router [
      Kirin.get "/api/users" api_users;
      Kirin.get "/api/users/:id" api_user;
      Kirin.get "/*" (level2_handler manifest);
    ]

  | "ssr" | "level3" ->
    (* Level 3: Full SSR *)
    let config = {
      Kirin_react.Ssr.default_config with
      bundle = "dist/server/entry-server.js";
      workers = 2;
      timeout = 5.0;
    } in
    let engine = Kirin_react.create_ssr ~config () in
    at_exit (fun () -> Kirin_react.Ssr.shutdown engine);
    Kirin.router [
      Kirin.get "/api/users" api_users;
      Kirin.get "/api/users/:id" api_user;
      Kirin.get "/*" (level3_handler engine);
    ]

  | _ ->
    failwith "Unknown mode. Use: static, hydration, or ssr"
  in

  Printf.printf "Server running at http://localhost:3000\n%!";
  Kirin.start ~port:3000
  @@ Kirin.logger
  @@ handler
