(** Hello World example for Kirin *)

let home_handler _req =
  Kirin.html {|
    <html>
    <head><title>Kirin</title></head>
    <body>
      <h1>🦒 Hello, Kirin!</h1>
      <p>OCaml 5.x Eio-native web framework</p>
      <ul>
        <li><a href="/hello/world">Say hello to world</a></li>
        <li><a href="/json">JSON response</a></li>
        <li><a href="/users/42">User profile (path param)</a></li>
      </ul>
    </body>
    </html>
  |}

let hello_handler req =
  let name = Kirin.param "name" req in
  Kirin.html (Printf.sprintf {|
    <html>
    <head><title>Hello %s</title></head>
    <body>
      <h1>Hello, %s! 👋</h1>
      <a href="/">Back home</a>
    </body>
    </html>
  |} name name)

let json_handler _req =
  Kirin.json (`Assoc [
    ("framework", `String "Kirin");
    ("language", `String "OCaml 5.x");
    ("async", `String "Eio (direct-style)");
    ("status", `String "running");
  ])

let user_handler req =
  let id = Kirin.param "id" req in
  Kirin.json (`Assoc [
    ("id", `Int (int_of_string id));
    ("name", `String ("User " ^ id));
    ("email", `String (Printf.sprintf "user%s@example.com" id));
  ])

let routes = Kirin.router [
  Kirin.get "/" home_handler;
  Kirin.get "/hello/:name" hello_handler;
  Kirin.get "/json" json_handler;
  Kirin.get "/users/:id" user_handler;
]

let () =
  Kirin.start ~port:3000
  @@ Kirin.logger
  @@ Kirin.timing
  @@ routes
