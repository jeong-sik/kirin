# Kirin

> OCaml 5.x Eio-native Web Framework

**Dream's DX + Axum's Architecture + Eio's Direct-style**

```ocaml
let () = Kirin.start ~port:8000
  @@ Kirin.logger
  @@ Kirin.router [
       Kirin.get "/" (fun _ -> Kirin.html "Hello, Kirin!");
       Kirin.get "/hello/:name" (fun req ->
         let name = Kirin.param "name" req in
         Kirin.html ("Hello, " ^ name ^ "!"));
       Kirin.get "/api/users/:id" (fun req ->
         let id = Kirin.param "id" req in
         Kirin.json (`Assoc [("id", `Int (int_of_string id))]));
     ]
```

## Features

- **Direct-style async** - No Lwt monads, just functions
- **Type-safe routing** - Path params with `:name` syntax
- **Middleware composition** - `@@` operator for elegant chaining
- **Sub-millisecond responses** - Built on Eio for multicore performance
- **WebSocket support** - RFC 6455 compliant
- **Server-Sent Events** - Real-time streaming
- **Template engine** - Mustache-like syntax with HTML escaping
- **Cookie handling** - HMAC-signed cookies for security
- **File uploads** - Multipart form-data parsing
- **Static files** - With MIME type detection and caching
- **Rate limiting** - Token bucket algorithm
- **Compression** - Gzip and deflate support
- **ETag caching** - Automatic 304 responses

## Installation

```bash
opam pin add kirin git+https://github.com/jeong-sik/kirin.git
```

### Dependencies

- OCaml 5.1+
- eio, eio_main
- cohttp-eio 6.x
- yojson, uri
- digestif (for HMAC/hashing)
- decompress (for gzip/deflate)

## Quick Start

Create `main.ml`:

```ocaml
let () = Kirin.start ~port:3000
  @@ Kirin.logger
  @@ Kirin.router [
       Kirin.get "/" (fun _ -> Kirin.html "<h1>Hello!</h1>");
     ]
```

Build and run:

```bash
dune exec ./main.exe
# Server running at http://localhost:3000
```

## Routing

### Basic Routes

```ocaml
Kirin.get "/path" handler      (* GET *)
Kirin.post "/path" handler     (* POST *)
Kirin.put "/path" handler      (* PUT *)
Kirin.patch "/path" handler    (* PATCH *)
Kirin.delete "/path" handler   (* DELETE *)
Kirin.head "/path" handler     (* HEAD *)
Kirin.options "/path" handler  (* OPTIONS *)
```

### Path Parameters

```ocaml
Kirin.get "/users/:id" (fun req ->
  let id = Kirin.param "id" req in
  Kirin.json (`Assoc [("user_id", `String id)]))

Kirin.get "/posts/:user_id/:post_id" (fun req ->
  let user_id = Kirin.param "user_id" req in
  let post_id = Kirin.param "post_id" req in
  (* ... *))
```

### Scoped Routes

```ocaml
let auth_middleware handler req =
  match Kirin.header "Authorization" req with
  | Some _ -> handler req
  | None -> Kirin.Response.make ~status:`Unauthorized "Unauthorized"

let admin_routes = Kirin.scope "/admin" [auth_middleware] [
  Kirin.get "/dashboard" dashboard_handler;
  Kirin.get "/users" users_handler;
  Kirin.post "/users" create_user_handler;
]

let routes = Kirin.router (
  [Kirin.get "/" home_handler] @ admin_routes
)
```

## Request Handling

```ocaml
(* Path parameters *)
let id = Kirin.param "id" req
let name = Kirin.param_opt "name" req  (* Returns option *)

(* Query parameters *)
let page = Kirin.query "page" req
let limit = Kirin.query_opt "limit" req

(* Headers *)
let auth = Kirin.header "Authorization" req

(* Body *)
let body = Kirin.body req                 (* Raw string *)
let json = Kirin.json_body req            (* Result of Yojson.Safe.t *)
let form = Kirin.form_body req            (* (string * string list) list *)

(* Cookies *)
let session = Kirin.cookie "session" req
let signed = Kirin.cookie_signed "token" req  (* HMAC-verified *)
```

## Response Helpers

```ocaml
(* Basic responses *)
Kirin.html "<h1>Hello</h1>"
Kirin.text "plain text"
Kirin.json (`Assoc [("status", `String "ok")])
Kirin.json_string {|{"already": "serialized"}|}

(* Status codes *)
Kirin.empty `No_content
Kirin.not_found ()
Kirin.bad_request ()
Kirin.server_error ()

(* Redirects *)
Kirin.redirect "/new-url"
Kirin.redirect_permanent "/moved"

(* Modify responses *)
Kirin.html "content"
|> Kirin.with_header "X-Custom" "value"
|> Kirin.with_status `Created

(* Set cookies *)
Kirin.html "content"
|> Kirin.set_cookie "session" "abc123"
|> Kirin.set_cookie_signed "token" "secure-value"

(* HTMX support *)
Kirin.htmx ~target:"#main" ~swap:"innerHTML" "<div>Updated</div>"
```

## Middleware

Middlewares are handler transformers: `handler -> handler`

```ocaml
let () = Kirin.start ~port:8000
  @@ Kirin.logger              (* Log requests/responses *)
  @@ Kirin.timing              (* X-Response-Time header *)
  @@ Kirin.cors ()             (* CORS headers *)
  @@ Kirin.compress            (* Gzip/deflate compression *)
  @@ Kirin.etag                (* ETag caching *)
  @@ Kirin.rate_limit          (* Rate limiting *)
  @@ Kirin.catch handler       (* Error handling *)
  @@ routes
```

### Custom Middleware

```ocaml
let auth_middleware : Kirin.middleware = fun handler req ->
  match Kirin.header "Authorization" req with
  | Some token when is_valid token -> handler req
  | _ -> Kirin.Response.make ~status:`Unauthorized "Unauthorized"

let request_id : Kirin.middleware = fun handler req ->
  let resp = handler req in
  Kirin.with_header "X-Request-ID" (generate_id ()) resp
```

### CORS Configuration

```ocaml
let cors_config : Kirin.cors_config = {
  origins = ["https://example.com"; "https://app.example.com"];
  methods = ["GET"; "POST"; "PUT"; "DELETE"];
  headers = ["Content-Type"; "Authorization"];
  credentials = true;
  max_age = Some 86400;
}

let () = Kirin.start ~port:8000
  @@ Kirin.cors ~config:cors_config ()
  @@ routes
```

### Rate Limiting

```ocaml
let rate_config : Kirin.rate_limit_config = {
  requests_per_second = 10.0;
  burst_size = 20;
}

let () = Kirin.start ~port:8000
  @@ Kirin.rate_limit ~config:rate_config
  @@ routes
```

## Static Files

```ocaml
let () = Kirin.start ~port:8000
  @@ Kirin.logger
  @@ Kirin.static "/assets" ~dir:"./public"
  @@ routes

(* GET /assets/style.css -> ./public/style.css *)
(* GET /assets/js/app.js -> ./public/js/app.js *)
```

## Templates

Mustache-like syntax with automatic HTML escaping:

```ocaml
let template = {|
  <html>
  <head><title>{{title}}</title></head>
  <body>
    <h1>Hello, {{name}}!</h1>

    {{#if logged_in}}
      <p>Welcome back!</p>
    {{else}}
      <p>Please log in.</p>
    {{/if logged_in}}

    <ul>
    {{#each items}}
      <li>{{name}} - {{price}}</li>
    {{/each items}}
    </ul>

    {{{raw_html}}}  <!-- Unescaped -->
  </body>
  </html>
|}

let handler _req =
  let ctx = Kirin.template_context_of [
    ("title", `String "My Shop");
    ("name", `String "Alice");
    ("logged_in", `Bool true);
    ("items", `List [
      `Assoc [("name", `String "Widget"); ("price", `String "$9.99")];
      `Assoc [("name", `String "Gadget"); ("price", `String "$19.99")];
    ]);
    ("raw_html", `String "<b>Bold</b>");
  ] in
  Kirin.template_html ctx template
```

### Template Features

- `{{variable}}` - HTML-escaped interpolation
- `{{{variable}}}` - Raw interpolation (no escaping)
- `{{#if key}}...{{else}}...{{/if key}}` - Conditionals
- `{{#unless key}}...{{/unless key}}` - Inverted conditionals
- `{{#each key}}...{{/each key}}` - Iteration
- `{{user.profile.name}}` - Dot notation for nested values

## WebSocket

```ocaml
let ws_handler : Kirin.Websocket.handler = {
  on_open = (fun () -> print_endline "Client connected");
  on_message = (fun frame ->
    match frame.opcode with
    | Kirin.Text ->
      print_endline ("Received: " ^ frame.payload);
      Some (Kirin.ws_text ("Echo: " ^ frame.payload))
    | Kirin.Ping ->
      Some (Kirin.ws_pong ~payload:frame.payload)
    | Kirin.Close ->
      None
    | _ -> None);
  on_close = (fun code reason ->
    print_endline ("Closed: " ^ reason));
  on_error = (fun msg ->
    print_endline ("Error: " ^ msg));
}

let () = Kirin.start ~port:8000
  @@ Kirin.websocket ~path:"/ws" ~handler:ws_handler
  @@ routes
```

### WebSocket Frames

```ocaml
(* Create frames *)
let text_frame = Kirin.ws_text "Hello"
let binary_frame = Kirin.ws_binary "\x00\x01\x02"
let ping = Kirin.ws_ping ()
let pong = Kirin.ws_pong ~payload:"ping-data"
let close = Kirin.ws_close ~code:Kirin.Normal ~reason:"Goodbye" ()

(* Encode for sending *)
let bytes = Kirin.ws_encode text_frame

(* Decode received data *)
match Kirin.ws_decode received_bytes with
| Ok (frame, consumed) -> (* ... *)
| Error msg -> (* ... *)
```

## Server-Sent Events (SSE)

```ocaml
let sse_handler _req =
  let events = [
    Kirin.sse_data "Hello, World!";
    Kirin.sse_event ~event_type:"update" "New data available"
    |> Kirin.sse_with_id "evt-123"
    |> Kirin.sse_with_retry 5000;
  ] in
  Kirin.sse_response events

(* Or use middleware *)
let () = Kirin.start ~port:8000
  @@ Kirin.sse ~path:"/events" ~on_events:(fun () -> [
       Kirin.sse_data (get_latest_data ())
     ])
  @@ routes
```

### Client-side:

```javascript
const events = new EventSource('/events');
events.onmessage = (e) => console.log(e.data);
events.addEventListener('update', (e) => console.log('Update:', e.data));
```

## File Uploads

```ocaml
let upload_handler req =
  match Kirin.multipart req with
  | Some form ->
    let title = Kirin.multipart_field "title" form in
    let file = Kirin.multipart_file "document" form in
    (match file with
    | Some f ->
      Printf.printf "Received: %s (%d bytes)\n"
        (Option.get f.filename)
        (String.length f.content);
      Kirin.json (`Assoc [("status", `String "uploaded")])
    | None ->
      Kirin.bad_request ~body:"No file uploaded" ())
  | None ->
    Kirin.bad_request ~body:"Invalid multipart data" ()
```

## Cookie Security

```ocaml
(* Set secret at startup (minimum 32 characters) *)
let () = Kirin.set_cookie_secret "your-secure-secret-key-at-least-32-chars"

let login_handler req =
  let user_id = authenticate req in
  Kirin.html "Logged in!"
  |> Kirin.set_cookie_signed "session" user_id

let protected_handler req =
  match Kirin.cookie_signed "session" req with
  | Some user_id -> (* ... *)
  | None -> Kirin.redirect "/login"
```

## Philosophy

1. **Bare Functions** - Handlers are `Request.t -> Response.t`
2. **Algebraic Composition** - Routes/middleware compose with `@@`
3. **Minimal Core** - 5 essential types: request, response, handler, middleware, route
4. **Secure Defaults** - CORS, cookie signing, HTML escaping built-in
5. **No Magic** - Debuggable, minimal PPX usage

## Documentation

- [API Reference](docs/API.md)
- [Design Document](docs/DESIGN.md)
- [Roadmap](docs/ROADMAP.md)

## License

MIT
