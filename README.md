# 🦒 Kirin

> OCaml 5.x Eio-native Web Framework

**Dream의 DX + Axum의 아키텍처 + Eio의 Direct-style**

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
- **View-agnostic** - Works with HTMX, React SSR, or JSON APIs

## Installation

```bash
opam pin add kirin git+https://github.com/jeong-sik/kirin.git
```

### Dependencies

- OCaml 5.1+
- eio, eio_main
- cohttp-eio 6.x
- yojson, uri

## API Overview

### Routing

```ocaml
Kirin.get "/path" handler
Kirin.post "/path" handler
Kirin.put "/path" handler
Kirin.delete "/path" handler

(* Path parameters *)
Kirin.get "/users/:id" (fun req ->
  let id = Kirin.param "id" req in ...)

(* Scoped routes with middleware *)
Kirin.scope "/admin" [auth_middleware] [
  Kirin.get "/dashboard" dashboard_handler;
]
```

### Request

```ocaml
Kirin.param "name" req      (* Path param *)
Kirin.query "page" req      (* Query param *)
Kirin.header "auth" req     (* Header *)
Kirin.body req              (* Body string *)
Kirin.json_body req         (* Parse JSON body *)
Kirin.form_body req         (* Parse form body *)
```

### Response

```ocaml
Kirin.html "<h1>Hello</h1>"
Kirin.json (`Assoc [...])
Kirin.text "plain text"
Kirin.redirect "/new-url"
Kirin.not_found ()

(* HTMX support *)
Kirin.htmx ~target:"#main" ~swap:"innerHTML" html_content
```

### Middleware

```ocaml
Kirin.start ~port:8000
  @@ Kirin.logger        (* Logs requests/responses *)
  @@ Kirin.timing        (* Adds X-Response-Time header *)
  @@ Kirin.cors ()       (* CORS headers *)
  @@ Kirin.catch handler (* Error handling *)
  @@ routes
```

## Philosophy

1. **Bare Functions** - Handlers are `Request.t -> Response.t`
2. **Algebraic Composition** - Routes/middleware compose naturally
3. **Minimal Core** - 5 essential types: request, response, handler, middleware, route
4. **Secure Defaults** - CORS, CSRF, cookie signing built-in
5. **No Magic** - Debuggable, no excessive PPX

## License

MIT
