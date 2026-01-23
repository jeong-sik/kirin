# Kirin Design Document

> OCaml 5.x Eio-native Web Framework
>
> **Dream의 DX + Axum의 아키텍처 + Eio의 Direct-style**

## 1. Philosophy

### Core Principles

| Principle | Description |
|-----------|-------------|
| **Bare Functions** | Handlers are `Request.t -> Response.t` |
| **Algebraic Composition** | Routes/middleware compose with `@@` |
| **Minimal Core** | 5 essential types only |
| **Secure Defaults** | CORS, cookie signing built-in |
| **No Magic** | Debuggable, minimal PPX |

### Design Influences

```
┌─────────────────────────────────────────────────────────┐
│                      Kirin                               │
├─────────────────────────────────────────────────────────┤
│  Dream (OCaml)      → DX, API simplicity                │
│  Axum (Rust)        → Middleware composition, types     │
│  Eio (OCaml 5.x)    → Direct-style async, multicore     │
│  Wisp (Gleam)       → Pattern matching routing          │
│  Phoenix (Elixir)   → Productivity, LiveView concept    │
└─────────────────────────────────────────────────────────┘
```

## 2. Anti-patterns (What NOT to do)

| Pattern | Why Avoid |
|---------|-----------|
| GADT Routing (Tapak-style) | Learning curve too steep |
| Actor Model (Actix-style) | Unnatural in OCaml |
| Heavy PPX | Debugging becomes hard |
| Monadic Chaining | Loses Eio's direct-style advantage |

## 3. Core Types

```ocaml
(* Only 5 essential types *)

type handler = Request.t -> Response.t
(** Direct style function - no monads! *)

type middleware = handler -> handler
(** Handler transformer for cross-cutting concerns *)

type route = {
  meth : Http.Method.t;
  pattern : string;
  handler : handler;
}
(** Single route definition *)

type router = route list -> handler
(** Route dispatcher *)

type config = {
  port : int;
  host : string;
  (* ... *)
}
(** Server configuration *)
```

## 4. View Strategy: View-Agnostic

Kirin does not impose a view layer. It supports:

### HTML (Direct)
```ocaml
Kirin.html "<h1>Hello</h1>"
```

### HTMX (Built-in helper)
```ocaml
Kirin.htmx ~target:"#main" ~swap:"innerHTML" html_content
```

### JSON API
```ocaml
Kirin.json (`Assoc [("status", `String "ok")])
```

### Template Engines (Optional)
- Tyxml
- EML (Dream-style)
- Custom

### SSR (Future)
- React SSR via js_of_ocaml interop
- WASM compilation via wasm_of_ocaml

## 5. Middleware Pipeline

```ocaml
(* Middleware composes right-to-left with @@ *)
let () = Kirin.start ~port:8000
  @@ Kirin.logger        (* 1st: Logs request/response *)
  @@ Kirin.timing        (* 2nd: Adds X-Response-Time *)
  @@ Kirin.cors ()       (* 3rd: CORS headers *)
  @@ Kirin.catch handler (* 4th: Error handling *)
  @@ routes              (* 5th: Route dispatch *)

(* Execution order: routes → catch → cors → timing → logger *)
```

## 6. Eio Integration

### Why Eio?

| Feature | Lwt | Async | Eio |
|---------|-----|-------|-----|
| Style | Monadic | Monadic | **Direct** |
| Multicore | No | No | **Yes** |
| Effects | No | No | **Yes** |
| Debuggability | Low | Low | **High** |

### Direct-style Example

```ocaml
(* No Lwt.bind, no >>= *)
let handler req =
  let user_id = Kirin.param "id" req in
  let user = Database.find_user user_id in  (* Just a function call! *)
  Kirin.json (User.to_json user)
```

## 7. Long-term Vision

```
Ultimate Goal: OCaml's NestJS
─────────────────────────────

HTTP/REST ──┐
            │
GraphQL ────┼──→  [ Kirin Core ]  ──→  Eio Runtime
            │         │
gRPC ───────┤         ├── Unified Middleware
            │         ├── Protocol Adapters
WebSocket ──┘         └── Type-safe Routing

Future:
├── WebRTC (data channels)
└── WASM (browser runtime)
```

## 8. References

- [Dream](https://github.com/aantron/dream) - OCaml web framework
- [Axum](https://github.com/tokio-rs/axum) - Rust web framework
- [Eio](https://github.com/ocaml-multicore/eio) - OCaml 5 effects-based I/O
- [cohttp-eio](https://github.com/mirage/ocaml-cohttp) - HTTP library for Eio
- [grpc-eio](https://github.com/dialohq/ocaml-grpc) - gRPC for OCaml with Eio
