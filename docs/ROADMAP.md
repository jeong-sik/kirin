# Kirin Roadmap

> 🦒 OCaml 5.x Eio-native Web Framework

## Current Status

**Phase 1 MVP: ✅ COMPLETE**

```
lib/
├── kirin.ml       (169 lines) - Main module, API facade
├── request.ml     (80 lines)  - Request handling
├── response.ml    (95 lines)  - Response builders
├── router.ml      (100 lines) - Route matching
├── middleware.ml  (120 lines) - Middleware pipeline
└── server.ml      (50 lines)  - Eio HTTP server

Total: ~614 lines
```

---

## Foundation Phases

### Phase 1: MVP ✅ (Complete)
**Goal**: Hello World가 동작하는 최소 프레임워크

- [x] cohttp-eio 기반 서버
- [x] Dream-like Router DSL
- [x] Path params (`/users/:id`)
- [x] Query params parsing
- [x] JSON body parsing (yojson)
- [x] Form body parsing (urlencoded)
- [x] Middleware composition (`@@`)
- [x] Response helpers (html, json, text, redirect)
- [x] HTMX support (`Kirin.htmx`)
- [x] Basic middlewares (logger, cors, timing, catch)

### Phase 2: Core Features (1 week)
**Goal**: Production-ready 기본 기능

- [ ] Cookie handling (signed, encrypted)
- [ ] Static file serving
- [ ] Multipart form parsing
- [ ] ETag / conditional requests
- [ ] Compression (gzip, br)
- [ ] Rate limiting middleware

### Phase 3: Advanced (2 weeks)
**Goal**: Real-time & Database 지원

- [ ] WebSocket support
- [ ] SSE (Server-Sent Events)
- [ ] Sessions (memory, cookie, db backends)
- [ ] Template engine integration (Tyxml/EML)
- [ ] Database integration (caqti-eio, pgx-eio)
- [ ] Connection pooling

### Phase 4: Production Ready (2 weeks)
**Goal**: 운영 환경 배포 가능

- [ ] HTTPS/TLS support
- [ ] Graceful shutdown
- [ ] Multi-domain parallelism
- [ ] Health check endpoint
- [ ] Metrics (Prometheus format)
- [ ] Comprehensive test suite
- [ ] API documentation
- [ ] Benchmarks vs Dream, Axum, Express

---

## Protocol Extension Phases

### Phase 5: gRPC Integration ✅ (Complete)
**Goal**: gRPC 서비스 지원

- [x] grpc-direct 통합 (Kirin.Grpc 모듈)
- [x] Unified middleware/interceptor bridge
- [x] Streaming RPC support (unary, server, client, bidi)
- [x] gRPC status codes and helpers
- [x] gRPC-Web support via grpc-direct
- [x] Health check and reflection modules
- [x] 8 gRPC tests added
- [ ] Protobuf code generation (use ocaml-protoc separately)

### Phase 6: GraphQL Support
**Goal**: GraphQL API 지원

- [ ] graphql-lwt 어댑터 (Lwt → Eio)
- [ ] Schema-first development
- [ ] Subscriptions (WebSocket)
- [ ] DataLoader pattern
- [ ] GraphQL Playground integration

### Phase 7: Cross-Platform (WASM)
**Goal**: 브라우저에서 실행

- [ ] wasm_of_ocaml 지원
- [ ] Universal routing (server/client)
- [ ] SSR + Hydration
- [ ] Service Worker support

### Phase ∞: WebRTC
**Goal**: P2P 통신 지원

- [ ] libdatachannel OCaml 바인딩
- [ ] Data channels
- [ ] Signaling server
- [ ] TURN/STUN integration

---

## Architecture Vision

```
┌─────────────────────────────────────────────────────────┐
│                   Application Layer                      │
├─────────────────────────────────────────────────────────┤
│  HTTP/REST │ GraphQL │ gRPC │ WebSocket │ SSE          │
├─────────────────────────────────────────────────────────┤
│                   Kirin Core                             │
│  ┌─────────────┬─────────────┬─────────────┐           │
│  │   Router    │  Middleware │  Response   │           │
│  └─────────────┴─────────────┴─────────────┘           │
├─────────────────────────────────────────────────────────┤
│                   Protocol Adapters                      │
│  ┌─────────┬─────────┬─────────┬─────────┐             │
│  │ cohttp  │ grpc-eio│graphql  │ ws-eio  │             │
│  └─────────┴─────────┴─────────┴─────────┘             │
├─────────────────────────────────────────────────────────┤
│                   Eio Runtime                            │
│           (OCaml 5.x Effects + Multicore)               │
└─────────────────────────────────────────────────────────┘
```

---

## Contributing

1. Fork the repository
2. Create a feature branch
3. Submit a PR with tests

## License

MIT
