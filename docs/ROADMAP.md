# Kirin Roadmap

> 🦒 OCaml 5.x Eio-native Web Framework

## Current Status: Phase 10 Complete ✅

**163 tests passing**

```
lib/
├── Core Framework
│   ├── kirin.ml       - Main module, API facade
│   ├── request.ml     - Request handling
│   ├── response.ml    - Response builders
│   ├── router.ml      - Route matching
│   ├── middleware.ml  - Middleware pipeline
│   └── server.ml      - Eio HTTP server
│
├── Web Features
│   ├── cookie.ml      - Cookie handling (signed)
│   ├── static.ml      - Static file serving
│   ├── multipart.ml   - Multipart form parsing
│   ├── etag.ml        - ETag caching
│   ├── compress.ml    - Gzip/deflate compression
│   ├── ratelimit.ml   - Rate limiting
│   ├── template.ml    - HTML template engine
│   └── tls_config.ml  - TLS/HTTPS configuration
│
├── Real-time
│   ├── websocket.ml   - WebSocket (RFC 6455)
│   └── sse.ml         - Server-Sent Events
│
├── Protocol Adapters
│   ├── grpc.ml            - gRPC integration
│   ├── graphql_adapter.ml - GraphQL integration
│   └── mcp_adapter.ml     - MCP (AI agent) integration
│
├── High-Performance (Phase 9)
│   ├── stream.ml      - Streaming I/O
│   ├── pool.ml        - Connection pooling
│   ├── backpressure.ml- Flow control
│   ├── cache.ml       - LRU cache with TTL
│   ├── jobs.ml        - Background job queue
│   └── parallel.ml    - OCaml 5 Domain parallelism
│
└── Production (Phase 10)
    ├── health.ml      - Kubernetes health checks
    ├── metrics.ml     - Prometheus metrics
    └── shutdown.ml    - Graceful shutdown
```

---

## Foundation Phases

### Phase 1: MVP ✅ Complete
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

### Phase 2: Core Features ✅ Complete
**Goal**: Production-ready 기본 기능

- [x] Cookie handling (signed, encrypted)
- [x] Static file serving
- [x] Multipart form parsing (RFC 7578)
- [x] ETag / conditional requests
- [x] Compression (gzip, deflate)
- [x] Rate limiting middleware

### Phase 3: Advanced ✅ Complete
**Goal**: Real-time 지원

- [x] WebSocket support (RFC 6455)
- [x] SSE (Server-Sent Events)
- [x] Template engine (Mustache-like)
- [x] TLS/HTTPS configuration

---

## Protocol Extension Phases

### Phase 5: gRPC Integration ✅ Complete
**Goal**: gRPC 서비스 지원

- [x] grpc-direct 통합 (Kirin.Grpc 모듈)
- [x] Unified middleware/interceptor bridge
- [x] Streaming RPC support (unary, server, client, bidi)
- [x] gRPC status codes and helpers
- [x] Health check and reflection modules
- [x] 8 gRPC tests

### Phase 6: GraphQL Support ✅ Complete
**Goal**: GraphQL API 지원

- [x] Graphql 어댑터 (Kirin.Graphql 모듈)
- [x] Schema-first development
- [x] HTTP handler (POST /graphql)
- [x] GraphQL Playground
- [x] Batched queries
- [x] 8 GraphQL tests

### Phase 7: Browser/WASM (Partial)
**Goal**: 브라우저에서 실행

- [x] Direct-Style Promise Effects 설계
- [ ] wasm_of_ocaml 지원 (future)
- [ ] Universal routing
- [ ] SSR + Hydration

### Phase 8: MCP Integration ✅ Complete
**Goal**: AI 에이전트 통합

- [x] MCP Server (tools/resources/prompts 제공)
- [x] MCP Client (외부 MCP 서버 연결)
- [x] JSON-RPC 2.0 구현
- [x] stdio/HTTP+SSE 전송
- [x] Kirin 어댑터 통합

---

## Performance & Production Phases

### Phase 9: High-Performance ✅ Complete
**Goal**: 고성능 컴포넌트

- [x] Streaming I/O (chunked transfer)
- [x] Connection Pool (generic resource pooling)
- [x] Backpressure (token bucket, bounded channels)
- [x] LRU Cache (TTL, stats)
- [x] Background Jobs (priority queue, workers)
- [x] Parallel Processing (OCaml 5 Domains)
- [x] 44 tests for Phase 9 modules

### Phase 10: Production Hardening ✅ Complete
**Goal**: 프로덕션 배포 준비

- [x] Health Checks (/health, /live, /ready, Kubernetes-style)
- [x] Prometheus Metrics (Counter, Gauge, Histogram, Summary)
- [x] Graceful Shutdown (SIGTERM/SIGINT, connection draining)
- [x] TLS Configuration (from Phase 3)
- [x] 21 tests for Phase 10 modules

---

## Future Phases

### Phase 11: WebRTC (Planned)
**Goal**: P2P 통신 지원

- [ ] libdatachannel OCaml 바인딩
- [ ] Data channels
- [ ] Signaling server
- [ ] TURN/STUN integration

### Phase 12: Database Integration (Planned)
**Goal**: 데이터베이스 통합

- [ ] Caqti-eio integration
- [ ] Migration system
- [ ] Query builder

---

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│                   Application Layer                      │
├─────────────────────────────────────────────────────────┤
│ HTTP/REST │ GraphQL │ gRPC │ WebSocket │ SSE │ MCP     │
├─────────────────────────────────────────────────────────┤
│                   Kirin Core                             │
│  ┌─────────────┬─────────────┬─────────────┐           │
│  │   Router    │  Middleware │  Response   │           │
│  └─────────────┴─────────────┴─────────────┘           │
├─────────────────────────────────────────────────────────┤
│              High-Performance Layer                      │
│  ┌─────────┬─────────┬─────────┬─────────┐             │
│  │Streaming│  Pool   │  Cache  │  Jobs   │             │
│  └─────────┴─────────┴─────────┴─────────┘             │
├─────────────────────────────────────────────────────────┤
│              Production Layer                            │
│  ┌─────────┬─────────┬─────────┐                       │
│  │ Health  │ Metrics │Shutdown │                       │
│  └─────────┴─────────┴─────────┘                       │
├─────────────────────────────────────────────────────────┤
│                   Eio Runtime                            │
│           (OCaml 5.x Effects + Multicore)               │
└─────────────────────────────────────────────────────────┘
```

---

## Examples

```bash
# Run examples
dune exec examples/hello_world/main.exe
dune exec examples/blog/main.exe
dune exec examples/graphql_api/main.exe
dune exec examples/grpc_service/main.exe
dune exec examples/high_performance/main.exe
```

---

## Test Summary

| Phase | Module | Tests |
|-------|--------|-------|
| 1-3 | Core (Response, Router, etc.) | 75 |
| 5 | gRPC | 8 |
| 6 | GraphQL | 8 |
| 9 | High-Performance | 44 |
| 10 | Production | 21 |
| **Total** | | **163** |

---

## Contributing

1. Fork the repository
2. Create a feature branch
3. Submit a PR with tests

## License

MIT
