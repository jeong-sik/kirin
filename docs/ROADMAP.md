# Kirin Roadmap

> 🦒 OCaml 5.x Eio-native Web Framework

## Current Status: Phase 17 Complete ✅ (All phases through 17)

**418 tests passing** (204 core + 22 MCP + 20 Auth + 32 OpenAPI + 36 i18n + 60 Validation + 44 Testing)

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
│   ├── sse.ml         - Server-Sent Events
│   └── webrtc_adapter.ml - WebRTC P2P (Phase 11)
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
├── Production (Phase 10)
│   ├── health.ml      - Kubernetes health checks
│   ├── metrics.ml     - Prometheus metrics
│   └── shutdown.ml    - Graceful shutdown
│
├── Database (Phase 12)
│   ├── db.ml          - Caqti-eio connection pooling
│   ├── migrate.ml     - Version-tracked migrations
│   └── query.ml       - Type-safe query builder
│
├── API Documentation (Phase 14)
│   └── openapi.ml     - OpenAPI 3.0 spec builder
│
├── Internationalization (Phase 15)
│   └── i18n.ml        - Multi-language support
│
├── Validation (Phase 16)
│   └── validation.ml  - Schema-based validation
│
├── Testing (Phase 17)
│   └── testing.ml     - Test utilities and mocks
│
└── Browser (Phase 7)
    └── kirin_browser.ml - Client-side framework (js_of_ocaml)
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

### Phase 7: Browser/WASM ✅ Complete
**Goal**: 브라우저에서 실행

- [x] js_of_ocaml 기반 브라우저 모듈 (`kirin.browser`)
- [x] DOM 조작 (query, create, manipulate, events)
- [x] Fetch API (GET/POST/PUT/DELETE/PATCH)
- [x] History API 라우팅 (pushState, popstate)
- [x] localStorage 지원
- [x] SSR + Hydration (component registry, data attributes)
- [x] Timer utilities (setTimeout, setInterval, requestAnimationFrame)
- [ ] wasm_of_ocaml 지원 (future)

### Phase 8: MCP Integration ✅ Complete
**Goal**: AI 에이전트 통합

- [x] MCP Server (tools/resources/prompts 제공)
- [x] MCP Client (외부 MCP 서버 연결)
- [x] JSON-RPC 2.0 구현
- [x] stdio/HTTP+SSE 전송
- [x] Kirin 어댑터 통합
- [x] 22 tests for MCP module

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

### Phase 11: WebRTC ✅ Complete
**Goal**: P2P 통신 지원

- [x] ocaml-webrtc 통합 (순수 OCaml WebRTC 구현)
- [x] PeerConnection API (ICE, SDP)
- [x] DataChannel API (send/receive)
- [x] Signaling server (WebSocket + JSON-RPC)
- [x] SDP offer/answer 생성 (RFC 4566, 8832)
- [x] ICE candidate 처리
- [x] STUN server 정보 엔드포인트
- [x] 12 WebRTC tests

---

### Phase 12: Database Integration ✅ Complete
**Goal**: 데이터베이스 통합

- [x] Caqti-eio integration (connection pooling, transactions)
- [x] Migration system (version-tracked, checksum validation)
- [x] Type-safe query builder (select, join, insert, update, delete)
- [x] Multi-database support (PostgreSQL, SQLite, MariaDB)
- [x] Health check integration
- [x] 29 database tests (18 Query, 7 Migrate, 4 Db)

---

### Phase 13: Authentication ✅ Complete
**Goal**: 인증 및 보안 모듈

- [x] JWT (encode/decode, HMAC-SHA256, claims validation)
- [x] Password hashing (PBKDF2-SHA256, salt, strength checker)
- [x] Session management (in-memory store, TTL)
- [x] CSRF protection (token generation/validation)
- [x] OAuth2 providers (Google, GitHub, Apple, Discord)
- [x] PKCE support (code_challenge, code_verifier)
- [x] Auth middleware (Bearer, Session, API Key)
- [x] Rate limiting (per-IP, per-user)
- [x] 20 authentication tests (5 JWT, 4 Password, 3 Session, 4 CSRF, 4 OAuth2)

---

### Phase 14: OpenAPI/Swagger ✅ Complete
**Goal**: API 문서 자동 생성

- [x] OpenAPI 3.0 specification builder
- [x] Schema helpers (string, integer, number, boolean, array, object)
- [x] Path, operation, parameter, response builders
- [x] Component schema registry ($ref support)
- [x] Swagger UI HTML generation
- [x] ReDoc HTML generation
- [x] Server configuration
- [x] Contact/License info
- [x] 32 OpenAPI tests

---

### Phase 15: Internationalization ✅ Complete
**Goal**: 다국어 지원

- [x] CLDR-compliant pluralization (en, fr, ko, ru, ar)
- [x] Accept-Language header parsing (quality sorting)
- [x] Translation interpolation ({{placeholder}} syntax)
- [x] Locale detection from headers
- [x] Fallback locale support
- [x] Number formatting by locale
- [x] Currency formatting (USD, EUR, KRW, etc.)
- [x] Date formatting by locale
- [x] 36 i18n tests

---

### Phase 16: Schema-based Validation ✅ Complete
**Goal**: 요청 데이터 검증

- [x] JSON Schema validators (string, int, float, bool, null)
- [x] String constraints (min/max length, pattern, format)
- [x] Number constraints (min/max, exclusive, multiple_of)
- [x] Array validation (min/max items, unique items)
- [x] Object validation (required fields, additional properties)
- [x] Format validators (email, uuid, uri, date, datetime)
- [x] Composition (oneOf, anyOf, allOf, enum, const)
- [x] Query parameter coercion (string→int/bool)
- [x] Custom validators
- [x] Error formatting (JSON, string)
- [x] 60 validation tests

---

### Phase 17: Testing Utilities ✅ Complete
**Goal**: 테스트 도구

- [x] Test request builders (GET, POST, PUT, DELETE, PATCH)
- [x] Request headers and query params helpers
- [x] JSON body helpers
- [x] Bearer token authentication
- [x] Test response inspectors (status, headers, body)
- [x] JSON path assertions (nested, array index)
- [x] Mock server with endpoint matching
- [x] Call counting and verification
- [x] Random data generators (string, email, int)
- [x] 44 testing utility tests

---

## Future Phases

---

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                     Application Layer                        │
├─────────────────────────────────────────────────────────────┤
│ HTTP/REST │ GraphQL │ gRPC │ WebSocket │ SSE │ MCP │ WebRTC│
├─────────────────────────────────────────────────────────────┤
│                     Kirin Core                               │
│  ┌─────────────┬─────────────┬─────────────┐               │
│  │   Router    │  Middleware │  Response   │               │
│  └─────────────┴─────────────┴─────────────┘               │
├─────────────────────────────────────────────────────────────┤
│                High-Performance Layer                        │
│  ┌─────────┬─────────┬─────────┬─────────┐                 │
│  │Streaming│  Pool   │  Cache  │  Jobs   │                 │
│  └─────────┴─────────┴─────────┴─────────┘                 │
├─────────────────────────────────────────────────────────────┤
│                Production Layer                              │
│  ┌─────────┬─────────┬─────────┐                           │
│  │ Health  │ Metrics │Shutdown │                           │
│  └─────────┴─────────┴─────────┘                           │
├─────────────────────────────────────────────────────────────┤
│                     Eio Runtime                              │
│             (OCaml 5.x Effects + Multicore)                 │
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────┐
│                Browser (js_of_ocaml)                         │
├─────────────────────────────────────────────────────────────┤
│  ┌─────────┬─────────┬─────────┬─────────┬─────────┐       │
│  │   DOM   │  Fetch  │ History │ Storage │ Hydrate │       │
│  └─────────┴─────────┴─────────┴─────────┴─────────┘       │
└─────────────────────────────────────────────────────────────┘
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
| 7 | Browser | 0 |
| 8 | MCP | 22 |
| 9 | High-Performance | 44 |
| 10 | Production | 21 |
| 11 | WebRTC | 12 |
| 12 | Database | 29 |
| 13 | Authentication | 20 |
| 14 | OpenAPI | 32 |
| 15 | i18n | 36 |
| 16 | Validation | 60 |
| 17 | Testing | 44 |
| **Total** | | **418** |

---

## Contributing

1. Fork the repository
2. Create a feature branch
3. Submit a PR with tests

## License

MIT
