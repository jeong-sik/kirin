# Kirin Roadmap

> 🦒 OCaml 5.x Eio-native Web Framework

## Current Status: Phase 29 Complete ✅ (All phases through 29)

**1053 tests passing** (204 core + 6 Relay + 22 MCP + 20 Auth + 32 OpenAPI + 36 i18n + 60 Validation + 44 Testing + 58 React + 38 HTMX + 74 tRPC + 90 TanStack + 99 Solid + 99 Svelte + 70 Vue + 47 Angular + 54 Qwik + 76 Astro + 48 Preact + 40 Lit + 48 Alpine + 55 Remix)

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
├── React Integration (Phase 18)
│   ├── manifest.ml    - Vite manifest parsing
│   ├── assets.ml      - Asset URL resolution
│   ├── vite.ml        - Vite dev server integration
│   ├── meta.ml        - SEO meta tag helpers
│   ├── data.ml        - Initial data serialization
│   ├── hydrate.ml     - HTML shell generation
│   ├── protocol.ml    - JSON-RPC for SSR
│   ├── worker.ml      - Generic worker interface
│   ├── node_worker.ml - Node.js subprocess pool
│   ├── ssr.ml         - SSR engine
│   └── streaming.ml   - React 18 streaming SSR
│
├── HTMX+ (Phase 19)
│   ├── kirin_htmx.ml  - API facade
│   ├── headers.ml     - HTMX response headers
│   ├── hyperscript.ml - Hyperscript DSL helpers
│   ├── alpine.ml      - Alpine.js integration
│   ├── extensions.ml  - HTMX extensions
│   ├── oob.ml         - Out-of-Band swaps
│   └── form.ml        - Form helpers
│
├── tRPC (Phase 20)
│   ├── kirin_trpc.ml  - API facade
│   ├── procedure.ml   - Procedure types (query/mutation/subscription)
│   ├── trpc_router.ml - Router for organizing procedures
│   ├── context.ml     - Request context
│   ├── batch.ml       - Batch request handling
│   ├── handler.ml     - Kirin route handler integration
│   ├── subscription.ml - WebSocket subscriptions
│   └── codegen.ml     - TypeScript/Zod/JSON Schema generation
│
├── TanStack Router (Phase 21)
│   ├── kirin_tanstack.ml - API facade
│   ├── route_def.ml      - Route definitions with loaders/actions
│   ├── file_router.ml    - File-based route discovery
│   ├── loader.ml         - Data loading (Remix-style)
│   ├── action.ml         - Form handling and mutations
│   ├── manifest.ml       - Route manifest generation
│   ├── preload.ml        - Route preloading hints
│   ├── handler.ml        - Kirin route handler integration
│   └── codegen.ml        - TypeScript type generation
│
├── Solid.js SSR (Phase 22)
│   ├── kirin_solid.ml    - API facade
│   ├── route_def.ml      - Route definitions with Solid patterns
│   ├── file_router.ml    - File-based route discovery
│   ├── loader.ml         - Data loading (SolidStart-style)
│   ├── action.ml         - Form handling and server functions
│   ├── manifest.ml       - Route manifest generation
│   ├── preload.ml        - Route preloading hints
│   ├── meta.ml           - Solid Meta tag helpers
│   ├── data.ml           - Initial data serialization
│   ├── hydrate.ml        - HTML shell generation
│   ├── protocol.ml       - JSON-RPC for SSR
│   ├── ssr.ml            - SSR engine with caching
│   ├── streaming.ml      - Suspense streaming SSR
│   ├── handler.ml        - Kirin route handler integration
│   └── codegen.ml        - TypeScript generation
│
├── Svelte SSR (Phase 23)
│   ├── kirin_svelte.ml   - API facade (SvelteKit-style)
│   ├── route_def.ml      - Route definitions with Svelte patterns
│   ├── file_router.ml    - File-based route discovery (+page, +layout, +server)
│   ├── loader.ml         - Data loading (+page.server.ts style)
│   ├── action.ml         - Form actions with named actions
│   ├── manifest.ml       - Route manifest generation
│   ├── preload.ml        - Route preloading hints (data-sveltekit-*)
│   ├── meta.ml           - Svelte Meta tag helpers (svelte:head)
│   ├── data.ml           - Initial data serialization (__sveltekit_data)
│   ├── hydrate.ml        - HTML shell generation
│   ├── protocol.ml       - JSON-RPC for SSR
│   ├── worker.ml         - Generic worker interface
│   ├── ssr.ml            - SSR engine with LRU caching
│   ├── streaming.ml      - Suspense streaming SSR
│   ├── handler.ml        - Kirin route handler integration
│   └── codegen.ml        - TypeScript route type generation
│
├── Vue/Nuxt SSR (Phase 24)
│   ├── kirin_vue.ml      - API facade (Nuxt 3-style)
│   ├── route_def.ml      - Route definitions with Vue patterns
│   ├── file_router.ml    - Nuxt file routing ([id], [...slug], [[optional]])
│   ├── loader.ml         - Data loading (useFetch, useAsyncData)
│   ├── action.ml         - Server actions (H3 event handler)
│   ├── manifest.ml       - Route manifest generation
│   ├── preload.ml        - Route preloading hints
│   ├── meta.ml           - Vue Meta tag helpers (useHead)
│   ├── data.ml           - Initial data serialization (__NUXT__)
│   ├── hydrate.ml        - HTML shell generation
│   ├── protocol.ml       - JSON-RPC 2.0 for SSR
│   ├── worker.ml         - Generic worker interface
│   ├── ssr.ml            - SSR engine with LRU caching
│   ├── streaming.ml      - Vue 3 Suspense streaming SSR
│   ├── handler.ml        - Kirin route handler integration
│   └── codegen.ml        - TypeScript/Vue type generation
│
├── Angular Universal SSR (Phase 25)
│   ├── kirin_angular.ml  - API facade (Angular Universal-style)
│   ├── route_def.ml      - Route definitions with render modes (SSR/CSR/SSG)
│   ├── file_router.ml    - Angular file routing (:id, **, matchers)
│   ├── transfer_state.ml - HTTP Transfer Cache (ng-state)
│   ├── hydration.ml      - Incremental hydration (v19+)
│   ├── meta.ml           - Angular Meta tag helpers
│   ├── protocol.ml       - JSON-RPC 2.0 for SSR
│   ├── ssr.ml            - SSR engine with LRU caching
│   ├── handler.ml        - Kirin route handler integration
│   └── codegen.ml        - TypeScript/Angular type generation
│
├── Qwik Resumable SSR (Phase 26)
│   ├── kirin_qwik.ml     - API facade (QwikCity-style)
│   ├── qrl.ml            - QRL (Qwik URL) serialized function references
│   ├── signal.ml         - Fine-grained reactive signals
│   ├── route_def.ml      - Route definitions with resumability
│   ├── file_router.ml    - QwikCity file routing ([id], [...slug])
│   ├── container.ml      - Container state for resumability
│   ├── loader.ml         - routeLoader$ implementation
│   ├── action.ml         - routeAction$ implementation
│   ├── meta.ml           - Qwik Meta tag helpers (useDocumentHead)
│   ├── protocol.ml       - JSON-RPC 2.0 for SSR
│   ├── ssr.ml            - SSR engine with resumability support
│   ├── handler.ml        - Kirin route handler integration
│   └── codegen.ml        - TypeScript/Qwik type generation
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

### Phase 18: React Integration ✅ Complete
**Goal**: React SSR (Server-Side Rendering) with Best Practices

Three levels of integration:

| Level | Description | Use Case |
|-------|-------------|----------|
| Level 1: Static | Vite build serving | SPA, CSR apps |
| Level 2: Hydration | Server HTML shell + client hydrate | SEO meta tags |
| Level 3: Full SSR | Node.js worker pool rendering | Full SEO, TTFB optimization |

- [x] **Level 1: Static/Vite**
  - [x] Vite manifest parsing (content-addressed hashing)
  - [x] Asset URL resolution with cache busting
  - [x] Dev server proxy for HMR
  - [x] Static file serving with MIME types

- [x] **Level 2: Hydration**
  - [x] SEO meta tag helpers (OG, Twitter Cards)
  - [x] XSS-safe initial data serialization
  - [x] HTML shell generation
  - [x] TanStack Query-style dehydration

- [x] **Level 3: Full SSR**
  - [x] JSON-RPC 2.0 protocol over stdio
  - [x] Worker pool management (round-robin)
  - [x] Memory limit monitoring
  - [x] Graceful restart after N requests
  - [x] Render caching with TTL
  - [x] React 18 streaming SSR (SSE)
  - [x] Progressive hydration helpers

- [x] 58 React tests

---

## Future Phases

### Phase 19: HTMX Enhancement ✅ Complete
**Goal**: HTMX + Hyperscript + Alpine.js 통합

- [x] HTMX response headers (HX-Trigger, HX-Push-Url, HX-Redirect, etc.)
- [x] Hyperscript DSL helpers (`_="on click..."`)
- [x] Alpine.js x-data/x-show/x-model/x-bind helpers
- [x] HTMX extensions (preload, ws, sse, response-targets, loading-states)
- [x] Out-of-Band (OOB) swap helpers
- [x] Form generation with HTMX attributes
- [x] Common patterns (infinite scroll, lazy load, search, click-to-edit)
- [x] 38 HTMX tests

### Phase 20: tRPC Integration ✅ Complete
**Goal**: End-to-end 타입 안전 API

- [x] tRPC router adapter for Kirin
- [x] Procedure definition DSL (query/mutation/subscription)
- [x] Input validators (string, int, bool, list, optional, field)
- [x] Output serializers (json, string, int, bool, list, option)
- [x] Type info for TypeScript codegen
- [x] Subscription support (SSE events, registry, WebSocket messages)
- [x] Batch request handling (parse, execute, serialize)
- [x] Context management (headers, bearer token extraction)
- [x] Handler integration with Kirin routes
- [x] TypeScript client generation
- [x] JSON Schema generation
- [x] Zod schema generation
- [x] 74 tRPC tests

### Phase 21: TanStack Router ✅ Complete
**Goal**: 파일 기반 타입 안전 라우팅

- [x] Route definitions with loaders/actions (route_def.ml)
- [x] File-based route discovery ([id], [...slug], [[opt]], (group))
- [x] Route manifest generation and JSON serialization
- [x] Loader pattern (parallel/sequential, redirect, not_found, unauthorized)
- [x] Action pattern (success, redirect, validation_error, server_error)
- [x] Type-safe route params (string, int, uuid, slug, optional)
- [x] Route preloading hints (intent, viewport, render)
- [x] Kirin handler integration
- [x] TypeScript code generation (route types, router file, hooks)
- [x] 90 TanStack Router tests

### Phase 22: Solid.js SSR ✅ Complete
**Goal**: React 대안 SSR 지원

Solid.js is a reactive JavaScript framework with fine-grained reactivity. Unlike React's virtual DOM diffing, Solid compiles to direct DOM operations for better performance.

Three integration levels (reusing React SSR architecture pattern):

| Level | Description | Use Case |
|-------|-------------|----------|
| Level 1: Static | Vite build serving | SPA, CSR apps |
| Level 2: Hydration | Server HTML shell + progressive hydrate | SEO meta tags |
| Level 3: Full SSR | Node.js worker pool rendering + streaming | Full SEO, TTFB optimization |

- [x] **Route System**
  - [x] Route definitions with loaders/actions (route_def.ml)
  - [x] File-based route discovery ([id], [...slug], (group))
  - [x] Route manifest generation and JSON serialization
  - [x] SolidStart-style patterns

- [x] **Data Loading**
  - [x] Loader pattern (parallel/sequential, redirect, not_found)
  - [x] Action pattern (success, redirect, validation_error, server_error)
  - [x] createResource/createRouteData support
  - [x] Optimistic updates

- [x] **Preloading**
  - [x] Route preloading hints (intent, viewport, render)
  - [x] Progressive hydration priorities (Immediate, Visible, Idle, Interaction)

- [x] **SSR Engine**
  - [x] JSON-RPC 2.0 protocol over stdio (protocol.ml)
  - [x] Worker pool management with round-robin
  - [x] Render caching with TTL and LRU eviction
  - [x] Memory limit monitoring (200MB threshold)
  - [x] Graceful restart after N requests

- [x] **Streaming SSR**
  - [x] Suspense support with out-of-order streaming
  - [x] renderToStringAsync / renderToStream equivalents
  - [x] Progressive chunk delivery (Html, Script, Complete, Error)
  - [x] SSE-based streaming response

- [x] **Meta Tags**
  - [x] SEO meta tag helpers (OG, Twitter Cards)
  - [x] Builder pattern for meta composition
  - [x] JSON serialization for client hydration

- [x] **Hydration**
  - [x] XSS-safe initial data serialization (__SOLID_DATA__)
  - [x] HTML shell generation with Vite integration
  - [x] Island architecture support (component-based hydration)
  - [x] Streaming placeholders and replacement scripts

- [x] **Code Generation**
  - [x] TypeScript route types
  - [x] Loader/action type definitions
  - [x] Router configuration generation

- [x] 99 Solid.js tests

### Phase 23: Svelte SSR ✅ Complete
**Goal**: Svelte/SvelteKit SSR 지원

SvelteKit is a full-featured framework for Svelte with file-based routing, server-side rendering, and form actions. Kirin provides first-class integration following SvelteKit conventions.

Three integration levels (reusing React/Solid SSR architecture pattern):

| Level | Description | Use Case |
|-------|-------------|----------|
| Level 1: Static | Vite build serving | SPA, CSR apps |
| Level 2: Hydration | Server HTML shell + Vite HMR | SEO meta tags |
| Level 3: Full SSR | Worker pool rendering + streaming | Full SEO, TTFB optimization |

- [x] **Route System**
  - [x] Route definitions with loaders/actions (route_def.ml)
  - [x] SvelteKit file discovery (+page.svelte, +page.server.ts, +layout.svelte)
  - [x] Dynamic routes ([id], [...rest], [[optional]], (group))
  - [x] Route manifest generation and JSON serialization

- [x] **Data Loading** (+page.server.ts style)
  - [x] Loader pattern with LoadData, LoadRedirect, LoadError, LoadNotFound
  - [x] Server-only and universal loaders
  - [x] Cookies and headers access in loader context
  - [x] Route params extraction

- [x] **Form Actions** (+page.server.ts actions)
  - [x] Default and named actions
  - [x] ActionSuccess, ActionFail, ActionRedirect, ActionError results
  - [x] Form data parsing (Text, Multiple, File types)
  - [x] Validation helpers (required, email, min/max length)
  - [x] Action output JSON serialization

- [x] **Preloading** (data-sveltekit-* attributes)
  - [x] Multiple strategies (Hover, Tap, Viewport, Eager, Off)
  - [x] Preload data (data-sveltekit-preload-data)
  - [x] Preload code (data-sveltekit-preload-code)
  - [x] Route hint generation (modulepreload, prefetch)

- [x] **Meta Tags** (svelte:head)
  - [x] SEO meta tag helpers (OG, Twitter Cards)
  - [x] Builder pattern for meta composition
  - [x] JSON serialization for client hydration

- [x] **SSR Engine**
  - [x] JSON-RPC 2.0 protocol over stdio (protocol.ml)
  - [x] Worker pool with round-robin scheduling
  - [x] LRU render cache with TTL and eviction
  - [x] Memory limit monitoring (200MB threshold)
  - [x] Graceful restart after N requests
  - [x] Stats tracking (renders, cache hits, errors)

- [x] **Streaming SSR**
  - [x] Progressive chunk delivery (Shell, Html, Script, Complete, Error)
  - [x] SSE-based streaming response
  - [x] Hydration priorities (Immediate, Visible, Idle, Interaction)
  - [x] Streaming placeholders and boundaries

- [x] **Hydration**
  - [x] XSS-safe initial data serialization (__sveltekit_data)
  - [x] HTML shell generation with Vite integration
  - [x] SPA fallback mode

- [x] **Code Generation**
  - [x] TypeScript route types
  - [x] Load function type definitions
  - [x] Action type definitions
  - [x] Router configuration generation
  - [x] Hooks templates (hooks.server.ts, hooks.client.ts)

- [x] **Handler Integration**
  - [x] Page handlers with SSR fallback
  - [x] Load data handlers (/__data.json)
  - [x] Action handlers (POST with action param)
  - [x] Health check handler (/_health)
  - [x] Vite dev proxy (HMR, WebSocket)

- [x] 99 Svelte tests

### Phase 24: Vue/Nuxt SSR ✅
**Goal**: Vue/Nuxt SSR 지원

- [x] Vue 3 worker pool (reuse SSR architecture)
- [x] Nuxt-style file routing (`[id]`, `[...slug]`, `[[optional]]`)
- [x] useFetch and useAsyncData patterns
- [x] Server routes and API endpoints (H3 event handler)
- [x] Streaming SSR with Suspense
- [x] 70 Vue tests

### Phase 25: Angular Universal SSR ✅
**Goal**: Angular Universal SSR 지원

Angular Universal is the official Angular SSR solution with features like Hybrid Rendering (different render modes per route), Incremental Hydration (hydrate on demand), and HTTP Transfer Cache.

Key Angular v19+ Concepts:

| Concept | Description |
|---------|-------------|
| Hybrid Rendering | SSR + SSG + CSR per route via ServerRoute config |
| Incremental Hydration | Hydrate components on triggers (idle, viewport, interaction) |
| HTTP Transfer Cache | `ng-state` script tag for server→client data transfer |

- [x] **Route System**
  - [x] Route definitions with render modes (SSR, CSR, SSG per route)
  - [x] Angular file routing (`:id`, `**` wildcard, route matchers)
  - [x] Guards and resolvers support
  - [x] Lazy loading with loadChildren
  - [x] Named outlets and child routes

- [x] **Transfer State**
  - [x] HTTP Transfer Cache for server-fetched data
  - [x] `ng-state` script tag generation
  - [x] XSS-safe JSON serialization
  - [x] TTL and cache key management

- [x] **Hydration**
  - [x] Full hydration mode (default)
  - [x] Incremental hydration (v19+)
  - [x] Hydration triggers (OnIdle, OnViewport, OnInteraction, OnTimer, OnHover, Never)
  - [x] Hydration boundary markers (`<!--nghb:id:trigger-->`)
  - [x] Skip hydration attribute (ngSkipHydration)
  - [x] Component state tracking (Dehydrated → Hydrating → Hydrated)

- [x] **SSR Engine**
  - [x] JSON-RPC 2.0 protocol over stdio
  - [x] Worker pool with round-robin scheduling
  - [x] LRU render cache with TTL
  - [x] Memory limit monitoring (200MB threshold)
  - [x] Graceful restart after N requests
  - [x] Cache hit rate tracking

- [x] **Handler Integration**
  - [x] SSR, CSR, Hybrid, Prerender modes
  - [x] Static file serving with MIME types
  - [x] Fallback to CSR on SSR failure
  - [x] Route-based render mode selection

- [x] **Code Generation**
  - [x] TypeScript route parameter types
  - [x] ServerRoute configuration (app.routes.server.ts)
  - [x] Guard and resolver templates
  - [x] Type guard functions

- [x] 47 Angular tests

### Phase 26: Qwik Resumable SSR ✅
**Goal**: Qwik's revolutionary resumability approach (near-zero JS on initial load)

Qwik is fundamentally different from other frameworks - instead of hydration, it uses "resumability" where server-rendered HTML is immediately interactive. The key innovation is QRL (Qwik URL) - serialized function references that lazy-load on demand.

Key Qwik Concepts:

| Concept | Description |
|---------|-------------|
| Resumability | No hydration needed - pick up where server left off |
| QRL | Serialized function URLs (`chunk.js#symbol`) for lazy loading |
| Signals | Fine-grained reactive state (like SolidJS) |
| Container | Serialized app state in `<script type="qwik/json">` |
| $ Suffix | Convention for lazy-loaded boundaries (component$, etc.) |

- [x] **QRL System**
  - [x] QRL creation and serialization (`chunk.js#symbol[captures]`)
  - [x] Capture variables (closure serialization)
  - [x] Reference captures (container state indices)
  - [x] QRL parsing from string
  - [x] Event handler attributes (on:click, on:input, on:submit)
  - [x] Prefetch hints generation

- [x] **Signal Reactivity**
  - [x] Signal creation and updates
  - [x] Computed signals (derived values)
  - [x] Resource signals (async data with pending/resolved/rejected)
  - [x] Store (proxy-like object signals)
  - [x] Signal context for SSR serialization

- [x] **Route System**
  - [x] Route definitions with render modes (SSR, Static, SPA)
  - [x] QwikCity file routing ([id], [...slug], [[optional]], (group))
  - [x] Layout support and nested routing
  - [x] Guards and loaders integration

- [x] **Container State**
  - [x] Serializable object format (SObj)
  - [x] Object references ($ref$)
  - [x] Signal references ($signal$)
  - [x] QRL references ($qrl$)
  - [x] JSON serialization to `<script type="qwik/json">`
  - [x] Container pause/resume for SSR

- [x] **Data Loading**
  - [x] routeLoader$ implementation
  - [x] Data dependencies and invalidation
  - [x] Redirect and error results
  - [x] Context with URL, params, headers

- [x] **Actions**
  - [x] routeAction$ implementation
  - [x] Form data parsing (text, multiple, file)
  - [x] Validation helpers (email, required, minLength)
  - [x] Action results (ok, fail, redirect)

- [x] **Meta/Head**
  - [x] useDocumentHead-style helpers
  - [x] SEO optimization (title, description, og, twitter)
  - [x] Canonical URLs and robots
  - [x] Structured data (JSON-LD)

- [x] **SSR Engine**
  - [x] JSON-RPC 2.0 protocol over stdio
  - [x] Worker pool with round-robin scheduling
  - [x] LRU render cache with TTL
  - [x] Cache hit rate tracking
  - [x] Memory-efficient resumability output

- [x] **Handler Integration**
  - [x] SSR, Static, SPA modes
  - [x] Static file serving with MIME types
  - [x] Fallback to SPA on SSR failure
  - [x] Loader and action endpoints

- [x] **Code Generation**
  - [x] TypeScript route parameter types
  - [x] Route configuration generation
  - [x] QRL helper functions
  - [x] Component$ skeletons

- [x] 54 Qwik tests

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
│                   Frontend Integration                       │
│  ┌──────┬──────┬──────┬──────┬───────┬──────┬──────┬──────┐│
│  │React │Solid │Svelte│ Vue  │Angular│ Qwik │HTMX+ │ tRPC ││
│  │ SSR  │ SSR  │ SSR  │ SSR  │  SSR  │Resume│Alpine│Adapt.││
│  └──────┴──────┴──────┴──────┴───────┴──────┴──────┴──────┘│
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
| 18 | React | 58 |
| 19 | HTMX+ | 38 |
| 20 | tRPC | 74 |
| 21 | TanStack Router | 90 |
| 22 | Solid.js SSR | 99 |
| 23 | Svelte SSR | 99 |
| 24 | Vue/Nuxt SSR | 70 |
| 25 | Angular Universal SSR | 47 |
| 26 | Qwik Resumable SSR | 54 |
| **Total** | | **1047** |

---

## Contributing

1. Fork the repository
2. Create a feature branch
3. Submit a PR with tests

## License

MIT
