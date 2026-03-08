# Changelog

All notable changes to Kirin will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.0.0] - Unreleased

### Changed
- **WebRTC**: Replaced internal mock PeerConnection/DataChannel with real ocaml-webrtc thin wrapper. Full ICE, DTLS, SCTP, and DataChannel stack via `Webrtc_eio`.
- **WebRTC API (breaking)**: `Kirin.WebRTC.PeerConnection` and `Kirin.WebRTC.DataChannel` modules removed. Use `Kirin.WebRTC.create_peer`, `Kirin.WebRTC.create_datachannel`, and `Kirin.WebRTC.Peer` instead.
- **WebRTC Config**: `webrtc_config.ml` now re-exports `Webrtc.Ice.connection_state` for type equality with ocaml-webrtc. Added `ice_server_of_stun`/`stun_of_ice_server` conversion functions.

### Removed
- Internal mock PeerConnection and DataChannel (303 lines of mock code).

## [0.9.0] - 2026-03-08

### Changed
- **Stream EOS Type Safety**: `string Eio.Stream.t` to `string option Eio.Stream.t` — `None` signals end-of-stream instead of empty string sentinel. Prevents false EOS on protocols where empty string is valid data.
- **Traceparent Hex Validation**: `parse_traceparent` now validates hex characters (lowercase a-f, 0-9) per W3C Trace Context spec.
- **Random Seeding**: `Random.self_init ()` at module init for trace ID uniqueness.
- **File Splits**: validation.ml, testing.ml, webrtc_adapter.ml, metrics.ml split into sub-modules via `include` pattern. Public API unchanged.
- **Coverage Gate**: CI threshold raised from 50% to 60%.

### Fixed
- **Stream Producer EOS Safety**: All 3 producer functions (`response`, `file_response`, `file_inline`) wrapped with `Fun.protect ~finally` to guarantee `None` (EOS) is always sent even on producer exception. Prevents consumer fiber hang.
- **WebRTC Signaling Exception**: Narrowed `decode_message` catch-all to `Yojson.Safe.Util.Type_error`.

### Added
- **.mli Interface Files**: Added for 8 more modules (trace, backpressure, cache, validation, metrics, websocket, sse, cookie). Total: 15.

## [0.8.0] - 2026-03-07

### Added
- **Trace Module**: Lightweight request tracing with W3C Trace Context (`traceparent`) support. Pluggable exporters (Stderr, JSON). No OpenTelemetry dependency.
- **Middleware Context**: `ctx : Hmap.t` field on `Request.t` for type-safe state sharing between middleware.
- **.mli Interface Files**: Added for 7 core modules (request, response, router, middleware, server, stream, pool).
- **CI Coverage**: `bisect_ppx` coverage job with 50% warning gate.

### Fixed
- **Streaming**: `Response.Producer` now uses true chunked streaming via `Eio.Flow.source` adapter instead of buffering the entire response in memory.
- **Exception Narrowing**: Narrowed 46 broad `with _ ->` handlers to specific exception types across 20+ files.

### Changed
- **OpenAPI Split**: `openapi.ml` (868 lines) split into `openapi_schema.ml` + `openapi_ui.ml`. Backward compatible via `include Openapi_schema`.

## [0.5.2] - 2026-02-04

### Fixed
- **Cache Module**: Replaced `Stdlib.Mutex` with `Eio.Mutex` to prevent fiber blocking during cache operations.
- **SSE Module**: Implemented real streaming via `Producer` pattern instead of buffering entire response. Added `response_legacy` for backward compatibility with list-based API.

### Changed
- **`Kirin.sse_response`**: Now points to `Sse.response_legacy` for backward compatibility.
- **`Kirin.sse_stream_response`**: New function for real-time SSE streaming from `Eio.Stream`.

## [0.5.1] - 2026-02-02

### Fixed
- **Time_compat**: Added Eio-native timestamps module.

## [0.5.0] - 2026-02-02

### Added
- **Fs_compat Module**: Eio-native file I/O with blocking fallback
  - `Fs_compat.set_fs` for global filesystem injection at startup
  - `Fs_compat.load` / `load_binary` for reading files
  - `Fs_compat.save` for writing files
  - `Fs_compat.file_exists` / `is_directory` / `readdir`
  - Prevents fiber blocking during file operations

### Changed
- **19 Modules Migrated**: All blocking file I/O converted to `Fs_compat`
  - Core: `i18n.ml`, `tls_config.ml`, `static.ml`, `testing.ml`
  - React: `manifest.ml`, `assets.ml`, `vite.ml`
  - Angular: `handler.ml`
  - Astro: `handler.ml`, `ssr.ml`
  - Preact: `manifest.ml`
  - Qwik: `handler.ml`, `ssr.ml`
  - Svelte: `manifest.ml`, `codegen.ml`
  - Vue: `manifest.ml`

## [0.3.1] - 2026-01-27

### Fixed
- **Logger Safety**: Added `Logger.shutdown` to ensure logs are flushed before exit.
- **Backpressure Warning**: Documented blocking behavior of logger stream under extreme load.

## [0.3.0] - 2026-01-27

### Added
- **Pydantic-style Auto-Validation**: New `Type` DSL for combining OCaml types with validation schemas.
- **`Kirin.validated`**: A higher-order handler that automatically parses and validates JSON request bodies into OCaml records.
- **Validation Example App**: Demonstration of type-safe validation in `examples/validation_app`.

## [0.2.0] - 2026-01-27

### Added
- **GraphQL Relay Support** (`kirin.graphql_relay`): Global ID, Connections, and Cursor-based pagination.
- **ReScript Integration**: Official example for using Kirin with ReScript and Relay.
- **Relay Example App**: Full-stack demonstration in `examples/relay_app`.

## [0.1.0] - 2026-01-24

### Added

#### Core Framework
- Direct-style async web framework built on OCaml 5.x and Eio
- Type-safe routing with path parameters (`:name` syntax)
- Middleware composition with `@@` operator
- WebSocket support (RFC 6455)
- Server-Sent Events (SSE)
- Template engine with Mustache-like syntax
- Cookie handling with HMAC signing
- File uploads (multipart form-data)
- Static file serving with MIME detection
- Rate limiting (token bucket)
- Compression (gzip/deflate)
- ETag caching

#### High-Performance Components
- Streaming I/O for large files
- Connection pool with health checks
- Backpressure flow control
- LRU cache with TTL
- Background job queue
- Domain-based parallelism

#### Authentication (`kirin.auth`)
- JWT tokens (HS256, RS256, ES256)
- OAuth 2.0 / OIDC
- Session management
- CSRF protection
- API keys
- Rate limiting per user

#### MCP Integration (`kirin.mcp`)
- Model Context Protocol server implementation
- Tool registration and execution
- Resource management
- SSE transport

#### API Documentation
- OpenAPI 3.0 spec generation
- Swagger UI integration

#### Internationalization (`kirin.i18n`)
- Message catalogs
- Locale detection
- Pluralization rules

#### Form Validation
- Type-safe validators
- Error messages
- Custom rules

#### Testing Utilities
- Mock requests/responses
- Assertion helpers
- Snapshot testing

#### Frontend SSR Integrations
- **React** (`kirin.react`) - 3-level integration (Static/Hydration/Full SSR)
- **HTMX** (`kirin.htmx`) - Server-side enhancement
- **tRPC** (`kirin.trpc`) - Type-safe RPC
- **TanStack** (`kirin.tanstack`) - Router & Query integration
- **Solid.js** (`kirin.solid`) - Fine-grained reactivity SSR
- **Svelte** (`kirin.svelte`) - Compiler-optimized SSR
- **Vue** (`kirin.vue`) - Options & Composition API SSR
- **Angular** (`kirin.angular`) - Full SSR with Universal
- **Qwik** (`kirin.qwik`) - Resumability-first SSR
- **Astro** (`kirin.astro`) - Islands architecture SSR
- **Preact** (`kirin.preact`) - Lightweight React alternative
- **Lit** (`kirin.lit`) - Web Components SSR
- **Alpine.js** (`kirin.alpine`) - Minimal reactivity SSR
- **Remix** (`kirin.remix`) - Full-stack React meta-framework

### Statistics
- **1,314 tests** passing
- **21 library modules**
- **10 example applications**

[0.1.0]: https://github.com/jeong-sik/kirin/releases/tag/v0.1.0

## [0.5.1] - 2026-02-02

### Added
- **Time_compat Module**: Eio-native timestamp API with Unix fallback
  - `Time_compat.set_clock` for global clock injection at startup
  - `Time_compat.now ()` replaces `Unix.gettimeofday ()` 
  - Prevents domain blocking in async context
