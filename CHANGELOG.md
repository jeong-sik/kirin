# Changelog

All notable changes to Kirin will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

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
