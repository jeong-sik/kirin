(lang dune 3.16)

(name kirin)

(version 0.3.0)

(generate_opam_files true)

(source
 (github jeong-sik/kirin))

(authors "Seungji Park <yousleepwhen@gmail.com>")

(maintainers "Seungji Park <yousleepwhen@gmail.com>")

(license MIT)

(documentation https://github.com/jeong-sik/kirin)

(package
 (name kirin)
 (synopsis "OCaml 5.x Eio-native web framework")
 (description
  "Kirin is a modern OCaml 5.x web framework built on Eio for direct-style async.

Features:
- Routing with path parameters (/users/:id)
- Middleware composition (logger, CORS, compression, rate limiting)
- WebSocket support (RFC 6455)
- Server-Sent Events (SSE)
- HTML template engine (Mustache-like syntax)
- Cookie handling with HMAC signing
- ETag/304 caching
- Multipart form parsing (RFC 7578)
- TLS/HTTPS configuration
- Response compression (gzip/deflate)
- gRPC integration (via grpc-direct)
- Unified HTTP + gRPC server")
 (depends
  (ocaml (>= "5.1"))
  (dune (>= "3.16"))
  (eio (>= "1.0"))
  (eio_main (>= "1.0"))
  (cohttp (>= "5.0"))
  (cohttp-eio (>= "6.0"))
  (yojson (>= "2.0"))
  (uri (>= "4.0"))
  (mirage-crypto (>= "1.0"))
  (mirage-crypto-rng (>= "1.0"))
  (cstruct (>= "6.0"))
  (base64 (>= "3.0"))
  (digestif (>= "1.0"))
  (decompress (>= "1.5"))
  (bigstringaf (>= "0.9"))
  (caqti (>= "2.1"))
  (caqti-eio (>= "2.1"))
  (ppx_deriving (>= "5.0"))
  (ppx_deriving_yojson (>= "3.0"))
  (grpc-direct (>= "0.1.0"))
  (grpc-direct-core (>= "0.1.0"))
  (ocaml-webrtc (>= "0.1.0"))
  (graphql (>= "0.14.0"))
  (graphql_parser (>= "0.14.0"))
  (alcotest :with-test)
  (odoc :with-doc))
 (tags
  (web eio http framework websocket sse templates grpc graphql)))
