# Breaking Changes: v1.x to v2.0.0

This document lists all breaking changes in Kirin v2.0.0 and provides migration guidance.

## 1. WebRTC Module Restructure

The internal mock PeerConnection and DataChannel modules were removed. The WebRTC adapter
now wraps the real `ocaml-webrtc` library.

### Removed Modules

- `Kirin.WebRTC.PeerConnection` -- removed entirely
- `Kirin.WebRTC.DataChannel` -- removed entirely

### Replacements

| Before (v1.x) | After (v2.0.0) |
|----------------|----------------|
| `WebRTC.PeerConnection.create` | `WebRTC.create_peer ~role:Client ()` |
| `WebRTC.DataChannel.create peer "label"` | `WebRTC.create_datachannel peer ~label:"label"` |
| `WebRTC.PeerConnection.t` | `WebRTC.Peer.t` |

### Migration

```ocaml
(* Before *)
let pc = Kirin.WebRTC.PeerConnection.create () in
let dc = Kirin.WebRTC.DataChannel.create pc "chat" in
...

(* After *)
let peer = Kirin.WebRTC.create_peer ~role:Client () in
let dc = Kirin.WebRTC.create_datachannel peer ~label:"chat" in
...
```

## 2. WebRTC Type Rename

The ICE state type was renamed to reflect the broader connection state it represents.

| Before (v1.x) | After (v2.0.0) |
|----------------|----------------|
| `Kirin.webrtc_ice_state` | `Kirin.webrtc_connection_state` |

The type now has 6 variants: `New`, `Connecting`, `Connected`, `Disconnected`, `Failed`, `Closed`.

### Migration

```ocaml
(* Before *)
let state : Kirin.webrtc_ice_state = get_state peer in
...

(* After *)
let state : Kirin.webrtc_connection_state = get_state peer in
...
```

## 3. WebRTC Dead Types Removed

The following types were removed from `webrtc_config` because they are no longer
needed after the switch from mock to real WebRTC:

- `datachannel_state`
- `datachannel_options`
- `default_datachannel_options`

### Migration

Use the types from `Webrtc.Webrtc_eio` (exposed as `WebRTC.Peer`) directly.

## 4. Stream EOS Type Change

The end-of-stream signaling mechanism changed from empty string sentinel to `Option`.

| Before (v1.x) | After (v2.0.0) |
|----------------|----------------|
| `string Eio.Stream.t` | `string option Eio.Stream.t` |
| EOS = `""` (empty string) | EOS = `None` |
| Data = `"payload"` | Data = `Some "payload"` |

This prevents false end-of-stream on protocols where empty string is valid data
(e.g., SSE heartbeat, WebSocket keep-alive).

### Migration

```ocaml
(* Before: consuming a stream *)
let rec consume stream =
  let chunk = Eio.Stream.take stream in
  if chunk = "" then () (* EOS *)
  else begin process chunk; consume stream end

(* After: consuming a stream *)
let rec consume stream =
  match Eio.Stream.take stream with
  | None -> ()  (* EOS *)
  | Some chunk -> process chunk; consume stream
```

```ocaml
(* Before: producing to a stream *)
Eio.Stream.add stream "data";
Eio.Stream.add stream ""  (* signal EOS *)

(* After: producing to a stream *)
Eio.Stream.add stream (Some "data");
Eio.Stream.add stream None  (* signal EOS *)
```

The `Response.body` type reflects this change:

```ocaml
(* Before *)
type body = String of string | Stream of string Eio.Stream.t | ...

(* After *)
type body = String of string | Stream of string option Eio.Stream.t | ...
```

## 5. Module Splits via Include Pattern

Several large modules were split into sub-modules for maintainability. The public API
is preserved via `include` re-exports, so most code requires no changes.

| Original Module | Split Into |
|-----------------|------------|
| `validation.ml` | `validation_schema.ml` + `validation_format.ml` |
| `testing.ml` | `testing_mock.ml` + `testing_assert.ml` + `testing_request.ml` + `testing_response.ml` + `testing_json_path.ml` |
| `webrtc_adapter.ml` | `webrtc_config.ml` + `webrtc_signaling.ml` |
| `metrics.ml` | `metric_common.ml` + `metric_counter.ml` + `metric_gauge.ml` + `metric_histogram.ml` + `metric_summary.ml` + `metric_prometheus.ml` |
| `openapi.ml` | `openapi_schema.ml` + `openapi_ui.ml` |

### Migration

If you import `Kirin.Validation`, `Kirin.Testing`, `Kirin.Metrics`, etc., no changes
are needed -- the parent module re-exports everything. If you directly reference
sub-module files in your build system, update the references.

## 6. Maturity Annotations

All modules now carry `@status` annotations in their `.mli` files:

- **stable**: API is locked. Breaking changes only in major versions.
- **needs-work**: Functional but API may be refined in minor versions.
- **experimental**: API may change at any time. Not covered by semver guarantees.

Experimental modules: all SSR adapters (React, Vue, Svelte, Angular, Astro, Qwik,
Preact, Solid, Lit, Alpine, Remix, HTMX, TanStack, Browser), MCP adapter, WebRTC
adapter, tRPC.

## Summary of Required Actions

1. **WebRTC users**: Replace `PeerConnection`/`DataChannel` with `create_peer`/`create_datachannel`/`Peer`.
2. **WebRTC users**: Rename `webrtc_ice_state` to `webrtc_connection_state`.
3. **Streaming users**: Update stream consumers/producers for `string option` instead of `string`.
4. **All users**: Review experimental module usage -- those APIs are not covered by semver stability guarantees.
