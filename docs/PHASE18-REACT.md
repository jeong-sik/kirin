# Phase 18: React Integration

> Kirin에서 React 앱을 서빙하고 SSR까지 지원

## 목표

3가지 레벨의 React 지원:

```
Level 1: Static React      ─── Vite 빌드 결과물 서빙
Level 2: Hydration         ─── 서버 템플릿 + 클라이언트 Hydration  
Level 3: Full SSR          ─── 서버에서 React 렌더링
```

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                        Kirin Server                              │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────────────┐  │
│  │ Level 1      │  │ Level 2      │  │ Level 3              │  │
│  │ Static       │  │ Hydration    │  │ Full SSR             │  │
│  │              │  │              │  │                      │  │
│  │ static.ml    │  │ template.ml  │  │ quickjs_runtime.ml   │  │
│  │ + vite.ml    │  │ + hydrate    │  │ or node_worker.ml    │  │
│  └──────────────┘  └──────────────┘  └──────────────────────┘  │
│                                                                  │
├─────────────────────────────────────────────────────────────────┤
│                     React Integration Layer                      │
│                        lib/react/                                │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │ react.ml      - API facade                               │   │
│  │ vite.ml       - Vite dev server integration              │   │
│  │ manifest.ml   - Build manifest parsing                   │   │
│  │ hydrate.ml    - Data serialization for hydration         │   │
│  │ ssr.ml        - Server-side rendering engine             │   │
│  │ runtime.ml    - JS runtime abstraction (QuickJS/Node)    │   │
│  └─────────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────────┘
```

## Level 1: Static React (Vite Integration)

### 기능
- Vite 개발 서버 프록시 (HMR 지원)
- Production 빌드 결과물 서빙
- Asset manifest 파싱 (hashed filenames)

### API
```ocaml
(* Development: Vite dev server proxy *)
let routes = [
  Kirin.React.vite_dev_proxy ~port:5173 "/";
]

(* Production: Static serving with manifest *)
let routes = [
  Kirin.React.static ~manifest:"dist/.vite/manifest.json" ~dir:"dist" "/";
]
```

### 파일
```
lib/react/
├── vite.ml       - Vite proxy & manifest
└── static.ml     - Enhanced static serving
```

---

## Level 2: Template + Hydration

### 기능
- 서버에서 HTML 셸 생성 (SEO용 메타데이터)
- 초기 데이터를 `window.__INITIAL_DATA__`로 주입
- React가 클라이언트에서 hydrate

### API
```ocaml
let handler req =
  let user = fetch_user () in
  let initial_data = `Assoc [("user", user_to_json user)] in
  
  Kirin.React.hydrate
    ~title:"My App"
    ~meta:[("description", "A cool app")]
    ~scripts:["/assets/main.js"]
    ~initial_data
    ~root_id:"app"
    ()

(* 생성되는 HTML *)
(*
<!DOCTYPE html>
<html>
<head>
  <title>My App</title>
  <meta name="description" content="A cool app">
</head>
<body>
  <div id="app"></div>
  <script>window.__INITIAL_DATA__ = {"user": {...}}</script>
  <script src="/assets/main.js"></script>
</body>
</html>
*)
```

### 파일
```
lib/react/
├── hydrate.ml    - HTML shell + data injection
└── meta.ml       - SEO meta tag helpers
```

---

## Level 3: Full SSR

### 선택지

| Runtime | 장점 | 단점 |
|---------|------|------|
| **QuickJS** | 임베디드, 빠른 시작 | React 18 지원 미확인 |
| **Node.js subprocess** | 완전한 호환성 | 프로세스 오버헤드 |
| **Bun subprocess** | 빠름, 호환성 | Bun 설치 필요 |

### 권장: Node.js Worker Pool

```ocaml
(* Node.js worker pool로 SSR *)
let ssr_pool = Kirin.React.Ssr.create_pool
  ~workers:4
  ~bundle:"dist/server/entry-server.js"
  ()

let handler req =
  let url = Kirin.Request.path req in
  let! html = Kirin.React.Ssr.render ssr_pool ~url in
  Kirin.html html
```

### SSR Protocol (JSON-RPC over stdio)

```
Kirin ──stdin──> Node Worker
      <──stdout──
      
Request:  {"method": "render", "params": {"url": "/users/1"}}
Response: {"result": {"html": "<div>...</div>", "head": "..."}}
```

### 파일
```
lib/react/
├── ssr.ml           - SSR orchestration
├── node_worker.ml   - Node.js subprocess pool  
├── quickjs.ml       - QuickJS embedded (optional)
└── protocol.ml      - Worker communication
```

---

## 구현 순서

### Step 1: Core Infrastructure
```
lib/react/
├── dune
├── kirin_react.ml   - Main module
├── manifest.ml      - Vite manifest parsing
└── meta.ml          - HTML meta helpers
```

### Step 2: Level 1 - Static/Vite
```
├── vite.ml          - Dev server proxy
└── assets.ml        - Asset URL resolution
```

### Step 3: Level 2 - Hydration
```
├── hydrate.ml       - HTML shell generation
└── data.ml          - Initial data injection
```

### Step 4: Level 3 - Full SSR
```
├── ssr.ml           - SSR engine
├── worker.ml        - Generic worker interface
├── node_worker.ml   - Node.js implementation
└── streaming.ml     - React 18 streaming SSR
```

---

## React 18 Streaming SSR

```ocaml
(* Streaming SSR with Suspense *)
let handler req =
  let url = Kirin.Request.path req in
  Kirin.React.Ssr.stream ssr_pool ~url (fun stream ->
    (* React sends chunks as Suspense resolves *)
    Kirin.Response.stream stream
  )
```

---

## 예상 코드량

| 파일 | LOC |
|------|-----|
| kirin_react.ml | ~50 |
| manifest.ml | ~80 |
| meta.ml | ~60 |
| vite.ml | ~150 |
| assets.ml | ~80 |
| hydrate.ml | ~120 |
| data.ml | ~60 |
| ssr.ml | ~200 |
| worker.ml | ~100 |
| node_worker.ml | ~180 |
| streaming.ml | ~150 |
| **Total** | **~1,230** |

---

## 테스트 계획

| Category | Tests |
|----------|-------|
| Manifest parsing | 6 |
| Meta tag generation | 5 |
| Vite proxy | 4 |
| Hydration HTML | 8 |
| Data serialization | 5 |
| Worker pool | 6 |
| SSR render | 8 |
| Streaming | 6 |
| **Total** | **~48** |

---

## Dependencies

```
기존:
- yojson (JSON)
- eio (async I/O)
- cohttp-eio (HTTP proxy)

새로 필요:
- 없음 (Node.js는 subprocess로 호출)
```

---

## Example: Full Stack Setup

```
my-app/
├── kirin/
│   └── main.ml           # Kirin server
├── react/
│   ├── src/
│   │   ├── App.tsx
│   │   ├── entry-client.tsx   # Client entry
│   │   └── entry-server.tsx   # SSR entry
│   ├── vite.config.ts
│   └── package.json
└── dune-project
```

```ocaml
(* main.ml *)
let () =
  let ssr = Kirin.React.Ssr.create ~bundle:"react/dist/server/entry-server.js" () in
  
  let routes = Kirin.router [
    (* API routes *)
    Kirin.get "/api/users" api_users;
    
    (* React SSR for everything else *)
    Kirin.get "/*" (fun req ->
      let! html = Kirin.React.Ssr.render ssr ~url:(Kirin.Request.path req) in
      Kirin.html html
    );
  ] in
  
  Kirin.run ~port:3000 routes
```

---

## Checklist

- [ ] lib/react/dune
- [ ] lib/react/kirin_react.ml
- [ ] lib/react/manifest.ml
- [ ] lib/react/meta.ml
- [ ] lib/react/vite.ml
- [ ] lib/react/assets.ml
- [ ] lib/react/hydrate.ml
- [ ] lib/react/data.ml
- [ ] lib/react/ssr.ml
- [ ] lib/react/worker.ml
- [ ] lib/react/node_worker.ml
- [ ] lib/react/streaming.ml
- [ ] test/test_react.ml
- [ ] examples/react_app/
- [ ] dune build 성공
- [ ] dune runtest 성공
