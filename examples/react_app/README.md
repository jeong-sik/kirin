# Kirin + React Example

This example demonstrates the three levels of React integration with Kirin.

## Integration Levels

| Level | Mode | Description |
|-------|------|-------------|
| 1 | `static` | Vite build served by Kirin (SPA mode) |
| 2 | `hydration` | Server HTML shell + client hydration (SEO-friendly) |
| 3 | `ssr` | Full server-side rendering with Node.js workers |

## Quick Start

### 1. Build the React app

```bash
cd react
npm install
npm run build        # Level 1 & 2
npm run build:ssr    # Level 3 (also builds SSR entry)
```

### 2. Run Kirin server

```bash
cd kirin

# Level 1: Static serving
REACT_MODE=static dune exec ./main.exe

# Level 2: Hydration with initial data
REACT_MODE=hydration dune exec ./main.exe

# Level 3: Full SSR
REACT_MODE=ssr dune exec ./main.exe
```

### 3. Open browser

http://localhost:3000

## Development Mode

For hot module replacement during development:

```bash
# Terminal 1: Vite dev server
cd react && npm run dev

# Terminal 2: Kirin API server
cd kirin && REACT_MODE=static dune exec ./main.exe
```

Then open http://localhost:5173 (Vite proxies /api to Kirin)

## Architecture

```
┌─────────────────────────────────────────────┐
│                  Browser                     │
├─────────────────────────────────────────────┤
│                React App                     │
│  (entry-client.tsx)                         │
├─────────────────────────────────────────────┤
│                                             │
│  Level 1: Static    Level 2: Hydration      │
│  ┌─────────┐        ┌─────────┐             │
│  │  Vite   │        │  HTML   │             │
│  │  dist/  │        │  Shell  │             │
│  └─────────┘        └─────────┘             │
│       │                  │                   │
└───────┼──────────────────┼───────────────────┘
        │                  │
┌───────┴──────────────────┴───────────────────┐
│              Kirin Server (OCaml)             │
│                                               │
│  Level 3: SSR                                │
│  ┌─────────────────────────────────────┐    │
│  │  Node.js Worker Pool                 │    │
│  │  (entry-server.tsx via JSON-RPC)     │    │
│  └─────────────────────────────────────┘    │
└───────────────────────────────────────────────┘
```

## Files

```
react_app/
├── kirin/
│   ├── dune            # OCaml build config
│   └── main.ml         # Kirin server with 3 modes
│
├── react/
│   ├── src/
│   │   ├── App.tsx           # Main React component
│   │   ├── entry-client.tsx  # Client hydration entry
│   │   ├── entry-server.tsx  # SSR worker (Level 3)
│   │   └── index.css         # Styles
│   │
│   ├── index.html      # HTML template
│   ├── vite.config.ts  # Vite configuration
│   ├── tsconfig.json   # TypeScript config
│   └── package.json    # Dependencies
│
└── README.md           # This file
```

## API Endpoints

- `GET /api/users` - List all users
- `GET /api/users/:id` - Get user by ID

## How It Works

### Level 1: Static
1. Vite builds React app to `dist/`
2. Kirin serves static files from `dist/`
3. React renders entirely on client

### Level 2: Hydration
1. Kirin generates HTML shell with `__INITIAL_DATA__`
2. Browser receives pre-rendered structure
3. React hydrates and adds interactivity

### Level 3: Full SSR
1. Kirin spawns Node.js worker pool
2. Each request sends JSON-RPC to worker
3. Worker calls `renderToString(<App />)`
4. Kirin returns fully-rendered HTML
5. Client hydrates for interactivity

## Performance Notes

- **Level 1**: Fastest dev experience, slowest TTFB
- **Level 2**: Good SEO, moderate TTFB
- **Level 3**: Best SEO & TTFB, requires Node.js runtime

Worker pool configuration (Level 3):
- 2 workers by default
- 5 second timeout per render
- Auto-restart after 5000 requests (memory leak prevention)
