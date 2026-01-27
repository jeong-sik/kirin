# Kirin + Relay Example (ReScript)

This example demonstrates how to build a **Relay-compliant GraphQL Server** with Kirin and connect it to a **ReScript Relay** frontend.

## 1. Backend (Kirin)

The backend implements the Relay Server Specification:
- **Global Object Identification**: `to_global_id` / `from_global_id`
- **Connections**: Cursor-based pagination (`first`, `after`, `pageInfo`)
- **Node Interface**: (Partially supported, experimental)

### Run Server

```bash
cd kirin
dune exec ./main.exe
# Server running at http://localhost:9000
# GraphQL Endpoint: http://localhost:9000/graphql
# Playground: http://localhost:9000/graphql
```

## 2. Frontend (ReScript + Relay)

To complete the full stack, set up a ReScript project:

### Setup

```bash
npm create rescript-app@latest frontend
cd frontend
npm install rescript-relay relay-runtime react-relay graphql
```

### `relay.config.js`

```javascript
module.exports = {
  src: "./src",
  schema: "./schema.graphql",
  artifactDirectory: "./src/__generated__",
};
```

### Download Schema

With the server running:

```bash
npx get-graphql-schema http://localhost:9000/graphql > schema.graphql
```

### ReScript Component

```rescript
/* src/App.res */
module Query = %relay(`
  query AppQuery($first: Int!, $after: String) {
    users(first: $first, after: $after) {
      edges {
        node {
          id
          name
        }
      }
      pageInfo {
        hasNextPage
        endCursor
      }
    }
  }
`)

@react.component
let make = () => {
  let query = Query.use(~variables={first: 2, after: None}, ())
  
  <div>
    {query.users.edges
      ->Belt.Array.map(edge => <div key=edge.node.id> {React.string(edge.node.name)} </div>)
      ->React.array}
  </div>
}
```

This completes the **OCaml Sandwich**:
- **Backend**: OCaml (Kirin)
- **Frontend**: OCaml Semantics (ReScript)
- **Protocol**: Type-safe GraphQL Relay
