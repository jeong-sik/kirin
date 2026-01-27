module Query = %relay(`
  query AppQuery($first: Int!) {
    users(first: $first) {
      edges {
        node {
          id
          name
        }
      }
    }
  }
`)

@react.component
let make = () => {
  let query = Query.use(~variables={first: 10}, ())

  <div className="p-8">
    <h1 className="text-2xl font-bold mb-4"> {React.string("Kirin + Relay + ReScript")} </h1>
    <ul className="space-y-2">
      {query.users.edges
      ->Belt.Array.map(edge =>
        <li key=edge.node.id className="p-4 bg-gray-100 rounded shadow">
          {React.string(edge.node.name)}
        </li>
      )
      ->React.array}
    </ul>
  </div>
}
