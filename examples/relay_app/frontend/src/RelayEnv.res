open RescriptRelay

let fetchQuery: Network.fetchFunctionPromise = (operation, variables, _cacheConfig, _uploadables) => {
  open Fetch
  fetch(
    "/graphql",
    {
      method: #POST,
      body: {
        "query": operation.text,
        "variables": variables,
      }
      ->Js.Json.serializeExn
      ->Body.string,
      headers: Headers.fromPairs([("content-type", "application/json")]),
    },
  )->Js.Promise.then_(resp => resp->Response.json, _)
}

let network = Network.makePromise(~fetchFunction=fetchQuery, ())
let source = RecordSource.make()
let store = Store.make(~source, ())

let environment = Environment.make(~network, ~store, ())
