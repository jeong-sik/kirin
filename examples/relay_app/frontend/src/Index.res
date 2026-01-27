switch ReactDOM.querySelector("#root") {
| Some(rootElement) =>
  let root = ReactDOM.Client.createRoot(rootElement)
  ReactDOM.Client.Root.render(
    root,
    <React.Suspense fallback={<div> {React.string("Loading...")} </div>}>
      <RescriptRelay.Context.Provider environment=RelayEnv.environment>
        <App />
      </RescriptRelay.Context.Provider>
    </React.Suspense>,
  )
| None => ()
}
