(** Preact Compat

    React compatibility layer for Preact.
    Enables using React libraries with Preact through preact/compat. *)

(** {1 Compat Configuration} *)

(** Compat mode settings *)
type config = {
  enabled: bool;
  strict_mode: bool;
  legacy_context: bool;
  hooks_enabled: bool;
}

(** Default config *)
let default_config = {
  enabled = true;
  strict_mode = true;
  legacy_context = false;
  hooks_enabled = true;
}

(** {1 Alias Configuration} *)

(** Bundler alias for react -> preact/compat *)
type alias = {
  from: string;
  to_: string;
}

(** Create React alias *)
let react_alias = { from = "react"; to_ = "preact/compat" }

(** Create React DOM alias *)
let react_dom_alias = { from = "react-dom"; to_ = "preact/compat" }

(** Create React DOM client alias *)
let react_dom_client_alias = { from = "react-dom/client"; to_ = "preact/compat/client" }

(** Create test utils alias *)
let test_utils_alias = { from = "react-dom/test-utils"; to_ = "preact/test-utils" }

(** React JSX runtime alias *)
let jsx_runtime_alias = { from = "react/jsx-runtime"; to_ = "preact/jsx-runtime" }

(** All standard aliases *)
let standard_aliases = [
  react_alias;
  react_dom_alias;
  react_dom_client_alias;
  test_utils_alias;
  jsx_runtime_alias;
]

(** {1 Bundler Config Generation} *)

(** Generate Vite alias config *)
let vite_aliases () =
  let alias_entries = List.map (fun a ->
    Printf.sprintf "      '%s': 'preact/compat'" a.from
  ) [react_alias; react_dom_alias] in
  Printf.sprintf {|resolve: {
    alias: {
%s,
      'react/jsx-runtime': 'preact/jsx-runtime',
    },
  },|} (String.concat ",\n" alias_entries)

(** Generate Webpack alias config *)
let webpack_aliases () =
  let alias_entries = List.map (fun a ->
    Printf.sprintf "      '%s': 'preact/compat'" a.from
  ) [react_alias; react_dom_alias] in
  Printf.sprintf {|resolve: {
  alias: {
%s,
    'react/jsx-runtime': 'preact/jsx-runtime',
  },
},|} (String.concat ",\n" alias_entries)

(** Generate esbuild alias config *)
let esbuild_aliases () =
  Printf.sprintf {|alias: {
  'react': 'preact/compat',
  'react-dom': 'preact/compat',
  'react/jsx-runtime': 'preact/jsx-runtime',
}|}

(** {1 Library Compatibility} *)

(** Compatibility level *)
type compat_level =
  | Full      (* Works out of the box *)
  | Partial   (* Some features may not work *)
  | Aliased   (* Works with alias setup *)
  | Manual    (* Requires code changes *)

(** Known library compatibility *)
type library_compat = {
  name: string;
  level: compat_level;
  notes: string option;
}

(** Check library compatibility *)
let library_compatibility = [
  { name = "react-router"; level = Aliased; notes = Some "v6+ works well" };
  { name = "react-query"; level = Aliased; notes = Some "Full compatibility" };
  { name = "@tanstack/react-query"; level = Aliased; notes = Some "Full compatibility" };
  { name = "react-redux"; level = Aliased; notes = Some "Works with aliases" };
  { name = "framer-motion"; level = Partial; notes = Some "Most features work" };
  { name = "react-spring"; level = Aliased; notes = Some "Full compatibility" };
  { name = "zustand"; level = Full; notes = None };
  { name = "jotai"; level = Full; notes = None };
  { name = "swr"; level = Aliased; notes = Some "Full compatibility" };
  { name = "react-hook-form"; level = Aliased; notes = Some "Full compatibility" };
  { name = "@headlessui/react"; level = Aliased; notes = Some "Full compatibility" };
  { name = "react-three-fiber"; level = Partial; notes = Some "Basic features work" };
]

(** Find library compatibility *)
let find_compat lib =
  List.find_opt (fun c -> c.name = lib) library_compatibility

(** Compat level to string *)
let level_to_string = function
  | Full -> "full"
  | Partial -> "partial"
  | Aliased -> "aliased"
  | Manual -> "manual"

(** {1 Hook Compatibility} *)

(** React hooks available in Preact *)
type hook =
  | UseState
  | UseEffect
  | UseContext
  | UseReducer
  | UseCallback
  | UseMemo
  | UseRef
  | UseImperativeHandle
  | UseLayoutEffect
  | UseDebugValue
  | UseDeferredValue
  | UseTransition
  | UseId
  | UseSyncExternalStore

(** Hook to string *)
let hook_to_string = function
  | UseState -> "useState"
  | UseEffect -> "useEffect"
  | UseContext -> "useContext"
  | UseReducer -> "useReducer"
  | UseCallback -> "useCallback"
  | UseMemo -> "useMemo"
  | UseRef -> "useRef"
  | UseImperativeHandle -> "useImperativeHandle"
  | UseLayoutEffect -> "useLayoutEffect"
  | UseDebugValue -> "useDebugValue"
  | UseDeferredValue -> "useDeferredValue"
  | UseTransition -> "useTransition"
  | UseId -> "useId"
  | UseSyncExternalStore -> "useSyncExternalStore"

(** Generate hook import *)
let hook_import hooks =
  let hook_names = List.map hook_to_string hooks |> String.concat ", " in
  Printf.sprintf "import { %s } from 'preact/hooks';" hook_names

(** {1 Migration Helpers} *)

(** Migration note *)
type migration_note = {
  pattern: string;
  replacement: string;
  description: string;
}

(** Common migration patterns *)
let migration_patterns = [
  { pattern = "import React from 'react'";
    replacement = "import { h } from 'preact'";
    description = "Replace default React import" };
  { pattern = "React.createElement";
    replacement = "h";
    description = "Replace createElement with h" };
  { pattern = "import { render } from 'react-dom'";
    replacement = "import { render } from 'preact'";
    description = "Replace ReactDOM render" };
  { pattern = "ReactDOM.render";
    replacement = "render";
    description = "Replace ReactDOM.render call" };
]

(** {1 Serialization} *)

(** Config to JSON *)
let config_to_json c =
  `Assoc [
    ("enabled", `Bool c.enabled);
    ("strictMode", `Bool c.strict_mode);
    ("legacyContext", `Bool c.legacy_context);
    ("hooksEnabled", `Bool c.hooks_enabled);
  ]

(** Alias to JSON *)
let alias_to_json a =
  `Assoc [
    ("from", `String a.from);
    ("to", `String a.to_);
  ]

(** Library compat to JSON *)
let library_compat_to_json c =
  `Assoc [
    ("name", `String c.name);
    ("level", `String (level_to_string c.level));
    ("notes", match c.notes with Some n -> `String n | None -> `Null);
  ]
