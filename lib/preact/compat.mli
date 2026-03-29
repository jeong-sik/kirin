type config =
  { enabled : bool
  ; strict_mode : bool
  ; legacy_context : bool
  ; hooks_enabled : bool
  }

val default_config : config

type alias =
  { from : string
  ; to_ : string
  }

val react_alias : alias
val react_dom_alias : alias
val react_dom_client_alias : alias
val test_utils_alias : alias
val jsx_runtime_alias : alias
val standard_aliases : alias list
val vite_aliases : unit -> string
val webpack_aliases : unit -> string
val esbuild_aliases : unit -> string

type compat_level =
  | Full
  | Partial
  | Aliased
  | Manual

type library_compat =
  { name : string
  ; level : compat_level
  ; notes : string option
  }

val library_compatibility : library_compat list
val find_compat : string -> library_compat option
val level_to_string : compat_level -> string

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

val hook_to_string : hook -> string
val hook_import : hook list -> string

type migration_note =
  { pattern : string
  ; replacement : string
  ; description : string
  }

val migration_patterns : migration_note list
val config_to_json : config -> [> `Assoc of (string * [> `Bool of bool ]) list ]
val alias_to_json : alias -> [> `Assoc of (string * [> `String of string ]) list ]

val library_compat_to_json
  :  library_compat
  -> [> `Assoc of (string * [> `Null | `String of string ]) list ]
