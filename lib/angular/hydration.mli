type hydration_mode = Full | Incremental | Partial | None
type trigger =
    OnIdle
  | OnImmediate
  | OnInteraction
  | OnViewport
  | OnTimer of int
  | OnHover
  | Never
type options = {
  mode : hydration_mode;
  trigger : trigger;
  skip_hydration_ids : string list;
  preserve_whitespace : bool;
  enable_debug : bool;
}
val default_options : options
val incremental_options : options
val boundary_start : id:string -> ?trigger:trigger -> unit -> string
val boundary_end : id:string -> unit -> string
val with_boundary : id:string -> ?trigger:trigger -> string -> string
val skip_hydration_attr : string
val skip_hydration : unit -> string
val with_skip_hydration : string -> string -> string
type component_state =
    Dehydrated
  | Hydrating
  | Hydrated
  | Skipped
  | Error of string
type context = {
  mutable components : (string * component_state) list;
  options : options;
}
val create_context : ?options:options -> unit -> context
val register_component : context -> id:string -> component_state -> unit
val get_component_state : context -> string -> component_state option
val mark_hydrated : context -> string -> unit
val mark_skipped : context -> string -> unit
type mismatch =
    TextMismatch of { expected : string; actual : string; }
  | NodeMismatch of { expected_tag : string; actual_tag : string; }
  | AttributeMismatch of { attr : string; expected : string; actual : string;
    }
  | MissingNode of string
  | ExtraNode of string
val mismatch_to_string : mismatch -> string
val trigger_to_string : trigger -> string
val mode_to_string : hydration_mode -> string
val options_to_json :
  options ->
  [> `Assoc of
       (string *
        [> `Bool of bool
         | `List of [> `String of string ] list
         | `String of string ])
       list ]
val context_to_json :
  context ->
  [> `Assoc of
       (string *
        [> `Assoc of
             (string *
              [> `Bool of bool
               | `List of [> `String of string ] list
               | `String of string ])
             list ])
       list ]
