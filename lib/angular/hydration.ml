(** Angular Hydration

    Angular hydration support with incremental hydration (v19+). *)

(** {1 Hydration Modes} *)

(** Hydration mode *)
type hydration_mode =
  | Full               (* Full hydration - reattach all event listeners *)
  | Incremental        (* Incremental hydration - hydrate on demand *)
  | Partial            (* Partial hydration - skip some components *)
  | None               (* No hydration - pure SSR *)

(** Incremental trigger *)
type trigger =
  | OnIdle             (* Hydrate when browser is idle *)
  | OnImmediate        (* Hydrate immediately *)
  | OnInteraction      (* Hydrate on user interaction *)
  | OnViewport         (* Hydrate when in viewport *)
  | OnTimer of int     (* Hydrate after N milliseconds *)
  | OnHover            (* Hydrate on hover *)
  | Never              (* Never hydrate (static) *)

(** {1 Hydration Configuration} *)

(** Hydration options *)
type options = {
  mode: hydration_mode;
  trigger: trigger;
  skip_hydration_ids: string list;
  preserve_whitespace: bool;
  enable_debug: bool;
}

(** Default options *)
let default_options = {
  mode = Full;
  trigger = OnIdle;
  skip_hydration_ids = [];
  preserve_whitespace = false;
  enable_debug = false;
}

(** Incremental hydration options *)
let incremental_options = {
  default_options with
  mode = Incremental;
  trigger = OnViewport;
}

(** {1 Hydration Markers} *)

(** Generate hydration boundary marker *)
let boundary_start ~id ?(trigger=OnIdle) () =
  let trigger_attr = match trigger with
    | OnIdle -> "idle"
    | OnImmediate -> "immediate"
    | OnInteraction -> "interaction"
    | OnViewport -> "viewport"
    | OnTimer ms -> Printf.sprintf "timer(%d)" ms
    | OnHover -> "hover"
    | Never -> "never"
  in
  Printf.sprintf {|<!--nghb:%s:%s-->|} id trigger_attr

(** Generate hydration boundary end marker *)
let boundary_end ~id () =
  Printf.sprintf {|<!--/nghb:%s-->|} id

(** Wrap content with hydration boundary *)
let with_boundary ~id ?(trigger=OnIdle) content =
  let start = boundary_start ~id ~trigger () in
  let end_ = boundary_end ~id () in
  start ^ content ^ end_

(** {1 Skip Hydration} *)

(** Skip hydration marker *)
let skip_hydration_attr = "ngSkipHydration"

(** Generate skip hydration attribute *)
let skip_hydration () = skip_hydration_attr

(** Wrap with skip hydration *)
let with_skip_hydration tag content =
  Printf.sprintf "<%s %s>%s</%s>" tag skip_hydration_attr content tag

(** {1 Hydration State} *)

(** Component hydration state *)
type component_state =
  | Dehydrated           (* Not yet hydrated *)
  | Hydrating            (* Currently hydrating *)
  | Hydrated             (* Fully hydrated *)
  | Skipped              (* Skipped hydration *)
  | Error of string      (* Hydration error *)

(** Hydration context *)
type context = {
  mutable components: (string * component_state) list;
  options: options;
}

(** Create hydration context *)
let create_context ?(options=default_options) () = {
  components = [];
  options;
}

(** Register component for hydration *)
let register_component ctx ~id state =
  ctx.components <- (id, state) :: List.filter (fun (k, _) -> k <> id) ctx.components

(** Get component state *)
let get_component_state ctx id =
  List.assoc_opt id ctx.components

(** Mark component as hydrated *)
let mark_hydrated ctx id =
  register_component ctx ~id Hydrated

(** Mark component as skipped *)
let mark_skipped ctx id =
  register_component ctx ~id Skipped

(** {1 Mismatch Detection} *)

(** Mismatch type *)
type mismatch =
  | TextMismatch of { expected: string; actual: string }
  | NodeMismatch of { expected_tag: string; actual_tag: string }
  | AttributeMismatch of { attr: string; expected: string; actual: string }
  | MissingNode of string
  | ExtraNode of string

(** Mismatch to string *)
let mismatch_to_string = function
  | TextMismatch { expected; actual } ->
    Printf.sprintf "Text mismatch: expected '%s', got '%s'" expected actual
  | NodeMismatch { expected_tag; actual_tag } ->
    Printf.sprintf "Node mismatch: expected <%s>, got <%s>" expected_tag actual_tag
  | AttributeMismatch { attr; expected; actual } ->
    Printf.sprintf "Attribute '%s' mismatch: expected '%s', got '%s'" attr expected actual
  | MissingNode tag ->
    Printf.sprintf "Missing node: <%s>" tag
  | ExtraNode tag ->
    Printf.sprintf "Extra node: <%s>" tag

(** {1 Serialization} *)

(** Trigger to string *)
let trigger_to_string = function
  | OnIdle -> "idle"
  | OnImmediate -> "immediate"
  | OnInteraction -> "interaction"
  | OnViewport -> "viewport"
  | OnTimer ms -> Printf.sprintf "timer(%d)" ms
  | OnHover -> "hover"
  | Never -> "never"

(** Mode to string *)
let mode_to_string = function
  | Full -> "full"
  | Incremental -> "incremental"
  | Partial -> "partial"
  | None -> "none"

(** Options to JSON *)
let options_to_json opts =
  `Assoc [
    ("mode", `String (mode_to_string opts.mode));
    ("trigger", `String (trigger_to_string opts.trigger));
    ("skipHydrationIds", `List (List.map (fun s -> `String s) opts.skip_hydration_ids));
    ("preserveWhitespace", `Bool opts.preserve_whitespace);
    ("enableDebug", `Bool opts.enable_debug);
  ]

(** Context to JSON *)
let context_to_json ctx =
  let state_to_string = function
    | Dehydrated -> "dehydrated"
    | Hydrating -> "hydrating"
    | Hydrated -> "hydrated"
    | Skipped -> "skipped"
    | Error msg -> "error:" ^ msg
  in
  `Assoc [
    ("components", `Assoc (List.map (fun (id, state) ->
      (id, `String (state_to_string state))
    ) ctx.components));
    ("options", options_to_json ctx.options);
  ]
