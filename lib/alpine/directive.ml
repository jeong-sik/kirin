(** Alpine.js Directives

    Alpine.js uses HTML attributes as directives for reactive behavior.
    Supports x-data, x-bind, x-on, x-show, x-if, x-for, etc. *)

(** {1 Directive Types} *)

(** Directive modifier *)
type modifier =
  | Prevent
  | Stop
  | Outside
  | Window
  | Document
  | Once
  | Debounce of int
  | Throttle of int
  | Self
  | Camel
  | Dot
  | Passive
  | Capture

(** Event directive *)
type event_directive = {
  event_name: string;
  handler: string;
  modifiers: modifier list;
}

(** Bind directive *)
type bind_directive = {
  attribute: string;
  expression: string;
}

(** For directive *)
type for_directive = {
  item: string;
  index: string option;
  items: string;
}

(** Directive *)
type t =
  | Data of string
  | Init of string
  | Show of string
  | If of string
  | Bind of bind_directive
  | On of event_directive
  | Text of string
  | Html of string
  | Model of string
  | ModelNumber of string
  | ModelDebounce of string * int
  | For of for_directive
  | Transition of transition_config option
  | Effect of string
  | Ref of string
  | Cloak
  | Ignore
  | Id of string
  | Teleport of string

(** Transition configuration *)
and transition_config = {
  enter: string option;
  enter_start: string option;
  enter_end: string option;
  leave: string option;
  leave_start: string option;
  leave_end: string option;
}

(** {1 Directive Creation} *)

(** Create x-data directive *)
let data expr = Data expr

(** Create x-init directive *)
let init expr = Init expr

(** Create x-show directive *)
let show expr = Show expr

(** Create x-if directive *)
let if_ expr = If expr

(** Create x-bind directive *)
let bind ~attr expr = Bind { attribute = attr; expression = expr }

(** Create x-on directive *)
let on ?(modifiers = []) event handler =
  On { event_name = event; handler; modifiers }

(** Create x-text directive *)
let text expr = Text expr

(** Create x-html directive *)
let html expr = Html expr

(** Create x-model directive *)
let model expr = Model expr

(** Create x-model.number directive *)
let model_number expr = ModelNumber expr

(** Create x-model.debounce directive *)
let model_debounce ~ms expr = ModelDebounce (expr, ms)

(** Create x-for directive *)
let for_ ~item ?index ~items () = For { item; index; items }

(** Create x-transition directive *)
let transition ?config () = Transition config

(** Create transition config *)
let transition_config ?enter ?enter_start ?enter_end ?leave ?leave_start ?leave_end () =
  { enter; enter_start; enter_end; leave; leave_start; leave_end }

(** Create x-effect directive *)
let effect_ expr = Effect expr

(** Create x-ref directive *)
let ref_ name = Ref name

(** Create x-cloak directive *)
let cloak = Cloak

(** Create x-ignore directive *)
let ignore = Ignore

(** Create x-id directive *)
let id names = Id names

(** Create x-teleport directive *)
let teleport selector = Teleport selector

(** {1 Modifier Helpers} *)

(** Parse modifier from string *)
let modifier_of_string = function
  | "prevent" -> Some Prevent
  | "stop" -> Some Stop
  | "outside" -> Some Outside
  | "window" -> Some Window
  | "document" -> Some Document
  | "once" -> Some Once
  | "self" -> Some Self
  | "camel" -> Some Camel
  | "dot" -> Some Dot
  | "passive" -> Some Passive
  | "capture" -> Some Capture
  | s when String.length s > 8 && String.sub s 0 8 = "debounce" ->
    let ms = try int_of_string (String.sub s 8 (String.length s - 8)) with _ -> 250 in
    Some (Debounce ms)
  | s when String.length s > 8 && String.sub s 0 8 = "throttle" ->
    let ms = try int_of_string (String.sub s 8 (String.length s - 8)) with _ -> 250 in
    Some (Throttle ms)
  | _ -> None

(** Modifier to string *)
let modifier_to_string = function
  | Prevent -> "prevent"
  | Stop -> "stop"
  | Outside -> "outside"
  | Window -> "window"
  | Document -> "document"
  | Once -> "once"
  | Debounce ms -> Printf.sprintf "debounce.%dms" ms
  | Throttle ms -> Printf.sprintf "throttle.%dms" ms
  | Self -> "self"
  | Camel -> "camel"
  | Dot -> "dot"
  | Passive -> "passive"
  | Capture -> "capture"

(** {1 Rendering} *)

(** Render directive to HTML attribute *)
let to_attribute = function
  | Data expr -> Printf.sprintf "x-data=\"%s\"" expr
  | Init expr -> Printf.sprintf "x-init=\"%s\"" expr
  | Show expr -> Printf.sprintf "x-show=\"%s\"" expr
  | If expr -> Printf.sprintf "x-if=\"%s\"" expr
  | Bind { attribute; expression } ->
    Printf.sprintf "x-bind:%s=\"%s\"" attribute expression
  | On { event_name; handler; modifiers } ->
    let mods = match modifiers with
      | [] -> ""
      | ms -> "." ^ (String.concat "." (List.map modifier_to_string ms))
    in
    Printf.sprintf "x-on:%s%s=\"%s\"" event_name mods handler
  | Text expr -> Printf.sprintf "x-text=\"%s\"" expr
  | Html expr -> Printf.sprintf "x-html=\"%s\"" expr
  | Model expr -> Printf.sprintf "x-model=\"%s\"" expr
  | ModelNumber expr -> Printf.sprintf "x-model.number=\"%s\"" expr
  | ModelDebounce (expr, ms) -> Printf.sprintf "x-model.debounce.%dms=\"%s\"" ms expr
  | For { item; index; items } ->
    let iter = match index with
      | Some i -> Printf.sprintf "(%s, %s)" item i
      | None -> item
    in
    Printf.sprintf "x-for=\"%s in %s\"" iter items
  | Transition None -> "x-transition"
  | Transition (Some config) ->
    let parts = [
      Option.map (fun v -> Printf.sprintf "x-transition:enter=\"%s\"" v) config.enter;
      Option.map (fun v -> Printf.sprintf "x-transition:enter-start=\"%s\"" v) config.enter_start;
      Option.map (fun v -> Printf.sprintf "x-transition:enter-end=\"%s\"" v) config.enter_end;
      Option.map (fun v -> Printf.sprintf "x-transition:leave=\"%s\"" v) config.leave;
      Option.map (fun v -> Printf.sprintf "x-transition:leave-start=\"%s\"" v) config.leave_start;
      Option.map (fun v -> Printf.sprintf "x-transition:leave-end=\"%s\"" v) config.leave_end;
    ] |> List.filter_map Fun.id in
    String.concat " " parts
  | Effect expr -> Printf.sprintf "x-effect=\"%s\"" expr
  | Ref name -> Printf.sprintf "x-ref=\"%s\"" name
  | Cloak -> "x-cloak"
  | Ignore -> "x-ignore"
  | Id names -> Printf.sprintf "x-id=\"%s\"" names
  | Teleport selector -> Printf.sprintf "x-teleport=\"%s\"" selector

(** Render shorthand syntax *)
let to_shorthand = function
  | Bind { attribute; expression } ->
    Printf.sprintf ":%s=\"%s\"" attribute expression
  | On { event_name; handler; modifiers } ->
    let mods = match modifiers with
      | [] -> ""
      | ms -> "." ^ (String.concat "." (List.map modifier_to_string ms))
    in
    Printf.sprintf "@%s%s=\"%s\"" event_name mods handler
  | d -> to_attribute d

(** {1 Serialization} *)

(** Modifier to JSON *)
let modifier_to_json m =
  `String (modifier_to_string m)

(** Directive to JSON *)
let to_json = function
  | Data expr -> `Assoc [("type", `String "data"); ("expression", `String expr)]
  | Init expr -> `Assoc [("type", `String "init"); ("expression", `String expr)]
  | Show expr -> `Assoc [("type", `String "show"); ("expression", `String expr)]
  | If expr -> `Assoc [("type", `String "if"); ("expression", `String expr)]
  | Bind { attribute; expression } ->
    `Assoc [("type", `String "bind"); ("attribute", `String attribute); ("expression", `String expression)]
  | On { event_name; handler; modifiers } ->
    `Assoc [
      ("type", `String "on");
      ("event", `String event_name);
      ("handler", `String handler);
      ("modifiers", `List (List.map modifier_to_json modifiers));
    ]
  | Text expr -> `Assoc [("type", `String "text"); ("expression", `String expr)]
  | Html expr -> `Assoc [("type", `String "html"); ("expression", `String expr)]
  | Model expr -> `Assoc [("type", `String "model"); ("expression", `String expr)]
  | ModelNumber expr -> `Assoc [("type", `String "model.number"); ("expression", `String expr)]
  | ModelDebounce (expr, ms) ->
    `Assoc [("type", `String "model.debounce"); ("expression", `String expr); ("ms", `Int ms)]
  | For { item; index; items } ->
    `Assoc [
      ("type", `String "for");
      ("item", `String item);
      ("index", match index with Some i -> `String i | None -> `Null);
      ("items", `String items);
    ]
  | Transition _ -> `Assoc [("type", `String "transition")]
  | Effect expr -> `Assoc [("type", `String "effect"); ("expression", `String expr)]
  | Ref name -> `Assoc [("type", `String "ref"); ("name", `String name)]
  | Cloak -> `Assoc [("type", `String "cloak")]
  | Ignore -> `Assoc [("type", `String "ignore")]
  | Id names -> `Assoc [("type", `String "id"); ("names", `String names)]
  | Teleport selector -> `Assoc [("type", `String "teleport"); ("selector", `String selector)]
