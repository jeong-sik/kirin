(** Lit Templates

    Tagged template literals and template helpers for Lit.
    Supports expressions, directives, and event bindings. *)

(** {1 Template Parts} *)

(** Template expression *)
type expression =
  | Text of string
  | Property of string
  | Computed of string
  | Conditional of { condition: string; then_: t; else_: t option }
  | Loop of { items: string; item: string; index: string option; body: t }
  | Slot of { name: string option; fallback: string option }
  | Event of { event_name: string; handler: string; options: event_options }
  | Directive of { directive_name: string; args: string list }

(** Event options *)
and event_options = {
  capture: bool;
  once: bool;
  passive: bool;
}

(** Template *)
and t = {
  parts: expression list;
}

(** {1 Template Creation} *)

(** Create empty template *)
let empty = { parts = [] }

(** Create template from parts *)
let create parts = { parts }

(** Create text template *)
let text s = { parts = [Text s] }

(** Create property binding *)
let prop name = { parts = [Property name] }

(** Create computed expression *)
let computed expr = { parts = [Computed expr] }

(** Default event options *)
let default_event_options = { capture = false; once = false; passive = false }

(** Create event binding *)
let event ?(options = default_event_options) event_name handler =
  { parts = [Event { event_name; handler; options }] }

(** Create slot *)
let slot ?name ?fallback () =
  { parts = [Slot { name; fallback }] }

(** Create conditional *)
let when_ condition ~then_ ?else_ () =
  { parts = [Conditional { condition; then_; else_ }] }

(** Create loop *)
let repeat ~items ~item ?index body =
  { parts = [Loop { items; item; index; body }] }

(** Create directive *)
let directive directive_name args =
  { parts = [Directive { directive_name; args }] }

(** Concatenate templates *)
let concat templates =
  { parts = List.concat_map (fun t -> t.parts) templates }

(** {1 Common Directives (Simple)} *)

(** classMap directive *)
let class_map classes =
  let args = classes |> List.map (fun (cls, expr) ->
    Printf.sprintf "'%s': %s" cls expr
  ) |> String.concat ", " in
  directive "classMap" ["{" ^ args ^ "}"]

(** styleMap directive *)
let style_map styles =
  let args = styles |> List.map (fun (prop, value) ->
    Printf.sprintf "'%s': %s" prop value
  ) |> String.concat ", " in
  directive "styleMap" ["{" ^ args ^ "}"]

(** ifDefined directive *)
let if_defined expr =
  directive "ifDefined" [expr]

(** live directive *)
let live expr =
  directive "live" [expr]

(** ref directive *)
let ref_directive name =
  directive "ref" [name]

(** until directive (async) *)
let until promise fallback =
  directive "until" [promise; fallback]

(** {1 Rendering} *)

(** Render expression to Lit template literal *)
let rec render_expression = function
  | Text s -> s
  | Property name -> "${this." ^ name ^ "}"
  | Computed expr -> "${" ^ expr ^ "}"
  | Conditional { condition; then_; else_ } ->
    let then_str = render_to_lit then_ in
    let else_str = match else_ with
      | Some e -> render_to_lit e
      | None -> ""
    in
    Printf.sprintf "${%s ? html`%s` : html`%s`}" condition then_str else_str
  | Loop { items; item; index; body } ->
    let body_str = render_to_lit body in
    let map_args = match index with
      | Some i -> Printf.sprintf "(%s, %s)" item i
      | None -> item
    in
    Printf.sprintf "${this.%s.map(%s => html`%s`)}" items map_args body_str
  | Slot { name; fallback } ->
    let name_attr = match name with
      | Some n -> Printf.sprintf " name=\"%s\"" n
      | None -> ""
    in
    let fallback_content = match fallback with
      | Some f -> f
      | None -> ""
    in
    Printf.sprintf "<slot%s>%s</slot>" name_attr fallback_content
  | Event { event_name; handler; options } ->
    let opts =
      (if options.capture then ["capture"] else []) @
      (if options.once then ["once"] else []) @
      (if options.passive then ["passive"] else [])
    in
    let event_binding = match opts with
      | [] -> Printf.sprintf "@%s" event_name
      | _ -> Printf.sprintf "@%s" event_name  (* Options via addEventListener *)
    in
    Printf.sprintf "%s=${this.%s}" event_binding handler
  | Directive { directive_name; args } ->
    Printf.sprintf "${%s(%s)}" directive_name (String.concat ", " args)

(** Render template to Lit template literal content *)
and render_to_lit template =
  template.parts |> List.map render_expression |> String.concat ""

(** Render full html tagged template *)
let to_html_literal template =
  "html`" ^ render_to_lit template ^ "`"

(** {1 Directives Requiring render_to_lit} *)

(** cache directive *)
let cache template =
  let rendered = render_to_lit template in
  directive "cache" [rendered]

(** guard directive *)
let guard deps template =
  let rendered = render_to_lit template in
  directive "guard" [Printf.sprintf "[%s]" (String.concat ", " deps); rendered]

(** {1 Static Rendering (SSR)} *)

(** Render expression statically *)
let rec render_static_expression ~data = function
  | Text s -> s
  | Property name ->
    (match List.assoc_opt name data with
     | Some (`String s) -> s
     | Some (`Int n) -> string_of_int n
     | Some (`Float f) -> string_of_float f
     | Some (`Bool b) -> string_of_bool b
     | Some json -> Yojson.Safe.to_string json
     | None -> "")
  | Computed _ -> ""  (* Can't evaluate computed expressions statically *)
  | Conditional { condition; then_; else_ } ->
    (* Simple truthy check on property *)
    let is_truthy = match List.assoc_opt condition data with
      | Some (`Bool true) -> true
      | Some (`String s) when s <> "" -> true
      | Some (`Int n) when n <> 0 -> true
      | Some (`List (_::_)) -> true
      | _ -> false
    in
    if is_truthy then render_static ~data then_
    else (match else_ with Some e -> render_static ~data e | None -> "")
  | Loop { items; item; body; _ } ->
    (match List.assoc_opt items data with
     | Some (`List arr) ->
       arr |> List.map (fun item_data ->
         let item_obj = match item_data with
           | `Assoc pairs -> pairs
           | _ -> [(item, item_data)]
         in
         render_static ~data:item_obj body
       ) |> String.concat ""
     | _ -> "")
  | Slot { fallback; _ } ->
    (match fallback with Some f -> f | None -> "")
  | Event _ -> ""  (* Events don't render statically *)
  | Directive _ -> ""  (* Directives need client-side JS *)

(** Render template statically with data *)
and render_static ~data template =
  template.parts |> List.map (render_static_expression ~data) |> String.concat ""

(** {1 Serialization} *)

(** Event options to JSON *)
let event_options_to_json opts =
  `Assoc [
    ("capture", `Bool opts.capture);
    ("once", `Bool opts.once);
    ("passive", `Bool opts.passive);
  ]

(** Expression to JSON *)
let rec expression_to_json = function
  | Text s -> `Assoc [("type", `String "text"); ("value", `String s)]
  | Property name -> `Assoc [("type", `String "property"); ("name", `String name)]
  | Computed expr -> `Assoc [("type", `String "computed"); ("expr", `String expr)]
  | Conditional { condition; then_; else_ } ->
    `Assoc [
      ("type", `String "conditional");
      ("condition", `String condition);
      ("then", to_json then_);
      ("else", match else_ with Some e -> to_json e | None -> `Null);
    ]
  | Loop { items; item; index; body } ->
    `Assoc [
      ("type", `String "loop");
      ("items", `String items);
      ("item", `String item);
      ("index", match index with Some i -> `String i | None -> `Null);
      ("body", to_json body);
    ]
  | Slot { name; fallback } ->
    `Assoc [
      ("type", `String "slot");
      ("name", match name with Some n -> `String n | None -> `Null);
      ("fallback", match fallback with Some f -> `String f | None -> `Null);
    ]
  | Event { event_name; handler; options } ->
    `Assoc [
      ("type", `String "event");
      ("event", `String event_name);
      ("handler", `String handler);
      ("options", event_options_to_json options);
    ]
  | Directive { directive_name; args } ->
    `Assoc [
      ("type", `String "directive");
      ("name", `String directive_name);
      ("args", `List (List.map (fun a -> `String a) args));
    ]

(** Template to JSON *)
and to_json template =
  `Assoc [
    ("parts", `List (List.map expression_to_json template.parts));
  ]
