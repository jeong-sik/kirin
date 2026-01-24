(** Lit Custom Elements

    Define and manage Web Components (Custom Elements) for Lit.
    Supports reactive properties, attributes, and lifecycle. *)

(** {1 Property Types} *)

(** Property type for Lit elements *)
type property_type =
  | String
  | Number
  | Boolean
  | Object
  | Array

(** Property definition *)
type property = {
  prop_name: string;
  prop_type: property_type;
  prop_attribute: string option;
  prop_reflect: bool;
  prop_state: bool;
  prop_default: Yojson.Safe.t option;
}

(** {1 Element Definition} *)

(** Custom element definition *)
type t = {
  tag_name: string;
  class_name: string;
  properties: property list;
  styles: string list;
  template: string;
  shadow_mode: [`Open | `Closed];
}

(** {1 Property Helpers} *)

(** Create string property *)
let string_prop ?(attribute = true) ?(reflect = false) ?(default = None) name =
  {
    prop_name = name;
    prop_type = String;
    prop_attribute = if attribute then Some name else None;
    prop_reflect = reflect;
    prop_state = false;
    prop_default = Option.map (fun s -> `String s) default;
  }

(** Create number property *)
let number_prop ?(attribute = true) ?(reflect = false) ?(default = None) name =
  {
    prop_name = name;
    prop_type = Number;
    prop_attribute = if attribute then Some name else None;
    prop_reflect = reflect;
    prop_state = false;
    prop_default = Option.map (fun n -> `Float n) default;
  }

(** Create boolean property *)
let boolean_prop ?(attribute = true) ?(reflect = false) ?(default = None) name =
  {
    prop_name = name;
    prop_type = Boolean;
    prop_attribute = if attribute then Some name else None;
    prop_reflect = reflect;
    prop_state = false;
    prop_default = Option.map (fun b -> `Bool b) default;
  }

(** Create object property *)
let object_prop ?(default = None) name =
  {
    prop_name = name;
    prop_type = Object;
    prop_attribute = None;
    prop_reflect = false;
    prop_state = false;
    prop_default = default;
  }

(** Create array property *)
let array_prop ?(default = None) name =
  {
    prop_name = name;
    prop_type = Array;
    prop_attribute = None;
    prop_reflect = false;
    prop_state = false;
    prop_default = default;
  }

(** Create state (internal) property *)
let state_prop prop_type name =
  {
    prop_name = name;
    prop_type;
    prop_attribute = None;
    prop_reflect = false;
    prop_state = true;
    prop_default = None;
  }

(** {1 Element Creation} *)

(** Create element definition *)
let create ~tag_name ~class_name ?(properties = []) ?(styles = []) ?(shadow_mode = `Open) template =
  { tag_name; class_name; properties; styles; template; shadow_mode }

(** Validate tag name (must contain hyphen) *)
let is_valid_tag_name name =
  String.contains name '-' &&
  String.length name >= 3 &&
  not (String.sub name 0 1 >= "0" && String.sub name 0 1 <= "9")

(** {1 Code Generation} *)

(** Property type to TypeScript *)
let property_type_to_ts = function
  | String -> "String"
  | Number -> "Number"
  | Boolean -> "Boolean"
  | Object -> "Object"
  | Array -> "Array"

(** Generate property decorator *)
let property_decorator prop =
  let type_str = property_type_to_ts prop.prop_type in
  let options = [
    Printf.sprintf "type: %s" type_str;
  ] @ (if prop.prop_reflect then ["reflect: true"] else [])
    @ (if prop.prop_state then ["state: true"] else [])
    @ (match prop.prop_attribute with
       | Some attr when attr <> prop.prop_name ->
         [Printf.sprintf "attribute: '%s'" attr]
       | _ -> [])
  in
  Printf.sprintf "@property({ %s })" (String.concat ", " options)

(** Generate element class *)
let to_typescript elem =
  let props_code = elem.properties |> List.map (fun prop ->
    let decorator = if prop.prop_state
      then "@state()"
      else property_decorator prop
    in
    let default = match prop.prop_default with
      | Some json -> " = " ^ Yojson.Safe.to_string json
      | None -> ""
    in
    Printf.sprintf "  %s\n  %s%s;" decorator prop.prop_name default
  ) |> String.concat "\n\n" in

  let styles_code = match elem.styles with
    | [] -> ""
    | styles ->
      let css = String.concat "\n" styles in
      Printf.sprintf "\n  static styles = css`%s`;\n" css
  in

  Printf.sprintf {|import { LitElement, html, css } from 'lit';
import { customElement, property, state } from 'lit/decorators.js';

@customElement('%s')
export class %s extends LitElement {
%s%s
  render() {
    return html`%s`;
  }
}
|} elem.tag_name elem.class_name styles_code props_code elem.template

(** Generate element registration (no decorators) *)
let to_javascript elem =
  let props_obj = elem.properties
    |> List.filter (fun p -> not p.prop_state)
    |> List.map (fun prop ->
      let type_str = property_type_to_ts prop.prop_type in
      let opts = [Printf.sprintf "type: %s" type_str]
        @ (if prop.prop_reflect then ["reflect: true"] else [])
        @ (match prop.prop_attribute with
           | Some attr when attr <> prop.prop_name ->
             [Printf.sprintf "attribute: '%s'" attr]
           | _ -> [])
      in
      Printf.sprintf "    %s: { %s }" prop.prop_name (String.concat ", " opts)
    )
    |> String.concat ",\n"
  in

  let styles_code = match elem.styles with
    | [] -> ""
    | styles ->
      let css = String.concat "\n" styles in
      Printf.sprintf "\n  static styles = css`%s`;\n" css
  in

  Printf.sprintf {|import { LitElement, html, css } from 'lit';

export class %s extends LitElement {
  static properties = {
%s
  };
%s
  constructor() {
    super();
%s
  }

  render() {
    return html`%s`;
  }
}

customElements.define('%s', %s);
|}
    elem.class_name
    props_obj
    styles_code
    (elem.properties |> List.filter_map (fun p ->
      match p.prop_default with
      | Some json -> Some (Printf.sprintf "    this.%s = %s;" p.prop_name (Yojson.Safe.to_string json))
      | None -> None
    ) |> String.concat "\n")
    elem.template
    elem.tag_name
    elem.class_name

(** {1 Serialization} *)

(** Property type to JSON *)
let property_type_to_json = function
  | String -> `String "string"
  | Number -> `String "number"
  | Boolean -> `String "boolean"
  | Object -> `String "object"
  | Array -> `String "array"

(** Property to JSON *)
let property_to_json prop =
  `Assoc [
    ("name", `String prop.prop_name);
    ("type", property_type_to_json prop.prop_type);
    ("attribute", match prop.prop_attribute with Some a -> `String a | None -> `Null);
    ("reflect", `Bool prop.prop_reflect);
    ("state", `Bool prop.prop_state);
    ("default", match prop.prop_default with Some d -> d | None -> `Null);
  ]

(** Element to JSON *)
let to_json elem =
  `Assoc [
    ("tagName", `String elem.tag_name);
    ("className", `String elem.class_name);
    ("properties", `List (List.map property_to_json elem.properties));
    ("styles", `List (List.map (fun s -> `String s) elem.styles));
    ("template", `String elem.template);
    ("shadowMode", `String (match elem.shadow_mode with `Open -> "open" | `Closed -> "closed"));
  ]
