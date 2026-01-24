(** Shadow DOM for Lit

    Shadow DOM encapsulation and Declarative Shadow DOM for SSR.
    Supports both open and closed shadow roots. *)

(** {1 Shadow DOM Types} *)

(** Shadow root mode *)
type shadow_mode = Open | Closed

(** Slot definition *)
type slot = {
  slot_name: string option;  (* None = default slot *)
  slot_fallback: string option;
}

(** Shadow root *)
type t = {
  mode: shadow_mode;
  delegates_focus: bool;
  slot_assignment: [`Named | `Manual];
  styles: string list;
  content: string;
  slots: slot list;
}

(** {1 Shadow Root Creation} *)

(** Create shadow root *)
let create ?(mode = Open) ?(delegates_focus = false) ?(slot_assignment = `Named)
    ?(styles = []) ?(slots = []) content =
  { mode; delegates_focus; slot_assignment; styles; content; slots }

(** Create with default slot *)
let with_default_slot ?fallback content =
  create ~slots:[{ slot_name = None; slot_fallback = fallback }] content

(** Create with named slots *)
let with_named_slots slots content =
  let slot_defs = List.map (fun (name, fallback) ->
    { slot_name = Some name; slot_fallback = fallback }
  ) slots in
  create ~slots:slot_defs content

(** {1 Slot Helpers} *)

(** Create default slot *)
let default_slot ?fallback () =
  { slot_name = None; slot_fallback = fallback }

(** Create named slot *)
let named_slot name ?fallback () =
  { slot_name = Some name; slot_fallback = fallback }

(** Render slot element *)
let render_slot slot =
  let name_attr = match slot.slot_name with
    | Some name -> Printf.sprintf " name=\"%s\"" name
    | None -> ""
  in
  let fallback_content = match slot.slot_fallback with
    | Some content -> content
    | None -> ""
  in
  Printf.sprintf "<slot%s>%s</slot>" name_attr fallback_content

(** {1 Declarative Shadow DOM} *)

(** Render Declarative Shadow DOM template for SSR *)
let to_declarative_shadow_dom shadow =
  let mode_str = match shadow.mode with Open -> "open" | Closed -> "closed" in
  let focus_attr = if shadow.delegates_focus then " shadowrootdelegatesfocus" else "" in

  let styles_html = match shadow.styles with
    | [] -> ""
    | styles ->
      let css = String.concat "\n" styles in
      Printf.sprintf "<style>%s</style>" css
  in

  Printf.sprintf {|<template shadowrootmode="%s"%s>%s%s</template>|}
    mode_str focus_attr styles_html shadow.content

(** Render component with Declarative Shadow DOM *)
let render_component ~tag_name ~attributes ?(light_dom = "") shadow =
  let attrs_str = attributes
    |> List.map (fun (k, v) -> Printf.sprintf " %s=\"%s\"" k v)
    |> String.concat ""
  in
  Printf.sprintf "<%s%s>%s%s</%s>"
    tag_name attrs_str (to_declarative_shadow_dom shadow) light_dom tag_name

(** {1 Adopted Stylesheets} *)

(** CSS module type *)
type css_module = {
  css_id: string;
  css_content: string;
}

(** Create CSS module *)
let css_module ~id content =
  { css_id = id; css_content = content }

(** Render adopted stylesheet link (for client) *)
let adopted_stylesheet_script modules =
  let module_code = modules |> List.map (fun m ->
    Printf.sprintf "const %s = new CSSStyleSheet();\n%s.replaceSync(`%s`);"
      m.css_id m.css_id m.css_content
  ) |> String.concat "\n" in

  Printf.sprintf {|<script type="module">
%s
</script>|} module_code

(** {1 Parts API} *)

(** Element part for styling *)
type part = {
  part_name: string;
  part_element: string;
}

(** Create part *)
let part ~name element =
  { part_name = name; part_element = element }

(** Render element with part attribute *)
let render_with_part p =
  Printf.sprintf "<%s part=\"%s\">" p.part_element p.part_name

(** Generate ::part() CSS selectors *)
let part_selector ~host_selector ~part_name =
  Printf.sprintf "%s::part(%s)" host_selector part_name

(** {1 Export Parts} *)

(** Export parts for nested components *)
let exportparts parts =
  parts
  |> List.map (fun (internal, external_name) ->
    match external_name with
    | Some ext -> Printf.sprintf "%s: %s" internal ext
    | None -> internal
  )
  |> String.concat ", "

(** Render exportparts attribute *)
let exportparts_attr parts =
  Printf.sprintf "exportparts=\"%s\"" (exportparts parts)

(** {1 Polyfill Support} *)

(** Check if Declarative Shadow DOM is supported *)
let polyfill_check =
  {|<script>
(function() {
  if (!HTMLTemplateElement.prototype.hasOwnProperty('shadowRootMode')) {
    document.querySelectorAll('template[shadowrootmode]').forEach(template => {
      const mode = template.getAttribute('shadowrootmode');
      const shadowRoot = template.parentElement.attachShadow({ mode });
      shadowRoot.appendChild(template.content);
      template.remove();
    });
  }
})();
</script>|}

(** Include polyfill script *)
let with_polyfill html =
  html ^ "\n" ^ polyfill_check

(** {1 Serialization} *)

(** Shadow mode to JSON *)
let shadow_mode_to_json = function
  | Open -> `String "open"
  | Closed -> `String "closed"

(** Slot to JSON *)
let slot_to_json slot =
  `Assoc [
    ("name", match slot.slot_name with Some n -> `String n | None -> `Null);
    ("fallback", match slot.slot_fallback with Some f -> `String f | None -> `Null);
  ]

(** Shadow root to JSON *)
let to_json shadow =
  `Assoc [
    ("mode", shadow_mode_to_json shadow.mode);
    ("delegatesFocus", `Bool shadow.delegates_focus);
    ("slotAssignment", `String (match shadow.slot_assignment with `Named -> "named" | `Manual -> "manual"));
    ("styles", `List (List.map (fun s -> `String s) shadow.styles));
    ("content", `String shadow.content);
    ("slots", `List (List.map slot_to_json shadow.slots));
  ]
