(** Lit/Web Components Tests *)

open Alcotest

(** Helper function *)
let contains haystack needle =
  try
    let _ = Str.search_forward (Str.regexp_string needle) haystack 0 in
    true
  with Not_found -> false

(** {1 Element Tests} *)

let test_element_create () =
  let elem = Kirin_lit.Element.create
    ~tag_name:"my-element"
    ~class_name:"MyElement"
    "Hello World"
  in
  check string "tag_name" "my-element" elem.tag_name;
  check string "class_name" "MyElement" elem.class_name;
  check string "template" "Hello World" elem.template

let test_element_valid_tag_name () =
  check bool "valid hyphenated" true (Kirin_lit.Element.is_valid_tag_name "my-element");
  check bool "valid multi-hyphen" true (Kirin_lit.Element.is_valid_tag_name "my-custom-element");
  check bool "invalid no hyphen" false (Kirin_lit.Element.is_valid_tag_name "element");
  check bool "invalid too short" false (Kirin_lit.Element.is_valid_tag_name "a-")

let test_element_string_prop () =
  let prop = Kirin_lit.Element.string_prop ~reflect:true ~default:(Some "hello") "name" in
  check string "prop_name" "name" prop.prop_name;
  check bool "reflect" true prop.prop_reflect;
  check bool "has default" true (Option.is_some prop.prop_default)

let test_element_number_prop () =
  let prop = Kirin_lit.Element.number_prop ~default:(Some 42.0) "count" in
  check string "prop_name" "count" prop.prop_name;
  check bool "has default" true (Option.is_some prop.prop_default)

let test_element_boolean_prop () =
  let prop = Kirin_lit.Element.boolean_prop ~reflect:true "active" in
  check string "prop_name" "active" prop.prop_name;
  check bool "reflect" true prop.prop_reflect

let test_element_state_prop () =
  let prop = Kirin_lit.Element.state_prop Kirin_lit.Element.String "internal" in
  check string "prop_name" "internal" prop.prop_name;
  check bool "is_state" true prop.prop_state

let test_element_to_typescript () =
  let elem = Kirin_lit.Element.create
    ~tag_name:"my-button"
    ~class_name:"MyButton"
    ~properties:[Kirin_lit.Element.string_prop "label"]
    ~styles:[":host { display: inline-block; }"]
    "<button>${this.label}</button>"
  in
  let ts = Kirin_lit.Element.to_typescript elem in
  check bool "has import" true (String.length ts > 0);
  check bool "has customElement" true (contains ts "@customElement('my-button')");
  check bool "has class" true (contains ts "class MyButton extends LitElement")

let test_element_to_javascript () =
  let elem = Kirin_lit.Element.create
    ~tag_name:"my-counter"
    ~class_name:"MyCounter"
    ~properties:[Kirin_lit.Element.number_prop ~default:(Some 0.0) "count"]
    "<span>${this.count}</span>"
  in
  let js = Kirin_lit.Element.to_javascript elem in
  check bool "has class" true (contains js "class MyCounter extends LitElement");
  check bool "has customElements.define" true (contains js "customElements.define('my-counter'")

let test_element_to_json () =
  let elem = Kirin_lit.Element.create
    ~tag_name:"test-elem"
    ~class_name:"TestElem"
    "<div></div>"
  in
  let json = Kirin_lit.Element.to_json elem in
  match json with
  | `Assoc pairs ->
    check bool "has tagName" true (List.mem_assoc "tagName" pairs)
  | _ -> fail "Expected object"

(** {1 Template Tests} *)

let test_template_text () =
  let t = Kirin_lit.Template.text "Hello World" in
  let html = Kirin_lit.Template.to_html_literal t in
  check bool "contains text" true (contains html "Hello World")

let test_template_property () =
  let t = Kirin_lit.Template.prop "name" in
  let html = Kirin_lit.Template.to_html_literal t in
  check bool "has property binding" true (contains html "${this.name}")

let test_template_computed () =
  let t = Kirin_lit.Template.computed "this.items.length" in
  let html = Kirin_lit.Template.to_html_literal t in
  check bool "has computed" true (contains html "${this.items.length}")

let test_template_event () =
  let t = Kirin_lit.Template.event "click" "handleClick" in
  let html = Kirin_lit.Template.to_html_literal t in
  check bool "has event binding" true (contains html "@click")

let test_template_slot () =
  let t = Kirin_lit.Template.slot ~name:"header" () in
  let html = Kirin_lit.Template.to_html_literal t in
  check bool "has named slot" true (contains html "name=\"header\"")

let test_template_conditional () =
  let t = Kirin_lit.Template.when_ "this.show"
    ~then_:(Kirin_lit.Template.text "Visible")
    ~else_:(Kirin_lit.Template.text "Hidden")
    ()
  in
  let html = Kirin_lit.Template.to_html_literal t in
  check bool "has conditional" true (contains html "this.show ?")

let test_template_loop () =
  let t = Kirin_lit.Template.repeat
    ~items:"items"
    ~item:"item"
    (Kirin_lit.Template.text "<li>${item}</li>")
  in
  let html = Kirin_lit.Template.to_html_literal t in
  check bool "has map" true (contains html ".map(item =>")

let test_template_class_map () =
  let t = Kirin_lit.Template.class_map [("active", "this.isActive"); ("disabled", "this.isDisabled")] in
  let html = Kirin_lit.Template.to_html_literal t in
  check bool "has classMap" true (contains html "classMap")

let test_template_render_static () =
  let t = Kirin_lit.Template.concat [
    Kirin_lit.Template.text "<h1>";
    Kirin_lit.Template.prop "title";
    Kirin_lit.Template.text "</h1>";
  ] in
  let html = Kirin_lit.Template.render_static ~data:[("title", `String "Hello")] t in
  check bool "has rendered title" true (contains html "Hello")

(** {1 Shadow DOM Tests} *)

let test_shadow_dom_create () =
  let shadow = Kirin_lit.Shadow_dom.create
    ~mode:Kirin_lit.Shadow_dom.Open
    "<slot></slot>"
  in
  check string "content" "<slot></slot>" shadow.content

let test_shadow_dom_declarative () =
  let shadow = Kirin_lit.Shadow_dom.create
    ~mode:Kirin_lit.Shadow_dom.Open
    "<p>Content</p>"
  in
  let html = Kirin_lit.Shadow_dom.to_declarative_shadow_dom shadow in
  check bool "has template" true (contains html "<template shadowrootmode=\"open\">");
  check bool "has content" true (contains html "<p>Content</p>")

let test_shadow_dom_closed () =
  let shadow = Kirin_lit.Shadow_dom.create
    ~mode:Kirin_lit.Shadow_dom.Closed
    "Secret"
  in
  let html = Kirin_lit.Shadow_dom.to_declarative_shadow_dom shadow in
  check bool "has closed mode" true (contains html "shadowrootmode=\"closed\"")

let test_shadow_dom_styles () =
  let shadow = Kirin_lit.Shadow_dom.create
    ~styles:[":host { display: block; }"]
    "<slot></slot>"
  in
  let html = Kirin_lit.Shadow_dom.to_declarative_shadow_dom shadow in
  check bool "has style" true (contains html "<style>:host { display: block; }</style>")

let test_shadow_dom_slots () =
  let slot = Kirin_lit.Shadow_dom.render_slot (Kirin_lit.Shadow_dom.named_slot "content" ()) in
  check bool "has name" true (contains slot "name=\"content\"")

let test_shadow_dom_render_component () =
  let shadow = Kirin_lit.Shadow_dom.create "<slot></slot>" in
  let html = Kirin_lit.Shadow_dom.render_component
    ~tag_name:"my-comp"
    ~attributes:[("id", "test")]
    shadow
  in
  check bool "has tag" true (contains html "<my-comp");
  check bool "has id" true (contains html "id=\"test\"");
  check bool "has template" true (contains html "<template shadowrootmode")

let test_shadow_dom_polyfill () =
  let html = Kirin_lit.Shadow_dom.with_polyfill "<div></div>" in
  check bool "has polyfill" true (contains html "attachShadow")

let test_shadow_dom_parts () =
  let p = Kirin_lit.Shadow_dom.part ~name:"button" "button" in
  let html = Kirin_lit.Shadow_dom.render_with_part p in
  check bool "has part" true (contains html "part=\"button\"")

(** {1 SSR Tests} *)

let test_ssr_create () =
  let engine = Kirin_lit.Ssr.create Kirin_lit.Ssr.default_config in
  check bool "is healthy" true (Kirin_lit.Ssr.is_healthy engine)

let test_ssr_render () =
  let engine = Kirin_lit.Ssr.create Kirin_lit.Ssr.default_config in
  match Kirin_lit.Ssr.render engine ~tag_name:"my-element" () with
  | Ok html ->
    check bool "has tag" true (contains html "<my-element");
    check bool "has closing tag" true (contains html "</my-element>")
  | Error msg -> fail ("Render failed: " ^ msg)

let test_ssr_render_with_props () =
  let engine = Kirin_lit.Ssr.create Kirin_lit.Ssr.default_config in
  let props = `Assoc [("name", `String "Test")] in
  match Kirin_lit.Ssr.render engine ~tag_name:"my-element" ~props () with
  | Ok html ->
    check bool "has prop" true (contains html "name=\"Test\"")
  | Error msg -> fail ("Render failed: " ^ msg)

let test_ssr_declarative_shadow_dom () =
  let engine = Kirin_lit.Ssr.create Kirin_lit.Ssr.default_config in
  match Kirin_lit.Ssr.render engine ~tag_name:"my-element" () with
  | Ok html ->
    check bool "has declarative shadow dom" true (contains html "shadowrootmode=\"open\"")
  | Error _ -> fail "Should have rendered"

let test_ssr_polyfill () =
  let config = { Kirin_lit.Ssr.default_config with include_polyfill = true } in
  let engine = Kirin_lit.Ssr.create config in
  match Kirin_lit.Ssr.render engine ~tag_name:"my-element" () with
  | Ok html ->
    check bool "has polyfill" true (contains html "attachShadow")
  | Error _ -> fail "Should have rendered"

let test_ssr_no_polyfill () =
  let config = { Kirin_lit.Ssr.default_config with include_polyfill = false } in
  let engine = Kirin_lit.Ssr.create config in
  match Kirin_lit.Ssr.render engine ~tag_name:"my-element" () with
  | Ok html ->
    check bool "no polyfill" false (contains html "attachShadow")
  | Error _ -> fail "Should have rendered"

let test_ssr_fallback () =
  let engine = Kirin_lit.Ssr.create Kirin_lit.Ssr.default_config in
  let html = Kirin_lit.Ssr.render_with_fallback engine ~tag_name:"my-element" ~fallback:"<div>Fallback</div>" in
  check bool "not empty" true (String.length html > 0)

let test_ssr_stats () =
  let engine = Kirin_lit.Ssr.create Kirin_lit.Ssr.default_config in
  let _ = Kirin_lit.Ssr.render engine ~tag_name:"test" () in
  let stats = Kirin_lit.Ssr.stats engine in
  check int "total renders" 1 stats.stat_total_renders

let test_ssr_page () =
  let engine = Kirin_lit.Ssr.create Kirin_lit.Ssr.default_config in
  let components = [
    ("my-header", `Null, []);
    ("my-content", `Null, []);
    ("my-footer", `Null, []);
  ] in
  let html = Kirin_lit.Ssr.render_page engine ~components in
  check bool "has header" true (contains html "<my-header");
  check bool "has content" true (contains html "<my-content");
  check bool "has footer" true (contains html "<my-footer")

let test_ssr_shutdown () =
  let engine = Kirin_lit.Ssr.create Kirin_lit.Ssr.default_config in
  Kirin_lit.Ssr.shutdown engine;
  check bool "not healthy after shutdown" false (Kirin_lit.Ssr.is_healthy engine)

(** {1 Facade Tests} *)

let test_facade_element () =
  let elem = Kirin_lit.element
    ~tag_name:"test-el"
    ~class_name:"TestEl"
    "<div></div>"
  in
  check string "tag name" "test-el" elem.tag_name

let test_facade_shadow () =
  let s = Kirin_lit.shadow ~styles:[":host { color: red; }"] "<p></p>" in
  check bool "has styles" true (List.length s.styles > 0)

let test_facade_create_ssr () =
  let engine = Kirin_lit.create_ssr () in
  check bool "healthy" true (Kirin_lit.Ssr.is_healthy engine)

let test_facade_to_html () =
  let t = Kirin_lit.text "Test" in
  let html = Kirin_lit.to_html t in
  check bool "has html literal" true (contains html "html`")

(** {1 Test Suites} *)

let element_tests = [
  "create", `Quick, test_element_create;
  "valid tag name", `Quick, test_element_valid_tag_name;
  "string prop", `Quick, test_element_string_prop;
  "number prop", `Quick, test_element_number_prop;
  "boolean prop", `Quick, test_element_boolean_prop;
  "state prop", `Quick, test_element_state_prop;
  "to typescript", `Quick, test_element_to_typescript;
  "to javascript", `Quick, test_element_to_javascript;
  "to json", `Quick, test_element_to_json;
]

let template_tests = [
  "text", `Quick, test_template_text;
  "property", `Quick, test_template_property;
  "computed", `Quick, test_template_computed;
  "event", `Quick, test_template_event;
  "slot", `Quick, test_template_slot;
  "conditional", `Quick, test_template_conditional;
  "loop", `Quick, test_template_loop;
  "class map", `Quick, test_template_class_map;
  "render static", `Quick, test_template_render_static;
]

let shadow_dom_tests = [
  "create", `Quick, test_shadow_dom_create;
  "declarative", `Quick, test_shadow_dom_declarative;
  "closed mode", `Quick, test_shadow_dom_closed;
  "styles", `Quick, test_shadow_dom_styles;
  "slots", `Quick, test_shadow_dom_slots;
  "render component", `Quick, test_shadow_dom_render_component;
  "polyfill", `Quick, test_shadow_dom_polyfill;
  "parts", `Quick, test_shadow_dom_parts;
]

let ssr_tests = [
  "create", `Quick, test_ssr_create;
  "render", `Quick, test_ssr_render;
  "render with props", `Quick, test_ssr_render_with_props;
  "declarative shadow dom", `Quick, test_ssr_declarative_shadow_dom;
  "polyfill", `Quick, test_ssr_polyfill;
  "no polyfill", `Quick, test_ssr_no_polyfill;
  "fallback", `Quick, test_ssr_fallback;
  "stats", `Quick, test_ssr_stats;
  "page", `Quick, test_ssr_page;
  "shutdown", `Quick, test_ssr_shutdown;
]

let facade_tests = [
  "element", `Quick, test_facade_element;
  "shadow", `Quick, test_facade_shadow;
  "create ssr", `Quick, test_facade_create_ssr;
  "to html", `Quick, test_facade_to_html;
]

let () =
  run "Lit" [
    "Element", element_tests;
    "Template", template_tests;
    "Shadow DOM", shadow_dom_tests;
    "SSR", ssr_tests;
    "Facade", facade_tests;
  ]
