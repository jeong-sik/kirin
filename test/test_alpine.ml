(** Alpine.js Tests *)

open Alcotest

(** Helper *)
let contains haystack needle =
  try
    let _ = Str.search_forward (Str.regexp_string needle) haystack 0 in
    true
  with Not_found -> false

(** {1 Directive Tests} *)

let test_directive_data () =
  let d = Kirin_alpine.Directive.data "{ count: 0 }" in
  let attr = Kirin_alpine.Directive.to_attribute d in
  check bool "has x-data" true (contains attr "x-data");
  check bool "has expression" true (contains attr "{ count: 0 }")

let test_directive_show () =
  let d = Kirin_alpine.Directive.show "isOpen" in
  let attr = Kirin_alpine.Directive.to_attribute d in
  check bool "has x-show" true (contains attr "x-show=\"isOpen\"")

let test_directive_if () =
  let d = Kirin_alpine.Directive.if_ "condition" in
  let attr = Kirin_alpine.Directive.to_attribute d in
  check bool "has x-if" true (contains attr "x-if=\"condition\"")

let test_directive_bind () =
  let d = Kirin_alpine.Directive.bind ~attr:"class" "classes" in
  let attr = Kirin_alpine.Directive.to_attribute d in
  check bool "has x-bind" true (contains attr "x-bind:class=\"classes\"")

let test_directive_bind_shorthand () =
  let d = Kirin_alpine.Directive.bind ~attr:"class" "classes" in
  let attr = Kirin_alpine.Directive.to_shorthand d in
  check bool "has shorthand" true (contains attr ":class=\"classes\"")

let test_directive_on () =
  let d = Kirin_alpine.Directive.on "click" "handleClick" in
  let attr = Kirin_alpine.Directive.to_attribute d in
  check bool "has x-on" true (contains attr "x-on:click=\"handleClick\"")

let test_directive_on_shorthand () =
  let d = Kirin_alpine.Directive.on "click" "handleClick" in
  let attr = Kirin_alpine.Directive.to_shorthand d in
  check bool "has shorthand" true (contains attr "@click=\"handleClick\"")

let test_directive_on_modifiers () =
  let d = Kirin_alpine.Directive.on ~modifiers:[Kirin_alpine.Directive.Prevent; Kirin_alpine.Directive.Stop] "submit" "handle" in
  let attr = Kirin_alpine.Directive.to_attribute d in
  check bool "has prevent" true (contains attr ".prevent");
  check bool "has stop" true (contains attr ".stop")

let test_directive_text () =
  let d = Kirin_alpine.Directive.text "message" in
  let attr = Kirin_alpine.Directive.to_attribute d in
  check bool "has x-text" true (contains attr "x-text=\"message\"")

let test_directive_model () =
  let d = Kirin_alpine.Directive.model "name" in
  let attr = Kirin_alpine.Directive.to_attribute d in
  check bool "has x-model" true (contains attr "x-model=\"name\"")

let test_directive_for () =
  let d = Kirin_alpine.Directive.for_ ~item:"item" ~items:"items" () in
  let attr = Kirin_alpine.Directive.to_attribute d in
  check bool "has x-for" true (contains attr "x-for=\"item in items\"")

let test_directive_for_with_index () =
  let d = Kirin_alpine.Directive.for_ ~item:"item" ~index:"i" ~items:"items" () in
  let attr = Kirin_alpine.Directive.to_attribute d in
  check bool "has index" true (contains attr "(item, i) in items")

let test_directive_cloak () =
  let attr = Kirin_alpine.Directive.to_attribute Kirin_alpine.Directive.cloak in
  check string "is x-cloak" "x-cloak" attr

let test_directive_ref () =
  let d = Kirin_alpine.Directive.ref_ "button" in
  let attr = Kirin_alpine.Directive.to_attribute d in
  check bool "has x-ref" true (contains attr "x-ref=\"button\"")

let test_directive_to_json () =
  let d = Kirin_alpine.Directive.data "{ x: 1 }" in
  let json = Kirin_alpine.Directive.to_json d in
  match json with
  | `Assoc pairs -> check bool "has type" true (List.mem_assoc "type" pairs)
  | _ -> fail "Expected object"

(** {1 Component Tests} *)

let test_component_create () =
  let c = Kirin_alpine.Component.create "counter" in
  check string "name" "counter" c.name

let test_component_with_property () =
  let c = Kirin_alpine.Component.(create "test"
    |> with_property ~default:(`Int 0) `Number "count"
  ) in
  check int "has property" 1 (List.length c.properties)

let test_component_with_method () =
  let c = Kirin_alpine.Component.(create "test"
    |> with_method "increment" [] "this.count++"
  ) in
  check int "has method" 1 (List.length c.methods)

let test_component_to_x_data () =
  let c = Kirin_alpine.Component.(create "counter"
    |> with_property ~default:(`Int 0) `Number "count"
    |> with_method "increment" [] "this.count++"
  ) in
  let x_data = Kirin_alpine.Component.to_x_data c in
  check bool "has count" true (contains x_data "count: 0");
  check bool "has increment" true (contains x_data "increment()")

let test_component_to_alpine_data () =
  let c = Kirin_alpine.Component.create "test" in
  let code = Kirin_alpine.Component.to_alpine_data c in
  check bool "has Alpine.data" true (contains code "Alpine.data('test'")

let test_component_inline_data () =
  let c = Kirin_alpine.Component.(create "user"
    |> with_property ~default:(`String "") `String "name"
  ) in
  let data = [("name", `String "John")] in
  let inline = Kirin_alpine.Component.inline_data ~data c in
  check bool "has server value" true (contains inline "\"John\"")

let test_component_string_prop () =
  let p = Kirin_alpine.Component.string_prop ~default:"hello" "greeting" in
  check string "name" "greeting" p.prop_name

let test_component_number_prop () =
  let p = Kirin_alpine.Component.number_prop ~default:42.0 "count" in
  check string "name" "count" p.prop_name

(** {1 Store Tests} *)

let test_store_create () =
  let s = Kirin_alpine.Store.create "app" in
  check string "name" "app" s.name

let test_store_with_property () =
  let s = Kirin_alpine.Store.(create "app"
    |> with_property "theme" (`String "dark")
  ) in
  check int "has property" 1 (List.length s.properties)

let test_store_from_object () =
  let s = Kirin_alpine.Store.from_object "config" [
    ("debug", `Bool true);
    ("version", `String "1.0");
  ] in
  check int "has properties" 2 (List.length s.properties)

let test_store_to_object () =
  let s = Kirin_alpine.Store.(create "app"
    |> with_property "count" (`Int 5)
  ) in
  let obj = Kirin_alpine.Store.to_object s in
  check bool "has count" true (contains obj "count: 5")

let test_store_to_alpine_store () =
  let s = Kirin_alpine.Store.create "data" in
  let code = Kirin_alpine.Store.to_alpine_store s in
  check bool "has Alpine.store" true (contains code "Alpine.store('data'")

let test_store_accessor () =
  let s = Kirin_alpine.Store.create "user" in
  let acc = Kirin_alpine.Store.accessor s "name" in
  check string "accessor" "$store.user.name" acc

let test_store_x_text () =
  let text = Kirin_alpine.Store.x_text_store "user" "email" in
  check bool "has x-text" true (contains text "x-text=\"$store.user.email\"")

let test_store_hydration_script () =
  let script = Kirin_alpine.Store.hydration_script ~store_name:"app" ~data:[("ready", `Bool true)] in
  check bool "has Alpine.store" true (contains script "Alpine.store('app'");
  check bool "has data" true (contains script "\"ready\"")

(** {1 SSR Tests} *)

let test_ssr_config () =
  let config = Kirin_alpine.Ssr.default_config in
  check bool "has cdn" true (String.length config.cdn_url > 0)

let test_ssr_script_tag () =
  let tag = Kirin_alpine.Ssr.script_tag Kirin_alpine.Ssr.default_config in
  check bool "has script" true (contains tag "<script");
  check bool "has src" true (contains tag "alpinejs")

let test_ssr_cloak_style () =
  let style = Kirin_alpine.Ssr.cloak_style in
  check bool "has x-cloak" true (contains style "[x-cloak]");
  check bool "has display none" true (contains style "display: none")

let test_ssr_head_scripts () =
  let head = Kirin_alpine.Ssr.head_scripts Kirin_alpine.Ssr.default_config in
  check bool "has style" true (contains head "<style>");
  check bool "has script" true (contains head "<script")

let test_ssr_empty_state () =
  let state = Kirin_alpine.Ssr.empty_state in
  check int "no components" 0 (List.length state.components);
  check int "no stores" 0 (List.length state.stores)

let test_ssr_with_component () =
  let c = Kirin_alpine.Component.create "test" in
  let state = Kirin_alpine.Ssr.(empty_state |> with_component c) in
  check int "has component" 1 (List.length state.components)

let test_ssr_with_store () =
  let s = Kirin_alpine.Store.create "app" in
  let state = Kirin_alpine.Ssr.(empty_state |> with_store s) in
  check int "has store" 1 (List.length state.stores)

let test_ssr_with_data () =
  let state = Kirin_alpine.Ssr.(empty_state |> with_data "key" (`String "value")) in
  check int "has data" 1 (List.length state.initial_data)

let test_ssr_init_script () =
  let c = Kirin_alpine.Component.create "counter" in
  let state = Kirin_alpine.Ssr.(empty_state |> with_component c) in
  let script = Kirin_alpine.Ssr.init_script state in
  check bool "has alpine:init" true (contains script "alpine:init");
  check bool "has component" true (contains script "Alpine.data('counter'")

let test_ssr_render_element () =
  let html = Kirin_alpine.Ssr.render_element
    ~tag:"div"
    ~directives:[Kirin_alpine.Directive.data "{ open: false }"]
    ~children:"<p>Content</p>"
    ()
  in
  check bool "has div" true (contains html "<div");
  check bool "has x-data" true (contains html "x-data")

let test_ssr_render_cloaked () =
  let html = Kirin_alpine.Ssr.render_cloaked
    ~tag:"div"
    ~directives:[Kirin_alpine.Directive.show "visible"]
    ()
  in
  check bool "has x-cloak" true (contains html "x-cloak");
  check bool "has x-show" true (contains html "x-show")

let test_ssr_render_page () =
  let config = Kirin_alpine.Ssr.default_config in
  let state = Kirin_alpine.Ssr.empty_state in
  let page = Kirin_alpine.Ssr.render_page ~config ~state ~body:"<p>Hello</p>" in
  check bool "has doctype" true (contains page "<!DOCTYPE html>");
  check bool "has body" true (contains page "<p>Hello</p>")

let test_ssr_eval_expression () =
  let result = Kirin_alpine.Ssr.eval_expression ~data:[("name", `String "Alice")] "name" in
  check string "evaluated" "Alice" result

(** {1 Facade Tests} *)

let test_facade_data () =
  let d = Kirin_alpine.data "{ x: 1 }" in
  let attr = Kirin_alpine.Directive.to_attribute d in
  check bool "has x-data" true (contains attr "x-data")

let test_facade_component () =
  let c = Kirin_alpine.component "test" in
  check string "name" "test" c.name

let test_facade_store () =
  let s = Kirin_alpine.store "app" in
  check string "name" "app" s.name

let test_facade_modifiers () =
  let d = Kirin_alpine.on ~modifiers:[Kirin_alpine.prevent; Kirin_alpine.stop] "click" "handle" in
  let attr = Kirin_alpine.Directive.to_attribute d in
  check bool "has modifiers" true (contains attr ".prevent")

(** {1 Test Suites} *)

let directive_tests = [
  "data", `Quick, test_directive_data;
  "show", `Quick, test_directive_show;
  "if", `Quick, test_directive_if;
  "bind", `Quick, test_directive_bind;
  "bind shorthand", `Quick, test_directive_bind_shorthand;
  "on", `Quick, test_directive_on;
  "on shorthand", `Quick, test_directive_on_shorthand;
  "on modifiers", `Quick, test_directive_on_modifiers;
  "text", `Quick, test_directive_text;
  "model", `Quick, test_directive_model;
  "for", `Quick, test_directive_for;
  "for with index", `Quick, test_directive_for_with_index;
  "cloak", `Quick, test_directive_cloak;
  "ref", `Quick, test_directive_ref;
  "to json", `Quick, test_directive_to_json;
]

let component_tests = [
  "create", `Quick, test_component_create;
  "with property", `Quick, test_component_with_property;
  "with method", `Quick, test_component_with_method;
  "to x-data", `Quick, test_component_to_x_data;
  "to alpine data", `Quick, test_component_to_alpine_data;
  "inline data", `Quick, test_component_inline_data;
  "string prop", `Quick, test_component_string_prop;
  "number prop", `Quick, test_component_number_prop;
]

let store_tests = [
  "create", `Quick, test_store_create;
  "with property", `Quick, test_store_with_property;
  "from object", `Quick, test_store_from_object;
  "to object", `Quick, test_store_to_object;
  "to alpine store", `Quick, test_store_to_alpine_store;
  "accessor", `Quick, test_store_accessor;
  "x-text", `Quick, test_store_x_text;
  "hydration script", `Quick, test_store_hydration_script;
]

let ssr_tests = [
  "config", `Quick, test_ssr_config;
  "script tag", `Quick, test_ssr_script_tag;
  "cloak style", `Quick, test_ssr_cloak_style;
  "head scripts", `Quick, test_ssr_head_scripts;
  "empty state", `Quick, test_ssr_empty_state;
  "with component", `Quick, test_ssr_with_component;
  "with store", `Quick, test_ssr_with_store;
  "with data", `Quick, test_ssr_with_data;
  "init script", `Quick, test_ssr_init_script;
  "render element", `Quick, test_ssr_render_element;
  "render cloaked", `Quick, test_ssr_render_cloaked;
  "render page", `Quick, test_ssr_render_page;
  "eval expression", `Quick, test_ssr_eval_expression;
]

let facade_tests = [
  "data", `Quick, test_facade_data;
  "component", `Quick, test_facade_component;
  "store", `Quick, test_facade_store;
  "modifiers", `Quick, test_facade_modifiers;
]

let () =
  run "Alpine" [
    "Directive", directive_tests;
    "Component", component_tests;
    "Store", store_tests;
    "SSR", ssr_tests;
    "Facade", facade_tests;
  ]
