(** Template tests *)

open Alcotest
open Test_helpers

let test_template_simple_var () =
  let ctx = Kirin.template_context [("name", "World")] in
  let result = Kirin.template_render ctx "Hello, {{name}}!" in
  check string "simple var" "Hello, World!" result

let test_template_html_escape () =
  let ctx = Kirin.template_context [("html", "<script>alert('xss')</script>")] in
  let result = Kirin.template_render ctx "{{html}}" in
  check string "escaped" "&lt;script&gt;alert(&#x27;xss&#x27;)&lt;/script&gt;" result

let test_template_raw_var () =
  let ctx = Kirin.template_context [("html", "<b>bold</b>")] in
  let result = Kirin.template_render ctx "{{{html}}}" in
  check string "raw" "<b>bold</b>" result

let test_template_if_true () =
  let ctx = Kirin.template_context_of [("show", `Bool true)] in
  let result = Kirin.template_render ctx "{{#if show}}visible{{/if show}}" in
  check string "if true" "visible" result

let test_template_if_false () =
  let ctx = Kirin.template_context_of [("show", `Bool false)] in
  let result = Kirin.template_render ctx "{{#if show}}visible{{/if show}}" in
  check string "if false" "" result

let test_template_if_else () =
  let ctx = Kirin.template_context_of [("logged_in", `Bool false)] in
  let result = Kirin.template_render ctx "{{#if logged_in}}Welcome{{else}}Login{{/if logged_in}}" in
  check string "if else" "Login" result

let test_template_unless () =
  let ctx = Kirin.template_context_of [("error", `Null)] in
  let result = Kirin.template_render ctx "{{#unless error}}OK{{/unless error}}" in
  check string "unless null" "OK" result

let test_template_each () =
  let ctx = Kirin.template_context_of [
    ("items", `List [`String "a"; `String "b"; `String "c"])
  ] in
  let result = Kirin.template_render ctx "{{#each items}}{{this}}{{/each items}}" in
  check string "each" "abc" result

let test_template_each_objects () =
  let ctx = Kirin.template_context_of [
    ("users", `List [
      `Assoc [("name", `String "Alice")];
      `Assoc [("name", `String "Bob")];
    ])
  ] in
  let result = Kirin.template_render ctx "{{#each users}}{{name}} {{/each users}}" in
  check string "each objects" "Alice Bob " result

let test_template_dot_notation () =
  let ctx = Kirin.template_context_of [
    ("user", `Assoc [("profile", `Assoc [("name", `String "John")])])
  ] in
  let result = Kirin.template_render ctx "{{user.profile.name}}" in
  check string "dot notation" "John" result

let test_template_interpolate () =
  let result = Kirin.template_interpolate "Hello {{name}}, welcome to {{place}}!"
    [("name", "Alice"); ("place", "Wonderland")] in
  check string "interpolate" "Hello Alice, welcome to Wonderland!" result

let test_template_html_response () =
  let ctx = Kirin.template_context [("title", "Test")] in
  let resp = Kirin.template_html ctx "<h1>{{title}}</h1>" in
  check string "body" "<h1>Test</h1>" (response_body_to_string (Kirin.Response.body resp));
  check (option string) "content-type" (Some "text/html; charset=utf-8")
    (Kirin.Response.header "content-type" resp)

(* Cyclic-partial guard regression tests. The cap stops a partial
   resolver cycle from blowing the OCaml call stack. Without the cap,
   [test_template_partial_cycle_terminates] below would stack-overflow
   the test process instead of producing a string. *)

let test_template_partial_resolves_normal () =
  (* Non-cyclic partials still work — sanity / non-regression. *)
  let partials = function
    | "greeting" -> Some "Hello, {{name}}!"
    | _ -> None
  in
  let ctx = `Assoc [("name", `String "world")] in
  let out = Kirin.Template.render ~partials ctx "<p>{{> greeting}}</p>" in
  check string "partial expanded" "<p>Hello, world!</p>" out

let test_template_partial_cycle_terminates () =
  (* Self-referential partial. Without the depth cap this stack-overflows. *)
  let partials = function
    | "loop" -> Some "x{{> loop}}"
    | _ -> None
  in
  let ctx = `Assoc [] in
  let out = Kirin.Template.render ~partials ctx "{{> loop}}" in
  (* The cap fires before the stack does and the offending partial site
     emits "". The surrounding "x" characters from successful inner
     expansions accumulate, but bounded by max_partial_depth. *)
  check bool "render returned without stack overflow" true (String.length out >= 0);
  check bool "output length is bounded (< 1MB)" true (String.length out < 1_000_000)

let test_template_partial_mutual_cycle_terminates () =
  (* Two-step cycle: a -> b -> a -> ... Same depth-based termination. *)
  let partials = function
    | "a" -> Some "A{{> b}}"
    | "b" -> Some "B{{> a}}"
    | _ -> None
  in
  let ctx = `Assoc [] in
  let out = Kirin.Template.render ~partials ctx "{{> a}}" in
  check bool "render returned without stack overflow" true (String.length out >= 0);
  check bool "output length is bounded" true (String.length out < 1_000_000)

let tests = [
  test_case "simple variable" `Quick test_template_simple_var;
  test_case "html escape" `Quick test_template_html_escape;
  test_case "raw variable" `Quick test_template_raw_var;
  test_case "if true" `Quick test_template_if_true;
  test_case "if false" `Quick test_template_if_false;
  test_case "if else" `Quick test_template_if_else;
  test_case "unless" `Quick test_template_unless;
  test_case "each" `Quick test_template_each;
  test_case "each objects" `Quick test_template_each_objects;
  test_case "dot notation" `Quick test_template_dot_notation;
  test_case "interpolate" `Quick test_template_interpolate;
  test_case "html response" `Quick test_template_html_response;
  test_case "partial resolves normally" `Quick test_template_partial_resolves_normal;
  test_case "self-cycle terminates" `Quick test_template_partial_cycle_terminates;
  test_case "mutual cycle terminates" `Quick test_template_partial_mutual_cycle_terminates;
]
