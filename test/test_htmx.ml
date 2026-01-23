(** Test suite for Kirin HTMX+ module *)

open Alcotest

(** {1 Headers Tests} *)

let test_trigger () =
  let resp = Kirin.Response.html "<div>test</div>" in
  let resp = Kirin_htmx.Headers.trigger ["itemAdded"; "cartUpdated"] resp in
  let hx_trigger = Kirin.Response.header "HX-Trigger" resp in
  check (option string) "trigger header" (Some "itemAdded, cartUpdated") hx_trigger

let test_push_url () =
  let resp = Kirin.Response.html "<div>test</div>" in
  let resp = Kirin_htmx.Headers.push_url "/items/123" resp in
  let hx_push = Kirin.Response.header "HX-Push-Url" resp in
  check (option string) "push url header" (Some "/items/123") hx_push

let test_redirect () =
  let resp = Kirin.Response.html "" in
  let resp = Kirin_htmx.Headers.redirect "/login" resp in
  let hx_redirect = Kirin.Response.header "HX-Redirect" resp in
  check (option string) "redirect header" (Some "/login") hx_redirect

let test_reswap_typed () =
  let resp = Kirin.Response.html "<div>test</div>" in
  let resp = Kirin_htmx.Headers.reswap_typed Kirin_htmx.Headers.Outer_html resp in
  let hx_reswap = Kirin.Response.header "HX-Reswap" resp in
  check (option string) "reswap header" (Some "outerHTML") hx_reswap

let test_refresh () =
  let resp = Kirin.Response.html "" in
  let resp = Kirin_htmx.Headers.refresh resp in
  let hx_refresh = Kirin.Response.header "HX-Refresh" resp in
  check (option string) "refresh header" (Some "true") hx_refresh

let test_swap_to_string () =
  check string "innerHTML" "innerHTML" (Kirin_htmx.Headers.swap_to_string Kirin_htmx.Headers.Inner_html);
  check string "outerHTML" "outerHTML" (Kirin_htmx.Headers.swap_to_string Kirin_htmx.Headers.Outer_html);
  check string "beforeend" "beforeend" (Kirin_htmx.Headers.swap_to_string Kirin_htmx.Headers.Before_end);
  check string "delete" "delete" (Kirin_htmx.Headers.swap_to_string Kirin_htmx.Headers.Delete)

(** {1 Hyperscript Tests} *)

let test_hyperscript_on_click () =
  let script = Kirin_htmx.Hyperscript.on_click (Kirin_htmx.Hyperscript.toggle_class "active") in
  check string "on click toggle" "on click toggle .active" script

let test_hyperscript_fade_out () =
  let script = Kirin_htmx.Hyperscript.fade_out () in
  check string "fade out" "transition opacity to 0 over 0.3s" script

let test_hyperscript_remove_me () =
  let script = Kirin_htmx.Hyperscript.remove_me in
  check string "remove me" "remove me" script

let test_hyperscript_wait_then () =
  let script = Kirin_htmx.Hyperscript.wait_then "2s" (Kirin_htmx.Hyperscript.remove_class "loading") in
  check string "wait then" "wait 2s then remove .loading" script

let test_hyperscript_add_class_to () =
  let script = Kirin_htmx.Hyperscript.add_class_to "highlight" "#target" in
  check string "add class to" "add .highlight to #target" script

(** {1 Alpine Tests} *)

let test_alpine_data () =
  let data = Kirin_htmx.Alpine.data [("open", "false"); ("count", "0")] in
  check string "x-data" "{ open: false, count: 0 }" data

let test_alpine_show () =
  let (attr, value) = Kirin_htmx.Alpine.show "open" in
  check string "x-show attr" "x-show" attr;
  check string "x-show value" "open" value

let test_alpine_on_click () =
  let (attr, value) = Kirin_htmx.Alpine.on_click "open = !open" in
  check string "@click attr" "@click" attr;
  check string "@click value" "open = !open" value

let test_alpine_bind_class () =
  let (attr, value) = Kirin_htmx.Alpine.bind_class "{ active: isActive }" in
  check string ":class attr" ":class" attr;
  check string ":class value" "{ active: isActive }" value

let test_alpine_model () =
  let (attr, value) = Kirin_htmx.Alpine.model "name" in
  check string "x-model attr" "x-model" attr;
  check string "x-model value" "name" value

(** {1 Extensions Tests} *)

let test_ext_use () =
  let (attr, value) = Kirin_htmx.Extensions.use ["preload"; "ws"] in
  check string "hx-ext attr" "hx-ext" attr;
  check string "hx-ext value" "preload, ws" value

let test_ext_ws_setup () =
  let attrs = Kirin_htmx.Extensions.Ws.setup "ws://localhost:8080/chat" in
  check int "ws attrs count" 2 (List.length attrs)

let test_ext_sse_setup () =
  let attrs = Kirin_htmx.Extensions.Sse.setup "/events" "message" in
  check int "sse attrs count" 3 (List.length attrs)

let test_ext_response_targets () =
  let (attr, value) = Kirin_htmx.Extensions.ResponseTargets.target_4xx "#error" in
  check string "target-4* attr" "hx-target-4*" attr;
  check string "target-4* value" "#error" value

let test_ext_loading_states () =
  let (attr, value) = Kirin_htmx.Extensions.LoadingStates.add_class "loading" in
  check string "loading class attr" "data-loading-class" attr;
  check string "loading class value" "loading" value

(** {1 OOB Tests} *)

let test_oob_div () =
  let html = Kirin_htmx.Oob.div ~id:"notification" "Hello!" in
  check bool "contains id" true (String.length html > 0)

let test_oob_notification () =
  let html = Kirin_htmx.Oob.notification ~id:"flash" "Saved!" in
  check bool "notification html" true (String.length html > 0)

let test_oob_counter () =
  let html = Kirin_htmx.Oob.counter ~id:"count" 42 in
  check bool "counter html" true (String.length html > 0)

let test_oob_clear () =
  let html = Kirin_htmx.Oob.clear ~id:"target" in
  check bool "clear html" true (String.length html > 0)

let test_oob_multi () =
  let html = Kirin_htmx.Oob.multi [
    Kirin_htmx.Oob.notification ~id:"a" "A";
    Kirin_htmx.Oob.notification ~id:"b" "B";
  ] in
  check bool "multi html" true (String.length html > 0)

(** {1 Form Tests} *)

let test_form_text_input () =
  let html = Kirin_htmx.Form.text_input ~name:"username" ~placeholder:"Enter name" [] in
  check bool "input html" true (String.length html > 0)

let test_form_email_input () =
  let html = Kirin_htmx.Form.email_input ~name:"email" [] in
  check bool "email input" true (String.length html > 0)

let test_form_hidden_input () =
  let html = Kirin_htmx.Form.hidden_input ~name:"csrf" ~value:"token123" in
  check bool "hidden input" true (String.length html > 0)

let test_form_select () =
  let html = Kirin_htmx.Form.select ~name:"country" [
    Kirin_htmx.Form.option ~value:"us" "United States";
    Kirin_htmx.Form.option ~value:"kr" ~selected:true "Korea";
  ] [] in
  check bool "select html" true (String.length html > 0)

let test_form_submit () =
  let html = Kirin_htmx.Form.submit "Save" in
  check bool "submit button" true (String.length html > 0)

let test_form_wrapper () =
  let html = Kirin_htmx.Form.form ~action:"/submit" ~target:"#result" "content" in
  check bool "form html" true (String.length html > 0)

(** {1 Pattern Tests} *)

let test_search_input () =
  let html = Kirin_htmx.search_input ~url:"/search" ~target:"#results" () in
  check bool "search input" true (String.length html > 0)

let test_infinite_scroll () =
  let html = Kirin_htmx.infinite_scroll ~url:"/page/2" ~trigger_id:"load-more" () in
  check bool "infinite scroll" true (String.length html > 0)

let test_lazy_load () =
  let html = Kirin_htmx.lazy_load ~url:"/content" ~id:"lazy" () in
  check bool "lazy load" true (String.length html > 0)

let test_delete_with_confirm () =
  let html = Kirin_htmx.delete_with_confirm ~url:"/item/1" ~target:"#item-1" ~confirm_msg:"Delete?" "Delete" in
  check bool "delete button" true (String.length html > 0)

(** {1 Script Tests} *)

let test_htmx_script () =
  let html = Kirin_htmx.htmx_script () in
  check bool "htmx script" true (String.sub html 0 20 |> fun s -> String.length s > 0)

let test_all_scripts () =
  let html = Kirin_htmx.all_scripts () in
  check bool "all scripts" true (String.length html > 100)

(** {1 Test Suite} *)

let headers_tests = [
  "trigger", `Quick, test_trigger;
  "push_url", `Quick, test_push_url;
  "redirect", `Quick, test_redirect;
  "reswap_typed", `Quick, test_reswap_typed;
  "refresh", `Quick, test_refresh;
  "swap_to_string", `Quick, test_swap_to_string;
]

let hyperscript_tests = [
  "on_click", `Quick, test_hyperscript_on_click;
  "fade_out", `Quick, test_hyperscript_fade_out;
  "remove_me", `Quick, test_hyperscript_remove_me;
  "wait_then", `Quick, test_hyperscript_wait_then;
  "add_class_to", `Quick, test_hyperscript_add_class_to;
]

let alpine_tests = [
  "data", `Quick, test_alpine_data;
  "show", `Quick, test_alpine_show;
  "on_click", `Quick, test_alpine_on_click;
  "bind_class", `Quick, test_alpine_bind_class;
  "model", `Quick, test_alpine_model;
]

let extensions_tests = [
  "use", `Quick, test_ext_use;
  "ws_setup", `Quick, test_ext_ws_setup;
  "sse_setup", `Quick, test_ext_sse_setup;
  "response_targets", `Quick, test_ext_response_targets;
  "loading_states", `Quick, test_ext_loading_states;
]

let oob_tests = [
  "div", `Quick, test_oob_div;
  "notification", `Quick, test_oob_notification;
  "counter", `Quick, test_oob_counter;
  "clear", `Quick, test_oob_clear;
  "multi", `Quick, test_oob_multi;
]

let form_tests = [
  "text_input", `Quick, test_form_text_input;
  "email_input", `Quick, test_form_email_input;
  "hidden_input", `Quick, test_form_hidden_input;
  "select", `Quick, test_form_select;
  "submit", `Quick, test_form_submit;
  "form_wrapper", `Quick, test_form_wrapper;
]

let pattern_tests = [
  "search_input", `Quick, test_search_input;
  "infinite_scroll", `Quick, test_infinite_scroll;
  "lazy_load", `Quick, test_lazy_load;
  "delete_with_confirm", `Quick, test_delete_with_confirm;
]

let script_tests = [
  "htmx_script", `Quick, test_htmx_script;
  "all_scripts", `Quick, test_all_scripts;
]

let () =
  run "Kirin_htmx" [
    "Headers", headers_tests;
    "Hyperscript", hyperscript_tests;
    "Alpine", alpine_tests;
    "Extensions", extensions_tests;
    "Oob", oob_tests;
    "Form", form_tests;
    "Patterns", pattern_tests;
    "Scripts", script_tests;
  ]
