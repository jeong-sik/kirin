(** Hyperscript DSL Helpers

    Generate _hyperscript attributes for declarative behavior.
    See: https://hyperscript.org/ *)

(** {1 Core Builders} *)

(** Create a hyperscript attribute value *)
let script commands =
  String.concat " " commands

(** {1 Event Handlers} *)

(** On event trigger *)
let on event action =
  Printf.sprintf "on %s %s" event action

(** On click *)
let on_click action = on "click" action

(** On submit *)
let on_submit action = on "submit" action

(** On load *)
let on_load action = on "load" action

(** On keyup *)
let on_keyup action = on "keyup" action

(** On change *)
let on_change action = on "change" action

(** On focus *)
let on_focus action = on "focus" action

(** On blur *)
let on_blur action = on "blur" action

(** On mouse enter *)
let on_mouseenter action = on "mouseenter" action

(** On mouse leave *)
let on_mouseleave action = on "mouseleave" action

(** On intersection (viewport) *)
let on_intersect action = on "intersect" action

(** {1 Actions} *)

(** Add CSS class *)
let add_class cls = Printf.sprintf "add .%s" cls

(** Remove CSS class *)
let remove_class cls = Printf.sprintf "remove .%s" cls

(** Toggle CSS class *)
let toggle_class cls = Printf.sprintf "toggle .%s" cls

(** Add class to target *)
let add_class_to cls target = Printf.sprintf "add .%s to %s" cls target

(** Remove class from target *)
let remove_class_from cls target = Printf.sprintf "remove .%s from %s" cls target

(** Toggle class on target *)
let toggle_class_on cls target = Printf.sprintf "toggle .%s on %s" cls target

(** Set attribute *)
let set_attr name value = Printf.sprintf "set @%s to '%s'" name value

(** Remove attribute *)
let remove_attr name = Printf.sprintf "remove @%s" name

(** Set property *)
let set_prop name value = Printf.sprintf "set %s to %s" name value

(** Set inner HTML *)
let set_html content = Printf.sprintf "set innerHTML to '%s'" content

(** Set text content *)
let set_text content = Printf.sprintf "set textContent to '%s'" content

(** {1 Transitions} *)

(** Transition with duration *)
let transition property value duration =
  Printf.sprintf "transition %s to %s over %s" property value duration

(** Transition opacity *)
let fade_out ?(duration = "0.3s") () =
  transition "opacity" "0" duration

(** Transition opacity in *)
let fade_in ?(duration = "0.3s") () =
  transition "opacity" "1" duration

(** {1 Control Flow} *)

(** Wait/delay *)
let wait duration = Printf.sprintf "wait %s" duration

(** Wait then do action *)
let wait_then duration action = Printf.sprintf "wait %s then %s" duration action

(** If condition *)
let if_ condition then_action =
  Printf.sprintf "if %s %s" condition then_action

(** If-else *)
let if_else condition then_action else_action =
  Printf.sprintf "if %s %s else %s" condition then_action else_action

(** Repeat action *)
let repeat times action =
  Printf.sprintf "repeat %d times %s end" times action

(** Repeat until *)
let repeat_until condition action =
  Printf.sprintf "repeat until %s %s end" condition action

(** {1 DOM Manipulation} *)

(** Remove element *)
let remove target = Printf.sprintf "remove %s" target

(** Remove self *)
let remove_me = "remove me"

(** Append content *)
let append content target = Printf.sprintf "append '%s' to %s" content target

(** Prepend content *)
let prepend content target = Printf.sprintf "prepend '%s' to %s" content target

(** Put content *)
let put content location target =
  Printf.sprintf "put '%s' %s %s" content location target

(** {1 Events} *)

(** Trigger custom event *)
let trigger event = Printf.sprintf "trigger %s" event

(** Trigger event on target *)
let trigger_on event target = Printf.sprintf "trigger %s on %s" event target

(** Send event with detail *)
let send event detail = Printf.sprintf "send %s(detail: %s)" event detail

(** {1 HTMX Integration} *)

(** Trigger HTMX request on element *)
let htmx_trigger selector = Printf.sprintf "send htmx:trigger to %s" selector

(** Settle (wait for HTMX settle) *)
let settle = "settle"

(** {1 Target Selectors} *)

(** Self reference *)
let me = "me"

(** Closest parent matching selector *)
let closest selector = Printf.sprintf "closest %s" selector

(** Next sibling *)
let next selector = Printf.sprintf "next %s" selector

(** Previous sibling *)
let previous selector = Printf.sprintf "previous %s" selector

(** Find within *)
let find selector = Printf.sprintf "find %s in me" selector

(** {1 Complete Examples} *)

(** Toggle active class on click *)
let toggle_active_on_click =
  on_click (toggle_class "active")

(** Remove element with fade *)
let remove_with_fade ?(duration = "0.3s") () =
  script [
    on_click (fade_out ~duration ());
    wait_then duration remove_me;
  ]

(** Add loading class during HTMX request *)
let loading_indicator cls =
  script [
    on "htmx:beforeRequest" (add_class cls);
    on "htmx:afterRequest" (remove_class cls);
  ]

(** Dismiss alert/toast *)
let dismissable ?(duration = "0.3s") () =
  script [
    on_click (fade_out ~duration ());
    wait_then duration remove_me;
  ]

(** Auto-focus on load *)
let autofocus_on_load =
  on_load "focus()"

(** Copy to clipboard *)
let copy_to_clipboard selector =
  script [
    on_click (Printf.sprintf "call navigator.clipboard.writeText(%s.textContent)" selector);
    add_class "copied";
    wait_then "2s" (remove_class "copied");
  ]
