(** Kirin HTMX+ Integration

    Enhanced HTMX support with Hyperscript, Alpine.js, and extensions.

    {1 Overview}

    This module provides a comprehensive HTMX integration for Kirin:

    - {!Headers}: HTMX response headers (HX-Trigger, HX-Push-Url, etc.)
    - {!Hyperscript}: _hyperscript DSL helpers
    - {!Alpine}: Alpine.js directive helpers
    - {!Extensions}: HTMX extension attributes
    - {!Oob}: Out-of-Band swap helpers
    - {!Form}: Form generation with HTMX attributes

    {1 Quick Start}

    {[
      open Kirin_htmx

      (* HTMX response with trigger *)
      let handler req =
        let body = "<div>Updated!</div>" in
        Kirin.html body
        |> Headers.trigger ["itemAdded"]
        |> Headers.push_url "/items"

      (* OOB swap for multiple updates *)
      let multi_update req =
        let main = "<div>Main content</div>" in
        let oob = [
          Oob.notification ~id:"flash" "Item saved!";
          Oob.counter ~id:"item-count" 42;
        ] in
        Kirin.html (Oob.response ~main_content:main oob)

      (* Form with HTMX *)
      let form_example =
        Form.form ~action:"/submit" ~target:"#result" (
          Form.field ~label:"Name" ~name:"name"
            (Form.text_input ~name:"name" [])
        )
    ]}
*)

module Headers = Headers
module Hyperscript = Hyperscript
module Alpine = Alpine
module Extensions = Extensions
module Oob = Oob
module Form = Form

(** {1 Script Tags} *)

(** HTMX CDN script tag *)
let htmx_script ?(version = "1.9.10") () =
  Printf.sprintf {|<script src="https://unpkg.com/htmx.org@%s"></script>|} version

(** Hyperscript CDN script tag *)
let hyperscript_script ?(version = "0.9.12") () =
  Printf.sprintf {|<script src="https://unpkg.com/hyperscript.org@%s"></script>|} version

(** Alpine.js CDN script tag *)
let alpine_script ?(version = "3.13.3") () =
  Printf.sprintf {|<script defer src="https://cdn.jsdelivr.net/npm/alpinejs@%s/dist/cdn.min.js"></script>|} version

(** All scripts combined *)
let all_scripts ?(htmx_ver = "1.9.10") ?(hyperscript_ver = "0.9.12") ?(alpine_ver = "3.13.3") () =
  String.concat "\n" [
    htmx_script ~version:htmx_ver ();
    hyperscript_script ~version:hyperscript_ver ();
    alpine_script ~version:alpine_ver ();
  ]

(** {1 CSS} *)

(** Basic loading indicator CSS *)
let loading_css = {|<style>
.htmx-indicator { display: none; }
.htmx-request .htmx-indicator { display: inline; }
.htmx-request.htmx-indicator { display: inline; }

/* Alpine cloak */
[x-cloak] { display: none !important; }

/* Field errors */
.field-error { color: #dc2626; font-size: 0.875rem; }
.field-error:empty { display: none; }
</style>|}

(** {1 Common Patterns} *)

(** Create a dismissable alert/toast *)
let dismissable_alert ~id ?(class_ = "alert") content =
  Printf.sprintf {|<div id="%s" class="%s" _="%s">
  %s
  <button type="button" class="close" _="on click remove closest .alert">&times;</button>
</div>|} id class_ (Hyperscript.dismissable ()) content

(** Create an infinite scroll container *)
let infinite_scroll ~url ~trigger_id ?(threshold = "200px") () =
  Printf.sprintf {|<div hx-get="%s"
     hx-trigger="revealed"
     hx-swap="afterend"
     hx-target="this"
     id="%s"
     style="margin-bottom: %s;">
  Loading more...
</div>|} url trigger_id threshold

(** Create a search input with debounce *)
let search_input ~url ~target ?(debounce_ms = 300) ?(placeholder = "Search...") () =
  Printf.sprintf {|<input type="search"
       name="q"
       placeholder="%s"
       hx-get="%s"
       hx-trigger="input changed delay:%dms, search"
       hx-target="%s"
       hx-indicator=".htmx-indicator" />|} placeholder url debounce_ms target

(** Create a click-to-edit element *)
let click_to_edit ~get_url ~id content =
  Printf.sprintf {|<div id="%s"
     hx-get="%s"
     hx-trigger="click"
     hx-swap="outerHTML">
  %s
</div>|} id get_url content

(** Create a delete button with confirmation *)
let delete_with_confirm ~url ~target ~confirm_msg label =
  Printf.sprintf {|<button hx-delete="%s"
        hx-target="%s"
        hx-confirm="%s"
        hx-swap="outerHTML">
  %s
</button>|} url target confirm_msg label

(** Create a lazy-loaded element *)
let lazy_load ~url ~id ?(placeholder = "Loading...") () =
  Printf.sprintf {|<div id="%s"
     hx-get="%s"
     hx-trigger="load"
     hx-swap="outerHTML">
  %s
</div>|} id url placeholder

(** {1 Response Helpers} *)

(** Check if request is HTMX *)
let is_htmx = Headers.is_htmx_request

(** Check if request is boosted *)
let is_boosted = Headers.is_boosted

(** HTMX response with common headers *)
let response ?(trigger : string list option) ?(push_url : string option)
    ?(retarget : string option) ?(reswap : Headers.swap option) body =
  let resp = Kirin.Response.html body in
  let resp = match trigger with
    | Some events -> Headers.trigger events resp
    | None -> resp
  in
  let resp = match push_url with
    | Some url -> Headers.push_url url resp
    | None -> resp
  in
  let resp = match retarget with
    | Some sel -> Headers.retarget sel resp
    | None -> resp
  in
  match reswap with
  | Some swap -> Headers.reswap_typed swap resp
  | None -> resp
