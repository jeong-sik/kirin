(** Solid.js Streaming SSR

    Progressive rendering with Suspense support. *)

(** {1 Stream Types} *)

(** Stream event *)
type event =
  | ShellReady of string  (* Initial HTML shell *)
  | Chunk of string       (* Streamed HTML chunk *)
  | Script of string      (* Inline script for hydration *)
  | Complete              (* Stream complete *)
  | Error of string       (* Stream error *)

(** Stream callback *)
type callback = event -> unit

(** {1 Streaming Configuration} *)

(** Streaming options *)
type options = {
  buffer_size: int;         (* Chunk buffer size *)
  flush_interval_ms: int;   (* Auto-flush interval *)
  abort_delay_ms: int;      (* Delay before aborting slow suspense *)
  progressive: bool;        (* Enable progressive hydration *)
}

(** Default options *)
let default_options = {
  buffer_size = 8192;
  flush_interval_ms = 50;
  abort_delay_ms = 10000;
  progressive = true;
}

(** {1 Chunk Assembly} *)

(** Assemble shell with placeholder *)
let shell_with_placeholder ~head ~body_start ~suspense_fallbacks =
  Printf.sprintf {|<!DOCTYPE html>
<html>
<head>%s</head>
<body>%s%s|}
    head
    body_start
    (String.concat "" suspense_fallbacks)

(** Suspense replacement script *)
let replacement_script ~suspense_id ~content =
  Printf.sprintf {|<script>
$SR("s%s",%s)
</script>|} suspense_id (Yojson.Safe.to_string (`String content))

(** Streaming runtime script *)
let runtime_script = {|<script>
window.$SR = function(id, html) {
  var t = document.getElementById(id);
  if (t) {
    var r = document.createRange();
    r.setStartAfter(t);
    var f = r.createContextualFragment(html);
    t.parentNode.insertBefore(f, t.nextSibling);
    t.remove();
  }
};
</script>|}

(** {1 HTML Stream Parser} *)

(** Parse worker stream output *)
let parse_stream_line line =
  try
    Protocol.decode_stream_chunk line
  with _ ->
    Protocol.Error "Parse error"

(** {1 Kirin Response Helpers} *)

(** Start streaming response *)
let response_start ~content_type =
  let headers = [
    ("Content-Type", content_type);
    ("Transfer-Encoding", "chunked");
    ("X-Content-Type-Options", "nosniff");
  ] in
  headers

(** Format chunk for chunked transfer encoding *)
let format_chunk data =
  let len = String.length data in
  Printf.sprintf "%x\r\n%s\r\n" len data

(** End chunk *)
let end_chunk = "0\r\n\r\n"

(** {1 SSE Streaming} *)

(** SSE event format *)
let sse_event ~event_type ~data =
  Printf.sprintf "event: %s\ndata: %s\n\n" event_type data

(** Create SSE response for streaming SSR *)
let sse_stream ~engine ~url ?(props = `Assoc []) () =
  (* Note: This is a simplified version. Real implementation would use
     Eio streams or similar for proper async handling *)
  match Ssr.render engine ~url ~props () with
  | Ok response ->
    (* Convert render response to SSE events *)
    let shell_event = sse_event ~event_type:"shell" ~data:response.html in
    let head_event = sse_event ~event_type:"head" ~data:response.head in
    let complete_event = sse_event ~event_type:"complete" ~data:"" in
    String.concat "" [shell_event; head_event; complete_event]
  | Error msg ->
    sse_event ~event_type:"error" ~data:msg

(** {1 Progressive Hydration} *)

(** Island priority *)
type priority =
  | Immediate   (* Hydrate immediately *)
  | Visible     (* Hydrate when visible (IntersectionObserver) *)
  | Idle        (* Hydrate when idle (requestIdleCallback) *)
  | Interaction (* Hydrate on first interaction *)

let priority_to_string = function
  | Immediate -> "immediate"
  | Visible -> "visible"
  | Idle -> "idle"
  | Interaction -> "interaction"

(** Generate progressive hydration marker *)
let progressive_marker ~component_id ~priority =
  Printf.sprintf {|<solid-hydrate data-id="%s" data-priority="%s">|}
    component_id (priority_to_string priority)

(** Progressive hydration script *)
let progressive_script = {|<script>
(function() {
  const hydrators = new Map();

  window.__solidHydrate = function(id, fn) {
    hydrators.set(id, fn);
    checkHydration(id);
  };

  function checkHydration(id) {
    const el = document.querySelector(`[data-id="${id}"]`);
    if (!el) return;

    const priority = el.dataset.priority;
    const fn = hydrators.get(id);
    if (!fn) return;

    switch(priority) {
      case 'immediate':
        fn();
        break;
      case 'visible':
        new IntersectionObserver((entries, obs) => {
          if (entries[0].isIntersecting) {
            fn();
            obs.disconnect();
          }
        }).observe(el);
        break;
      case 'idle':
        requestIdleCallback(() => fn());
        break;
      case 'interaction':
        ['click', 'touchstart', 'focus'].forEach(e =>
          el.addEventListener(e, () => fn(), { once: true })
        );
        break;
    }
  }
})();
</script>|}

(** {1 Out-of-Order Streaming} *)

(** Generate out-of-order placeholder *)
let ooo_placeholder ~id ~fallback =
  Printf.sprintf {|<template id="%s">%s</template>|} id fallback

(** Generate out-of-order replacement *)
let ooo_replacement ~id ~content =
  Printf.sprintf {|<script>
(function(){
  var t=document.getElementById("%s");
  if(t){
    var p=t.parentNode,n=t.nextSibling;
    t.remove();
    var d=document.createElement("div");
    d.innerHTML=%s;
    while(d.firstChild)p.insertBefore(d.firstChild,n);
  }
})();
</script>|} id (Yojson.Safe.to_string (`String content))

(** {1 Error Boundary Streaming} *)

(** Stream error boundary fallback *)
let error_boundary_fallback ~boundary_id ~error_message =
  Printf.sprintf {|<div data-error-boundary="%s" class="ssr-error">
  <h2>Something went wrong</h2>
  <pre>%s</pre>
</div>|} boundary_id (Kirin.html_escape error_message)

(** Stream error recovery script *)
let error_recovery_script ~boundary_id =
  Printf.sprintf {|<script>
window.__solidErrorBoundary = window.__solidErrorBoundary || {};
window.__solidErrorBoundary["%s"] = true;
</script>|} boundary_id
