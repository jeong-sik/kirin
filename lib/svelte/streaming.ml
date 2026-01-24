(** Svelte Streaming SSR

    Streaming server-side rendering with progressive hydration. *)

(** {1 Stream Types} *)

(** Stream chunk type *)
type chunk =
  | Shell of string     (* Initial HTML shell *)
  | Head of string      (* Additional head content *)
  | Html of string      (* Streamed HTML content *)
  | Script of string    (* Inline script *)
  | Complete            (* Stream complete *)
  | Error of string     (* Error message *)

(** Stream state *)
type stream_state = {
  mutable chunks: chunk list;
  mutable completed: bool;
  mutable error: string option;
}

(** {1 Stream Creation} *)

(** Create new stream *)
let create () = {
  chunks = [];
  completed = false;
  error = None;
}

(** Add chunk to stream *)
let add_chunk stream chunk =
  if not stream.completed then
    stream.chunks <- stream.chunks @ [chunk]

(** Mark stream as complete *)
let complete stream =
  add_chunk stream Complete;
  stream.completed <- true

(** Mark stream as errored *)
let fail stream msg =
  stream.error <- Some msg;
  add_chunk stream (Error msg);
  stream.completed <- true

(** {1 Shell Generation} *)

(** Generate streaming shell *)
let shell ~title ~entry_script:_ ?(lang="en") () =
  Printf.sprintf {|<!DOCTYPE html>
<html lang="%s">
<head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <title>%s</title>
<!--HEAD-->
</head>
<body>
    <div id="svelte">|}
    lang
    (Kirin.html_escape title)

(** Generate shell end *)
let shell_end ~entry_script =
  Printf.sprintf {|</div>
    <script type="module" src="%s"></script>
</body>
</html>|}
    entry_script

(** {1 Chunk Rendering} *)

(** Render chunk to string *)
let render_chunk = function
  | Shell html -> html
  | Head content -> Printf.sprintf "<!--HEAD-START-->%s<!--HEAD-END-->" content
  | Html content -> content
  | Script content -> Printf.sprintf "<script>%s</script>" content
  | Complete -> ""
  | Error msg -> Printf.sprintf "<!-- SSR Error: %s -->" (Kirin.html_escape msg)

(** Render all chunks *)
let render_chunks stream =
  List.map render_chunk stream.chunks
  |> String.concat ""

(** {1 Progressive Hydration} *)

(** Hydration priority *)
type hydration_priority =
  | Immediate   (* Hydrate immediately *)
  | Visible     (* Hydrate when visible *)
  | Idle        (* Hydrate when idle *)
  | Interaction (* Hydrate on first interaction *)

(** Priority to string *)
let priority_string = function
  | Immediate -> "immediate"
  | Visible -> "visible"
  | Idle -> "idle"
  | Interaction -> "interaction"

(** Generate hydration marker *)
let hydration_marker ~id ~priority =
  Printf.sprintf {|<script>__svelte_hydrate('%s','%s')</script>|}
    id
    (priority_string priority)

(** Generate hydration boundary *)
let hydration_boundary ~id ~priority ~fallback content =
  Printf.sprintf {|<div data-svelte-hydrate="%s" data-priority="%s">%s</div>%s|}
    id
    (priority_string priority)
    (Option.value ~default:content fallback)
    (hydration_marker ~id ~priority)

(** {1 Out-of-Order Streaming} *)

(** Placeholder for async content *)
let placeholder ~id ~fallback =
  Printf.sprintf {|<template data-svelte-placeholder="%s">%s</template>|}
    id
    fallback

(** Replacement for async content *)
let replacement ~id content =
  Printf.sprintf {|<script>
(function(){
  var p=document.querySelector('[data-svelte-placeholder="%s"]');
  if(p){
    var d=document.createElement('div');
    d.innerHTML='%s';
    while(d.firstChild)p.parentNode.insertBefore(d.firstChild,p);
    p.remove();
  }
})();
</script>|}
    id
    (Data.serialize (`String content))

(** {1 Suspense Support} *)

(** Suspense boundary *)
let suspense ~id ~fallback =
  Printf.sprintf {|<div data-svelte-suspense="%s" style="display:contents">%s</div>|}
    id
    fallback

(** Suspense resolution *)
let resolve_suspense ~id content =
  Printf.sprintf {|<script>
(function(){
  var s=document.querySelector('[data-svelte-suspense="%s"]');
  if(s){s.innerHTML='%s';}
})();
</script>|}
    id
    (Data.serialize (`String content))

(** {1 SSE Streaming} *)

(** Format chunk as SSE event *)
let sse_event ~event ~data =
  Printf.sprintf "event: %s\ndata: %s\n\n" event data

(** Stream as SSE *)
let to_sse stream =
  List.map (fun chunk ->
    match chunk with
    | Shell html -> sse_event ~event:"shell" ~data:(Data.serialize (`String html))
    | Head content -> sse_event ~event:"head" ~data:(Data.serialize (`String content))
    | Html content -> sse_event ~event:"html" ~data:(Data.serialize (`String content))
    | Script content -> sse_event ~event:"script" ~data:(Data.serialize (`String content))
    | Complete -> sse_event ~event:"complete" ~data:"{}"
    | Error msg -> sse_event ~event:"error" ~data:(Data.serialize (`String msg))
  ) stream.chunks
  |> String.concat ""

(** {1 Response Helpers} *)

(** Create streaming response (simplified - returns string) *)
let streaming_response stream ~title ~entry_script =
  let shell_html = shell ~title ~entry_script () in
  let content = render_chunks stream in
  let end_html = shell_end ~entry_script in
  shell_html ^ content ^ end_html

(** Create SSE response headers *)
let sse_headers = [
  ("Content-Type", "text/event-stream");
  ("Cache-Control", "no-cache");
  ("Connection", "keep-alive");
]

(** {1 Error Recovery} *)

(** Error boundary wrapper *)
let error_boundary ~fallback content =
  Printf.sprintf {|<div data-svelte-error-boundary>
  <div data-svelte-error-content>%s</div>
  <template data-svelte-error-fallback>%s</template>
</div>|}
    content
    fallback

(** Error recovery script *)
let error_recovery_script =
  {|<script>
document.querySelectorAll('[data-svelte-error-boundary]').forEach(b => {
  const content = b.querySelector('[data-svelte-error-content]');
  const fallback = b.querySelector('[data-svelte-error-fallback]');
  if (content.querySelector('[data-svelte-error]')) {
    content.innerHTML = fallback.innerHTML;
  }
});
</script>|}

(** {1 Serialization} *)

(** Chunk to JSON *)
let chunk_to_json = function
  | Shell html -> `Assoc [("type", `String "shell"); ("html", `String html)]
  | Head content -> `Assoc [("type", `String "head"); ("content", `String content)]
  | Html content -> `Assoc [("type", `String "html"); ("content", `String content)]
  | Script content -> `Assoc [("type", `String "script"); ("content", `String content)]
  | Complete -> `Assoc [("type", `String "complete")]
  | Error msg -> `Assoc [("type", `String "error"); ("message", `String msg)]

(** Stream to JSON *)
let stream_to_json stream =
  `Assoc [
    ("chunks", `List (List.map chunk_to_json stream.chunks));
    ("completed", `Bool stream.completed);
    ("error", match stream.error with Some e -> `String e | None -> `Null);
  ]
