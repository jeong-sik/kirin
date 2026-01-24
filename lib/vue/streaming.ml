(** Vue/Nuxt Streaming SSR

    React 18/Vue 3-style streaming SSR with Suspense boundaries. *)

(** {1 Streaming Types} *)

(** Stream state *)
type state =
  | Pending
  | Streaming
  | Complete
  | Error of string

(** Chunk type *)
type chunk_type =
  | Shell        (* Initial HTML shell *)
  | Suspense     (* Suspense boundary content *)
  | Island       (* Island hydration data *)
  | Script       (* Inline script *)
  | Flush        (* Flush marker *)
  | End          (* Stream end marker *)

(** Stream chunk *)
type chunk = {
  chunk_type: chunk_type;
  content: string;
  id: string option;
  priority: int;
}

(** {1 Stream Configuration} *)

(** Stream config *)
type config = {
  flush_interval_ms: int;
  shell_timeout_ms: int;
  suspense_timeout_ms: int;
  enable_progressive: bool;
  preload_hints: bool;
}

(** Default config *)
let default_config = {
  flush_interval_ms = 16;  (* ~60fps *)
  shell_timeout_ms = 3000;
  suspense_timeout_ms = 5000;
  enable_progressive = true;
  preload_hints = true;
}

(** {1 Stream Building} *)

(** Create shell chunk *)
let shell_chunk ~html =
  { chunk_type = Shell; content = html; id = None; priority = 0 }

(** Create suspense chunk *)
let suspense_chunk ~id ~html =
  { chunk_type = Suspense; content = html; id = Some id; priority = 1 }

(** Create island chunk *)
let island_chunk ~id ~props =
  let content = Yojson.Safe.to_string props in
  { chunk_type = Island; content; id = Some id; priority = 2 }

(** Create script chunk *)
let script_chunk ~content =
  { chunk_type = Script; content; id = None; priority = 3 }

(** Create flush marker *)
let flush_chunk () =
  { chunk_type = Flush; content = ""; id = None; priority = 10 }

(** Create end marker *)
let end_chunk () =
  { chunk_type = End; content = ""; id = None; priority = 100 }

(** {1 Chunk Rendering} *)

(** Render chunk to HTML *)
let render_chunk chunk =
  match chunk.chunk_type with
  | Shell -> chunk.content
  | Suspense ->
    let id = Option.value ~default:"suspense" chunk.id in
    Printf.sprintf {|<template id="%s">%s</template>
<script>
(function(){
  var t = document.getElementById('%s');
  if (t) {
    var p = t.previousElementSibling;
    if (p && p.dataset.suspense === '%s') {
      p.innerHTML = t.innerHTML;
      t.remove();
    }
  }
})();
</script>|} id chunk.content id id
  | Island ->
    let id = Option.value ~default:"island" chunk.id in
    Printf.sprintf {|<script>
window.__NUXT_ISLANDS__ = window.__NUXT_ISLANDS__ || {};
window.__NUXT_ISLANDS__['%s'] = %s;
</script>|} id chunk.content
  | Script ->
    Printf.sprintf "<script>%s</script>" chunk.content
  | Flush -> ""
  | End -> ""

(** {1 Stream Protocol} *)

(** Encode for chunked transfer *)
let encode_chunked content =
  let len = String.length content in
  if len = 0 then ""
  else Printf.sprintf "%x\r\n%s\r\n" len content

(** Encode end of chunked transfer *)
let encode_chunked_end () =
  "0\r\n\r\n"

(** {1 Streaming Headers} *)

(** Get streaming response headers *)
let streaming_headers ?(content_type="text/html; charset=utf-8") () = [
  ("Content-Type", content_type);
  ("Transfer-Encoding", "chunked");
  ("X-Content-Type-Options", "nosniff");
  ("Cache-Control", "no-cache");
]

(** {1 Stream State Machine} *)

(** Stream context *)
type context = {
  config: config;
  mutable state: state;
  mutable chunks_sent: int;
  mutable bytes_sent: int;
  mutable suspense_pending: string list;
  mutable islands_pending: string list;
}

(** Create stream context *)
let create_context ?(config=default_config) () = {
  config;
  state = Pending;
  chunks_sent = 0;
  bytes_sent = 0;
  suspense_pending = [];
  islands_pending = [];
}

(** Start streaming *)
let start ctx =
  ctx.state <- Streaming

(** Mark suspense as pending *)
let add_suspense ctx id =
  ctx.suspense_pending <- id :: ctx.suspense_pending

(** Mark island as pending *)
let add_island ctx id =
  ctx.islands_pending <- id :: ctx.islands_pending

(** Resolve suspense *)
let resolve_suspense ctx id =
  ctx.suspense_pending <- List.filter (fun i -> i <> id) ctx.suspense_pending

(** Resolve island *)
let resolve_island ctx id =
  ctx.islands_pending <- List.filter (fun i -> i <> id) ctx.islands_pending

(** Send chunk *)
let send_chunk ctx chunk =
  ctx.chunks_sent <- ctx.chunks_sent + 1;
  let html = render_chunk chunk in
  ctx.bytes_sent <- ctx.bytes_sent + String.length html;
  encode_chunked html

(** Finish streaming *)
let finish ctx =
  ctx.state <- Complete;
  encode_chunked_end ()

(** Check if complete *)
let is_complete ctx =
  match ctx.state with
  | Complete -> true
  | Error _ -> true
  | _ ->
    List.length ctx.suspense_pending = 0 &&
    List.length ctx.islands_pending = 0

(** {1 SSE (Server-Sent Events)} *)

(** SSE event *)
type sse_event = {
  event_type: string option;
  data: string;
  event_id: string option;
  retry: int option;
}

(** Render SSE event *)
let render_sse_event ev =
  let lines = Buffer.create 256 in
  Option.iter (fun e -> Buffer.add_string lines (Printf.sprintf "event: %s\n" e)) ev.event_type;
  Option.iter (fun id -> Buffer.add_string lines (Printf.sprintf "id: %s\n" id)) ev.event_id;
  Option.iter (fun r -> Buffer.add_string lines (Printf.sprintf "retry: %d\n" r)) ev.retry;
  (* Data can have multiple lines *)
  String.split_on_char '\n' ev.data
  |> List.iter (fun line -> Buffer.add_string lines (Printf.sprintf "data: %s\n" line));
  Buffer.add_char lines '\n';
  Buffer.contents lines

(** Create SSE event for chunk *)
let chunk_to_sse chunk =
  let event_type = match chunk.chunk_type with
    | Shell -> "shell"
    | Suspense -> "suspense"
    | Island -> "island"
    | Script -> "script"
    | Flush -> "flush"
    | End -> "end"
  in
  {
    event_type = Some event_type;
    data = render_chunk chunk;
    event_id = chunk.id;
    retry = None;
  }

(** {1 Serialization} *)

(** Chunk type to string *)
let chunk_type_to_string = function
  | Shell -> "shell"
  | Suspense -> "suspense"
  | Island -> "island"
  | Script -> "script"
  | Flush -> "flush"
  | End -> "end"

(** State to string *)
let state_to_string = function
  | Pending -> "pending"
  | Streaming -> "streaming"
  | Complete -> "complete"
  | Error msg -> Printf.sprintf "error: %s" msg

(** Context to JSON *)
let context_to_json ctx =
  `Assoc [
    ("state", `String (state_to_string ctx.state));
    ("chunksSent", `Int ctx.chunks_sent);
    ("bytesSent", `Int ctx.bytes_sent);
    ("suspensePending", `List (List.map (fun s -> `String s) ctx.suspense_pending));
    ("islandsPending", `List (List.map (fun s -> `String s) ctx.islands_pending));
  ]

(** Config to JSON *)
let config_to_json c =
  `Assoc [
    ("flushIntervalMs", `Int c.flush_interval_ms);
    ("shellTimeoutMs", `Int c.shell_timeout_ms);
    ("suspenseTimeoutMs", `Int c.suspense_timeout_ms);
    ("enableProgressive", `Bool c.enable_progressive);
    ("preloadHints", `Bool c.preload_hints);
  ]
