(** React 18 Streaming SSR

    Supports React's renderToPipeableStream for progressive hydration.
    Sends HTML shell immediately, then streams Suspense boundaries.
*)

(** Streaming render state *)
type stream_state =
  | Pending
  | ShellReady of string    (* Initial HTML shell *)
  | Chunk of string         (* Suspense boundary resolved *)
  | Error of string
  | Complete

(** Streaming session *)
type session = {
  mutable state: stream_state;
  mutable chunks: string list;
  mutable is_complete: bool;
  worker: Node_worker.t option;
}

(** Create streaming session *)
let create_session ?worker () = {
  state = Pending;
  chunks = [];
  is_complete = false;
  worker;
}

(** HTML template parts for streaming *)
let html_start ~title ~head_extra ~styles =
  let style_tags = List.map (fun href ->
    Printf.sprintf {|<link rel="stylesheet" href="%s">|} href
  ) styles |> String.concat "\n" in
  Printf.sprintf {|<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>%s</title>
%s
%s
</head>
<body>
<div id="root">|} title head_extra style_tags

let html_end ~scripts ~initial_data =
  let script_tags = List.map (fun src ->
    Printf.sprintf {|<script type="module" src="%s"></script>|} src
  ) scripts |> String.concat "\n" in
  let data_script = match initial_data with
    | Some data -> Data.script_tag data
    | None -> ""
  in
  Printf.sprintf {|</div>
%s
%s
</body>
</html>|} data_script script_tags

(** Suspense boundary completion script *)
let suspense_complete_script ~boundary_id ~html =
  let escaped_html = String.escaped html in
  Printf.sprintf {|<script>
(function(){
  var b = document.getElementById("B:%s");
  if(b) {
    var t = document.createElement('template');
    t.innerHTML = "%s";
    b.parentNode.replaceChild(t.content, b);
  }
})();
</script>|} boundary_id escaped_html

(** Streaming write function type *)
type write_fn = string -> unit

(** Render to stream with chunked transfer *)
let render_stream ~engine ~url ?(props = `Assoc []) ~write () =
  let session = create_session () in

  (* Send initial shell *)
  let shell_html = html_start
    ~title:"Loading..."
    ~head_extra:""
    ~styles:[]
  in
  write shell_html;

  (* Render main content *)
  (match Ssr.render engine ~url ~props () with
  | Ok html ->
    write html;
    session.state <- ShellReady html
  | Error msg ->
    write (Printf.sprintf "<div class=\"error\">%s</div>" msg);
    session.state <- Error msg);

  (* Send closing tags *)
  write (html_end ~scripts:[] ~initial_data:None);
  session.is_complete <- true;
  session

(** Streaming configuration *)
type stream_config = {
  buffer_size: int;
  flush_interval_ms: int;
  on_shell_ready: (string -> unit) option;
  on_all_ready: (unit -> unit) option;
  on_error: (string -> unit) option;
}

let default_stream_config = {
  buffer_size = 16384;
  flush_interval_ms = 10;
  on_shell_ready = None;
  on_all_ready = None;
  on_error = None;
}

(** Progressive hydration helpers *)
module Progressive = struct
  (** Generate placeholder for lazy component *)
  let lazy_placeholder ~id ~fallback =
    Printf.sprintf {|<div id="lazy:%s" data-loading="true">%s</div>|} id fallback

  (** Generate hydration marker *)
  let hydration_marker ~id =
    Printf.sprintf {|<!--$hydrate:%s-->|} id

  (** Generate script to hydrate a component *)
  let hydrate_script ~id ~props =
    let props_json = Yojson.Safe.to_string props in
    Printf.sprintf {|<script>
window.__HYDRATE_QUEUE__ = window.__HYDRATE_QUEUE__ || [];
window.__HYDRATE_QUEUE__.push({id:"%s",props:%s});
</script>|} id props_json
end

(** SSE (Server-Sent Events) streaming *)
module SSE = struct
  (** Format SSE event *)
  let event ~name ~data =
    Printf.sprintf "event: %s\ndata: %s\n\n" name data

  (** Shell ready event *)
  let shell_ready html =
    let encoded = Base64.encode_string html in
    event ~name:"shell" ~data:encoded

  (** Chunk event *)
  let chunk ~boundary_id ~html =
    let json = `Assoc [
      ("id", `String boundary_id);
      ("html", `String (Base64.encode_string html));
    ] in
    event ~name:"chunk" ~data:(Yojson.Safe.to_string json)

  (** Complete event *)
  let complete () =
    event ~name:"complete" ~data:""

  (** Error event *)
  let error msg =
    event ~name:"error" ~data:msg

  (** Create SSE response *)
  let response ~engine ~url ?(props = `Assoc []) () =
    (* Note: In practice, this would stream via Eio *)
    match Ssr.render engine ~url ~props () with
    | Ok html ->
      let events = [
        shell_ready html;
        complete ();
      ] in
      let body = String.concat "" events in
      Kirin.Response.make ~status:`OK (`String body)
      |> Kirin.Response.with_header "Content-Type" "text/event-stream"
      |> Kirin.Response.with_header "Cache-Control" "no-cache"
      |> Kirin.Response.with_header "Connection" "keep-alive"
    | Error msg ->
      let body = error msg in
      Kirin.Response.make ~status:`Internal_server_error (`String body)
      |> Kirin.Response.with_header "Content-Type" "text/event-stream"
end

(** React Flight protocol support (future) *)
module Flight = struct
  type chunk_type =
    | Module
    | Hint
    | Model
    | Error_chunk

  (* Placeholder for React Server Components *)
  let _encode_chunk _chunk_type _data = ""
end

(** Render to Kirin streaming response *)
let kirin_stream_response ~engine ~url ?(props = `Assoc []) () =
  (* For now, return non-streaming response *)
  (* Full streaming would require Eio stream integration *)
  match Ssr.render engine ~url ~props () with
  | Ok html ->
    Kirin.Response.html html
    |> Kirin.Response.with_header "Transfer-Encoding" "chunked"
  | Error msg ->
    Kirin.Response.text ~status:`Internal_server_error ("SSR Error: " ^ msg)
