(** Lightweight request tracing with W3C Trace Context support.

    Provides span-based tracing without OpenTelemetry SDK dependency.
    Spans are stored in the request context (Hmap) via {!Middleware.tracing}.

    {b Usage:}
    {[
      (* Add tracing middleware *)
      let app = Middleware.pipeline [
        Trace.middleware ();
        my_handler
      ]

      (* Create child spans in handlers *)
      let handler req =
        let span = Trace.child_span req ~name:"db_query" in
        let result = do_query () in
        Trace.finish span;
        Response.json result
    ]}
*)

(** {1 Types} *)

(** Span identifier (16 hex chars). *)
type span_id = string

(** Trace identifier (32 hex chars). *)
type trace_id = string

(** Trace flags (sampling). *)
type trace_flags = int

(** A single trace span. *)
type span = {
  trace_id : trace_id;
  span_id : span_id;
  parent_id : span_id option;
  name : string;
  start_time : float;
  mutable end_time : float option;
  mutable status : [ `Ok | `Error of string ];
  mutable attributes : (string * string) list;
}

(** {1 ID Generation} *)

let hex_chars = "0123456789abcdef"

let random_hex n =
  let buf = Bytes.create n in
  for i = 0 to n - 1 do
    Bytes.set buf i hex_chars.[Random.int 16]
  done;
  Bytes.to_string buf

let new_trace_id () = random_hex 32
let new_span_id () = random_hex 16

(** {1 Span Operations} *)

let start ~name ?trace_id ?parent_id () =
  let trace_id = match trace_id with Some id -> id | None -> new_trace_id () in
  {
    trace_id;
    span_id = new_span_id ();
    parent_id;
    name;
    start_time = Unix.gettimeofday ();
    end_time = None;
    status = `Ok;
    attributes = [];
  }

let finish span =
  span.end_time <- Some (Unix.gettimeofday ())

let set_attr span ~key ~value =
  span.attributes <- (key, value) :: span.attributes

let set_error span msg =
  span.status <- `Error msg

let duration span =
  let end_t = match span.end_time with Some t -> t | None -> Unix.gettimeofday () in
  end_t -. span.start_time

(** {1 W3C Trace Context} *)

(** Parse a traceparent header: [version-trace_id-parent_id-flags]. *)
let parse_traceparent header =
  match String.split_on_char '-' header with
  | [_version; tid; pid; _flags]
    when String.length tid = 32 && String.length pid = 16 ->
    Some (tid, pid)
  | _ -> None

(** Format a traceparent header value. *)
let to_traceparent span =
  Printf.sprintf "00-%s-%s-01" span.trace_id span.span_id

(** {1 Context Integration} *)

(** Hmap key for the current span. *)
let span_key : span Hmap.key = Hmap.Key.create ()

(** Get the current span from a request, if tracing is active. *)
let current_span req =
  Hmap.find span_key (Request.ctx req)

(** Create a child span under the current request span. *)
let child_span req ~name =
  match current_span req with
  | Some parent ->
    start ~name ~trace_id:parent.trace_id ~parent_id:parent.span_id ()
  | None ->
    start ~name ()

(** {1 Exporter Interface} *)

(** Exporter receives finished spans. *)
module type EXPORTER = sig
  val export : span -> unit
end

(** Stderr exporter: prints spans to stderr. *)
module Stderr_exporter : EXPORTER = struct
  let export span =
    let dur_ms = duration span *. 1000.0 in
    let status_str = match span.status with `Ok -> "OK" | `Error e -> "ERR:" ^ e in
    Printf.eprintf "[trace] %s %s %.1fms %s\n%!"
      span.trace_id span.name dur_ms status_str
end

(** JSON exporter: outputs spans as JSON to stderr. *)
module Json_exporter : EXPORTER = struct
  let export span =
    let dur_ms = duration span *. 1000.0 in
    let status_str = match span.status with `Ok -> "ok" | `Error e -> e in
    let parent = match span.parent_id with Some p -> Printf.sprintf {|"%s"|} p | None -> "null" in
    let attrs = match span.attributes with
      | [] -> "{}"
      | kvs ->
        let pairs = List.map (fun (k, v) ->
          Printf.sprintf {|"%s":"%s"|} k v
        ) kvs in
        "{" ^ String.concat "," pairs ^ "}"
    in
    Printf.eprintf {|{"trace_id":"%s","span_id":"%s","parent_id":%s,"name":"%s","duration_ms":%.3f,"status":"%s","attributes":%s}|} span.trace_id span.span_id parent span.name dur_ms status_str attrs;
    Printf.eprintf "\n%!"
end

(** {1 Tracing Middleware} *)

(** Create a tracing middleware with the given exporter.
    Automatically creates a root span per request, propagates traceparent,
    and exports the span on completion. *)
let middleware ?(exporter : (module EXPORTER) option) () : Middleware.t =
  fun handler req ->
    (* Parse incoming traceparent *)
    let parent_ctx = match Request.header "traceparent" req with
      | Some tp -> parse_traceparent tp
      | None -> None
    in
    (* Create root span *)
    let span = match parent_ctx with
      | Some (tid, pid) ->
        start ~name:"request" ~trace_id:tid ~parent_id:pid ()
      | None ->
        start ~name:"request" ()
    in
    let meth = Http.Method.to_string (Request.meth req) in
    let path = Request.path req in
    set_attr span ~key:"http.method" ~value:meth;
    set_attr span ~key:"http.path" ~value:path;
    (* Store span in request context *)
    let ctx = Hmap.add span_key span (Request.ctx req) in
    let req = Request.with_ctx ctx req in
    (* Call handler *)
    let resp =
      try handler req
      with exn ->
        set_error span (Printexc.to_string exn);
        finish span;
        (match exporter with Some (module E : EXPORTER) -> E.export span | None -> ());
        raise exn
    in
    (* Finish and export *)
    let status_code = Cohttp.Code.code_of_status (Response.status resp) in
    set_attr span ~key:"http.status_code" ~value:(string_of_int status_code);
    if status_code >= 500 then
      set_error span (Printf.sprintf "HTTP %d" status_code);
    finish span;
    (match exporter with Some (module E : EXPORTER) -> E.export span | None -> ());
    (* Add traceparent to response *)
    Response.with_header "traceparent" (to_traceparent span) resp
