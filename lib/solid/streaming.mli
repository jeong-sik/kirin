type event =
    ShellReady of string
  | Chunk of string
  | Script of string
  | Complete
  | Error of string
type callback = event -> unit
type options = {
  buffer_size : int;
  flush_interval_ms : int;
  abort_delay_ms : int;
  progressive : bool;
}
val default_options : options
val shell_with_placeholder :
  head:string ->
  body_start:string -> suspense_fallbacks:string list -> string
val replacement_script : suspense_id:string -> content:string -> string
val runtime_script : string
val parse_stream_line : string -> Protocol.stream_chunk
val response_start : content_type:string -> (string * string) list
val format_chunk : string -> string
val end_chunk : string
val sse_event : event_type:string -> data:string -> string
val sse_stream :
  engine:Ssr.t ->
  url:string -> ?props:Yojson.Safe.t -> unit -> string
type priority = Immediate | Visible | Idle | Interaction
val priority_to_string : priority -> string
val progressive_marker : component_id:string -> priority:priority -> string
val progressive_script : string
val ooo_placeholder : id:string -> fallback:string -> string
val ooo_replacement : id:string -> content:string -> string
val error_boundary_fallback :
  boundary_id:string -> error_message:string -> string
val error_recovery_script : boundary_id:string -> string
