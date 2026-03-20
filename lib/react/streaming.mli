type stream_state =
    Pending
  | ShellReady of string
  | Chunk of string
  | Error of string
  | Complete
type session = {
  mutable state : stream_state;
  mutable chunks : string list;
  mutable is_complete : bool;
  worker : Node_worker.t option;
}
val create_session : ?worker:Node_worker.t -> unit -> session
val html_start :
  title:string -> head_extra:string -> styles:string list -> string
val html_end :
  scripts:string list -> initial_data:Yojson.Safe.t option -> string
val suspense_complete_script : boundary_id:string -> html:string -> string
type write_fn = string -> unit
val render_stream :
  engine:Ssr.t ->
  url:string ->
  ?props:Yojson.Safe.t -> write:(string -> unit) -> unit -> session
type stream_config = {
  buffer_size : int;
  flush_interval_ms : int;
  on_shell_ready : (string -> unit) option;
  on_all_ready : (unit -> unit) option;
  on_error : (string -> unit) option;
}
val default_stream_config : stream_config
module Progressive :
  sig
    val lazy_placeholder : id:string -> fallback:string -> string
    val hydration_marker : id:string -> string
    val hydrate_script : id:string -> props:Yojson.Safe.t -> string
  end
module SSE :
  sig
    val event : name:string -> data:string -> string
    val shell_ready : string -> string
    val chunk : boundary_id:string -> html:string -> string
    val complete : unit -> string
    val error : string -> string
    val response :
      engine:Ssr.t ->
      url:string -> ?props:Yojson.Safe.t -> unit -> Kirin.Response.t
  end
module Flight :
  sig
    type chunk_type = Module | Hint | Model | Error_chunk
    val _encode_chunk : 'a -> 'b -> string
  end
val kirin_stream_response :
  engine:Ssr.t ->
  url:string -> ?props:Yojson.Safe.t -> unit -> Kirin.Response.t
