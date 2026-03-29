type state =
  | Pending
  | Streaming
  | Complete
  | Error of string

type chunk_type =
  | Shell
  | Suspense
  | Island
  | Script
  | Flush
  | End

type chunk =
  { chunk_type : chunk_type
  ; content : string
  ; id : string option
  ; priority : int
  }

type config =
  { flush_interval_ms : int
  ; shell_timeout_ms : int
  ; suspense_timeout_ms : int
  ; enable_progressive : bool
  ; preload_hints : bool
  }

val default_config : config
val shell_chunk : html:string -> chunk
val suspense_chunk : id:string -> html:string -> chunk
val island_chunk : id:string -> props:Yojson.Safe.t -> chunk
val script_chunk : content:string -> chunk
val flush_chunk : unit -> chunk
val end_chunk : unit -> chunk
val render_chunk : chunk -> string
val encode_chunked : string -> string
val encode_chunked_end : unit -> string
val streaming_headers : ?content_type:string -> unit -> (string * string) list

type context =
  { config : config
  ; mutable state : state
  ; mutable chunks_sent : int
  ; mutable bytes_sent : int
  ; mutable suspense_pending : string list
  ; mutable islands_pending : string list
  ; mutex : Eio.Mutex.t
  }

val create_context : ?config:config -> unit -> context
val start : context -> unit
val add_suspense : context -> string -> unit
val add_island : context -> string -> unit
val resolve_suspense : context -> string -> unit
val resolve_island : context -> string -> unit
val send_chunk : context -> chunk -> string
val finish : context -> string
val is_complete : context -> bool

type sse_event =
  { event_type : string option
  ; data : string
  ; event_id : string option
  ; retry : int option
  }

val render_sse_event : sse_event -> string
val chunk_to_sse : chunk -> sse_event
val chunk_type_to_string : chunk_type -> string
val state_to_string : state -> string

val context_to_json
  :  context
  -> [> `Assoc of
          (string
          * [> `Int of int | `List of [> `String of string ] list | `String of string ])
            list
     ]

val config_to_json
  :  config
  -> [> `Assoc of (string * [> `Bool of bool | `Int of int ]) list ]
