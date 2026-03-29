type chunk =
  | Shell of string
  | Head of string
  | Html of string
  | Script of string
  | Complete
  | Error of string

type stream_state =
  { mutable chunks : chunk list
  ; mutable completed : bool
  ; mutable error : string option
  ; mutex : Eio.Mutex.t
  }

val create : unit -> stream_state
val add_chunk : stream_state -> chunk -> unit
val complete : stream_state -> unit
val fail : stream_state -> string -> unit
val shell : title:string -> entry_script:'a -> ?lang:string -> unit -> string
val shell_end : entry_script:string -> string
val render_chunk : chunk -> string
val render_chunks : stream_state -> string

type hydration_priority =
  | Immediate
  | Visible
  | Idle
  | Interaction

val priority_string : hydration_priority -> string
val hydration_marker : id:string -> priority:hydration_priority -> string

val hydration_boundary
  :  id:string
  -> priority:hydration_priority
  -> fallback:string option
  -> string
  -> string

val placeholder : id:string -> fallback:string -> string
val replacement : id:string -> string -> string
val suspense : id:string -> fallback:string -> string
val resolve_suspense : id:string -> string -> string
val sse_event : event:string -> data:string -> string
val to_sse : stream_state -> string
val streaming_response : stream_state -> title:string -> entry_script:string -> string
val sse_headers : (string * string) list
val error_boundary : fallback:string -> string -> string
val error_recovery_script : string
val chunk_to_json : chunk -> [> `Assoc of (string * [> `String of string ]) list ]

val stream_to_json
  :  stream_state
  -> [> `Assoc of
          (string
          * [> `Bool of bool
            | `List of [> `Assoc of (string * [> `String of string ]) list ] list
            | `Null
            | `String of string
            ])
            list
     ]
