type capture =
  | NoCapture
  | Capture of string list
  | CaptureRef of int list

type t =
  { chunk : string
  ; symbol : string
  ; capture : capture
  ; dev_name : string option
  }

val create
  :  chunk:string
  -> symbol:string
  -> ?capture:capture
  -> ?dev_name:string
  -> unit
  -> t

val of_string : chunk:string -> symbol:string -> t
val with_captures : chunk:string -> symbol:string -> string list -> t
val with_refs : chunk:string -> symbol:string -> int list -> t
val to_string : t -> string
val of_serialized : string -> t option
val on_click : t -> string
val on_input : t -> string
val on_submit : t -> string
val on_event : event:string -> t -> string
val on_window : event:string -> t -> string
val on_document : event:string -> t -> string

type prefetch_strategy =
  | PrefetchEvent
  | PrefetchVisible
  | PrefetchLoad
  | PrefetchNone

val prefetch_hint : ?strategy:prefetch_strategy -> t -> string

type component_qrl =
  { qrl : t
  ; styles : string list
  ; slot_projection : bool
  }

val component
  :  chunk:string
  -> symbol:string
  -> ?styles:string list
  -> ?slot_projection:bool
  -> unit
  -> component_qrl

val to_json
  :  t
  -> [> `Assoc of
          (string
          * [> `List of [> `Int of int | `String of string ] list
            | `Null
            | `String of string
            ])
            list
     ]

val of_json : Yojson__Safe.t -> t
