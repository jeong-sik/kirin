type load_context =
  { url : string
  ; params : (string * string) list
  ; route_id : string
  ; parent : unit -> Yojson.Safe.t option
  ; depends : string -> unit
  ; cookies : (string * string) list
  ; request_headers : (string * string) list
  ; platform : Yojson.Safe.t option
  ; locals : Yojson.Safe.t
  }

type load_output =
  | LoadData of Yojson.Safe.t
  | LoadRedirect of int * string
  | LoadError of int * string
  | LoadNotFound

type universal_loader = load_context -> load_output
type server_loader = load_context -> load_output

val create_context
  :  url:string
  -> params:(string * string) list
  -> route_id:string
  -> ?parent:(unit -> Yojson.Safe.t option)
  -> ?cookies:(string * string) list
  -> ?headers:(string * string) list
  -> ?platform:Yojson.Safe.t
  -> ?locals:Yojson.Safe.t
  -> unit
  -> load_context

val param : load_context -> string -> string option
val param_exn : load_context -> string -> string
val param_int : load_context -> string -> int option
val cookie : load_context -> string -> string option
val header : load_context -> string -> string option
val data : Yojson.Safe.t -> load_output
val redirect : ?status:int -> string -> load_output
val error : int -> string -> load_output
val not_found : load_output

type depends_on =
  | Url
  | Param of string
  | Custom of string

val register_depends : load_context -> depends_on list -> unit
val parallel : ('a -> 'b) list -> 'a -> 'b list
val with_timeout : 'a -> ('b -> 'c) -> 'b -> 'c

type cache_control =
  { max_age : int option
  ; s_maxage : int option
  ; stale_while_revalidate : int option
  ; private_cache : bool
  ; no_store : bool
  }

val default_cache : cache_control
val cache_for : int -> cache_control
val cdn_cache_for : int -> cache_control
val no_cache : cache_control
val cache_header : cache_control -> string

type stream_chunk =
  | Initial of Yojson.Safe.t
  | Update of string * Yojson.Safe.t
  | StreamError of string
  | StreamComplete

val streaming : Yojson.Safe.t -> (string * Yojson.Safe.t) list -> stream_chunk list
val output_to_json : load_output -> [> `Assoc of (string * Yojson.Safe.t) list ]

val context_to_json
  :  load_context
  -> [> `Assoc of
          (string
          * [> `Assoc of (string * [> `String of string ]) list | `String of string ])
            list
     ]
