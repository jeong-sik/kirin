type 'a fetch_result =
  { data : 'a option
  ; pending : bool
  ; error : string option
  ; status : string
  ; refresh : unit -> unit
  ; clear : unit -> unit
  }

val success : 'a -> 'a fetch_result
val error : string -> 'a fetch_result
val pending : unit -> 'a fetch_result

type context =
  { url : string
  ; method_ : string
  ; params : (string * string) list
  ; query : (string * string) list
  ; headers : (string * string) list
  ; cookies : (string * string) list
  ; body : Yojson.Safe.t option
  ; route_name : string option
  ; is_server : bool
  }

val create_context
  :  url:string
  -> method_:string
  -> ?params:(string * string) list
  -> ?query:(string * string) list
  -> ?headers:(string * string) list
  -> ?cookies:(string * string) list
  -> ?body:Yojson.Safe.t
  -> ?route_name:string
  -> ?is_server:bool
  -> unit
  -> context

val param : context -> string -> string option
val param_exn : context -> string -> string
val query : context -> string -> string option
val header : context -> string -> string option
val cookie : context -> string -> string option

type load_result =
  | LoadData of Yojson.Safe.t
  | LoadRedirect of int * string
  | LoadError of int * string
  | LoadNotFound

val data : Yojson.Safe.t -> load_result
val redirect : ?status:int -> string -> load_result
val error_result : int -> string -> load_result
val not_found : load_result

type fetch_options =
  { key : string option
  ; method_ : string
  ; query : (string * string) list
  ; headers : (string * string) list
  ; body : Yojson.Safe.t option
  ; base_url : string option
  ; server : bool
  ; lazy_ : bool
  ; immediate : bool
  ; default : Yojson.Safe.t option
  ; transform : (Yojson.Safe.t -> Yojson.Safe.t) option
  ; pick : string list
  ; watch : bool
  ; deep : bool
  ; dedupe : string
  ; timeout : int
  ; retry : int
  ; retry_delay : int
  ; on_request : (context -> unit) option
  ; on_response : (Yojson.Safe.t -> unit) option
  ; on_error : (string -> unit) option
  }

val default_fetch_options : fetch_options

type async_options =
  { key : string
  ; server : bool
  ; lazy_ : bool
  ; immediate : bool
  ; default : Yojson.Safe.t option
  ; transform : (Yojson.Safe.t -> Yojson.Safe.t) option
  ; pick : string list
  ; watch : string list
  ; deep : bool
  ; dedupe : string
  ; get_cached_data : (string -> Yojson.Safe.t option) option
  }

val default_async_options : string -> async_options

type server_handler = context -> load_result

val with_error_handling : ('a -> load_result) -> 'a -> load_result

type cache_entry =
  { value : Yojson.Safe.t
  ; timestamp : float
  ; ttl : int
  }

val is_valid : cache_entry -> bool

val context_to_json
  :  context
  -> [> `Assoc of
          (string
          * [> `Assoc of (string * [> `String of string ]) list
            | `Bool of bool
            | `String of string
            ])
            list
     ]

val result_to_json : load_result -> [> `Assoc of (string * Yojson.Safe.t) list ]

val fetch_options_to_json
  :  fetch_options
  -> [> `Assoc of (string * [> `Bool of bool | `Int of int | `String of string ]) list ]
