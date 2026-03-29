type segment =
  | Static of string
  | Dynamic of string
  | Splat
  | Optional of string

type t =
  { path : string
  ; segments : segment list
  ; loader : (Loader.loader_context -> Yojson.Safe.t Loader.loader_result) option
  ; action : (Action.action_context -> Yojson.Safe.t Action.action_result) option
  ; error_boundary : bool
  ; index : bool
  ; children : t list
  }

val parse_segment : string -> segment
val parse_path : string -> segment list

val create
  :  ?loader:(Loader.loader_context -> Yojson.Safe.t Loader.loader_result) option
  -> ?action:(Action.action_context -> Yojson.Safe.t Action.action_result) option
  -> ?error_boundary:bool
  -> ?index:bool
  -> ?children:t list
  -> string
  -> t

val index
  :  ?loader:(Loader.loader_context -> Yojson.Safe.t Loader.loader_result) option
  -> ?action:(Action.action_context -> Yojson.Safe.t Action.action_result) option
  -> unit
  -> t

val layout
  :  ?loader:(Loader.loader_context -> Yojson.Safe.t Loader.loader_result) option
  -> ?error_boundary:bool
  -> t list
  -> t

val with_loader : (Loader.loader_context -> Yojson.Safe.t Loader.loader_result) -> t -> t
val with_action : (Action.action_context -> Yojson.Safe.t Action.action_result) -> t -> t
val with_error_boundary : t -> t
val with_children : t list -> t -> t

type match_result =
  { route : t
  ; params : (string * string) list
  ; pathname : string
  }

val match_segment : segment -> string -> (string * string) list option
val match_route : t -> string list -> (string * string) list option
val find_match : t list -> string list -> match_result option
val find_matches : t list -> string list -> match_result list -> match_result list
val execute_loader : t -> Loader.loader_context -> Yojson.Safe.t Loader.loader_result
val execute_action : t -> Action.action_context -> Yojson.Safe.t Action.action_result

val execute_loaders
  :  match_result list
  -> Loader.loader_context
  -> (string * Yojson.Safe.t Loader.loader_result) list

val generate_url : t -> (string * string) list -> string
val segment_to_json : segment -> [> `Assoc of (string * [> `String of string ]) list ]

val to_json
  :  t
  -> ([> `Assoc of
           (string * [> `Bool of bool | `List of 'a list | `String of string ]) list
      ]
      as
      'a)

val match_to_json
  :  match_result
  -> [> `Assoc of
          (string
          * [> `Assoc of (string * [> `String of string ]) list | `String of string ])
            list
     ]
