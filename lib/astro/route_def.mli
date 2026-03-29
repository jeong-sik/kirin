type param_type =
  | String
  | Rest
  | Optional

type param =
  { name : string
  ; param_type : param_type
  }

type prerender =
  | Static
  | OnDemand
  | Hybrid

type t =
  { path : string
  ; name : string option
  ; prerender : prerender
  ; islands : Island.t list
  ; layout : string option
  ; middleware : string list
  ; get_static_paths : bool
  }

val static : string -> t
val ssr : string -> t
val hybrid : string -> t
val with_name : string -> t -> t
val with_island : Island.t -> t -> t
val with_islands : Island.t list -> t -> t
val with_layout : string -> t -> t
val with_middleware : string -> t -> t
val with_static_paths : t -> t
val extract_params : string -> param list
val get_params : t -> string list
val is_dynamic : t -> bool
val has_rest_params : t -> bool
val matches : t -> string -> (string * string) list option
val prerender_to_string : prerender -> string

val to_json
  :  t
  -> [> `Assoc of
          (string
          * [> `Bool of bool
            | `List of
                [> `Assoc of
                     (string
                     * [> `Assoc of (string * Yojson.Safe.t) list | `String of string ])
                       list
                | `String of string
                ]
                  list
            | `Null
            | `String of string
            ])
            list
     ]
