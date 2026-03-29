(** @since 1.0.0
    @status experimental
    This module's API may change in future versions. *)

module Loader = Loader
module Action = Action
module Route = Route
module Ssr = Ssr

val loader_context : Kirin.Request.t -> Loader.loader_context
val pure : 'a -> 'b -> 'a Loader.loader_result
val redirect : ?status:int -> string -> 'a -> 'b Loader.loader_result
val not_found : 'a -> 'b Loader.loader_result
val error : string -> 'a -> 'b Loader.loader_result

val parallel
  :  ('a -> 'b Loader.loader_result)
  -> ('a -> 'c Loader.loader_result)
  -> 'a
  -> ('b * 'c) Loader.loader_result

val optional : ('a -> 'b Loader.loader_result) -> 'a -> 'b option Loader.loader_result

val action_context
  :  form_data:Action.form_data
  -> Kirin.Request.t
  -> Action.action_context

val field : Action.action_context -> string -> string option
val required_field : Action.action_context -> string -> string option
val succeed : 'a -> 'b -> 'a Action.action_result
val redirect_action : ?status:int -> string -> 'a -> 'b Action.action_result

val validate
  :  validators:(string * (string -> bool) * string) list
  -> Action.action_context
  -> Action.form_data Action.action_result

val not_empty : string -> bool
val min_length : int -> string -> bool
val max_length : int -> string -> bool
val is_email : string -> bool
val is_numeric : string -> bool

val route
  :  ?loader:(Loader.loader_context -> Yojson.Safe.t Loader.loader_result) option
  -> ?action:(Action.action_context -> Yojson.Safe.t Action.action_result) option
  -> ?error_boundary:bool
  -> ?index:bool
  -> ?children:Route.t list
  -> string
  -> Route.t

val index
  :  ?loader:(Loader.loader_context -> Yojson.Safe.t Loader.loader_result) option
  -> ?action:(Action.action_context -> Yojson.Safe.t Action.action_result) option
  -> unit
  -> Route.t

val layout
  :  ?loader:(Loader.loader_context -> Yojson.Safe.t Loader.loader_result) option
  -> ?error_boundary:bool
  -> Route.t list
  -> Route.t

val with_loader
  :  (Loader.loader_context -> Yojson.Safe.t Loader.loader_result)
  -> Route.t
  -> Route.t

val with_action
  :  (Action.action_context -> Yojson.Safe.t Action.action_result)
  -> Route.t
  -> Route.t

val with_error_boundary : Route.t -> Route.t
val with_children : Route.t list -> Route.t -> Route.t

val find_matches
  :  Route.t list
  -> string list
  -> Route.match_result list
  -> Route.match_result list

val generate_url : Route.t -> (string * string) list -> string
val default_config : Ssr.ssr_config

val render_context
  :  url:string
  -> matches:Route.match_result list
  -> loader_data:(string * Yojson.Safe.t) list
  -> ?action_data:Yojson.Safe.t
  -> ?errors:(string * string) list
  -> unit
  -> Ssr.render_context

val load_data
  :  Route.match_result list
  -> Loader.loader_context
  -> (string * Yojson.Safe.t) list

val render_document
  :  config:Ssr.ssr_config
  -> context:Ssr.render_context
  -> body:string
  -> string

val handler
  :  config:Ssr.ssr_config
  -> routes:Route.t list
  -> render_component:(Ssr.render_context -> string)
  -> Kirin.Request.t
  -> Kirin.Response.t

val hydration_script
  :  loader_data:(string * Yojson.Safe.t) list
  -> action_data:Yojson.Safe.t option
  -> string

val error_boundary : error:string -> route:string -> string
val catch_boundary : status:int -> message:string -> string

val loader_context_to_json
  :  Loader.loader_context
  -> [> `Assoc of
          (string
          * [> `Assoc of (string * [> `String of string ]) list | `String of string ])
            list
     ]

val action_context_to_json
  :  Action.action_context
  -> [> `Assoc of
          (string
          * [> `Assoc of (string * [> `String of string ]) list | `String of string ])
            list
     ]

val route_to_json
  :  Route.t
  -> ([> `Assoc of
           (string * [> `Bool of bool | `List of 'a list | `String of string ]) list
      ]
      as
      'a)

val config_to_json
  :  Ssr.ssr_config
  -> [> `Assoc of (string * [> `Bool of bool | `Float of float | `String of string ]) list
     ]

val render_context_to_json
  :  Ssr.render_context
  -> [> `Assoc of (string * Yojson.Safe.t) list ]
