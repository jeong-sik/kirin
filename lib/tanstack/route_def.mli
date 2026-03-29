type http_method =
  | GET
  | POST
  | PUT
  | PATCH
  | DELETE

val method_to_string : http_method -> string

type param_type =
  | PString
  | PInt
  | PUuid
  | PSlug

val param_type_to_string : param_type -> string

type param =
  { name : string
  ; param_type : param_type
  ; optional : bool
  }

val string_param : string -> param
val int_param : string -> param
val uuid_param : string -> param
val slug_param : string -> param
val optional : param -> param

type 'ctx loader_context =
  { ctx : 'ctx
  ; params : (string * string) list
  ; search_params : (string * string) list
  }

type 'ctx action_context =
  { ctx : 'ctx
  ; params : (string * string) list
  ; method_ : http_method
  ; body : Yojson.Safe.t option
  }

type ('ctx, 'data) loader = 'ctx loader_context -> ('data, string) result
type ('ctx, 'result) action = 'ctx action_context -> ('result, string) result

type meta =
  { title : string option
  ; description : string option
  ; tags : string list
  }

val default_meta : meta

type ('ctx, 'loader_data, 'action_result) t =
  { id : string
  ; path : string
  ; params : param list
  ; meta : meta
  ; loader : ('ctx, 'loader_data) loader option
  ; action : ('ctx, 'action_result) action option
  ; children : string list
  }

val create
  :  id:string
  -> path:string
  -> ?params:param list
  -> ?meta:meta
  -> ?loader:('a, 'b) loader
  -> ?action:('a, 'c) action
  -> ?children:string list
  -> unit
  -> ('a, 'b, 'c) t

val with_loader
  :  id:string
  -> path:string
  -> ?params:param list
  -> ?meta:meta
  -> ('a, 'b) loader
  -> ('a, 'b, 'c) t

val with_action
  :  id:string
  -> path:string
  -> ?params:param list
  -> ?meta:meta
  -> ('a, 'b) action
  -> ('a, 'c, 'b) t

val with_loader_action
  :  id:string
  -> path:string
  -> ?params:param list
  -> ?meta:meta
  -> loader:('a, 'b) loader
  -> action:('a, 'c) action
  -> unit
  -> ('a, 'b, 'c) t

val layout : id:string -> path:string -> ?meta:meta -> string list -> ('a, 'b, 'c) t
val parse_path : string -> string list
val is_param_segment : string -> bool
val is_catchall_segment : string -> bool
val param_name : string -> string
val full_path_pattern : ('a, 'b, 'c) t -> string
val params_to_typescript : param list -> string
val route_id_type : ('a, 'b, 'c) t list -> string
