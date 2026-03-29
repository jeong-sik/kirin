type param_type =
  | String
  | Int
  | Uuid
  | Slug
  | Optional of param_type

type param =
  { name : string
  ; param_type : param_type
  ; required : bool
  }

val string_param : string -> param
val int_param : string -> param
val uuid_param : string -> param
val optional_param : string -> param_type -> param

type fetch_options =
  { key : string option
  ; server : bool
  ; lazy_ : bool
  ; immediate : bool
  ; watch : string list
  ; default : Yojson.Safe.t option
  ; transform : string option
  ; pick : string list
  ; cache : bool
  ; dedupe : string option
  }

val default_fetch_options : fetch_options

type middleware =
  { name : string
  ; global : bool
  ; handler : string
  }

val middleware : ?global:bool -> string -> string -> middleware
val global_middleware : string -> string -> middleware

type page_meta =
  { title : string option
  ; description : string option
  ; layout : string option
  ; middleware : string list
  ; keep_alive : bool
  ; key : string option
  ; page_transition : string option
  ; layout_transition : string option
  ; alias : string list
  ; redirect : string option
  }

val default_page_meta : page_meta

type loader_type =
  | UseFetch of string
  | UseAsyncData of string
  | ServerLoader
  | UniversalLoader

type t =
  { path : string
  ; name : string option
  ; component : string
  ; params : param list
  ; loader : loader_type option
  ; fetch_options : fetch_options
  ; middleware : middleware list
  ; children : t list
  ; meta : page_meta
  ; is_layout : bool
  ; layout_name : string option
  }

val page : string -> t
val layout : string -> t
val with_param : string -> param_type -> t -> t
val with_fetch : string -> t -> t
val with_async_data : string -> t -> t
val with_server_loader : t -> t
val with_fetch_options : fetch_options -> t -> t
val with_middleware : middleware -> t -> t
val with_children : t list -> t -> t
val with_meta : page_meta -> t -> t
val with_layout : string -> t -> t
val with_title : string -> t -> t
val with_alias : string -> t -> t
val with_redirect : string -> t -> t
val param_type_to_string : param_type -> string
val loader_type_to_string : loader_type -> string

val to_json
  :  t
  -> ([> `Assoc of
           (string * [> `Bool of bool | `List of 'a list | `String of string ]) list
      | `String of string
      ]
      as
      'a)
