type param_type =
  | String
  | Int
  | Uuid
  | Slug
  | Optional of param_type
  | Rest
  | Matcher of string

type param =
  { name : string
  ; param_type : param_type
  ; required : bool
  }

type load_result =
  | Data of Yojson.Safe.t
  | Redirect of string * int
  | Error of int * string
  | NotFound

type action_result =
  | Success of Yojson.Safe.t
  | Fail of int * Yojson.Safe.t
  | ActionRedirect of string
  | ActionError of string

type action =
  { action_name : string option
  ; handler : url:string -> form_data:Yojson.Safe.t -> action_result
  }

type loader =
  { load_server : bool
  ; depends : string list
  ; handler : url:string -> params:(string * string) list -> load_result
  }

type layout =
  { layout_id : string
  ; loader : loader option
  ; reset : bool
  }

type page =
  { loader : loader option
  ; actions : action list
  ; prerender : bool option
  ; ssr : bool
  ; csr : bool
  }

type t =
  { path : string
  ; pattern : string
  ; params : param list
  ; page : page option
  ; layout : layout option
  ; error : bool
  ; group : string option
  }

val page
  :  ?prerender:bool option
  -> ?ssr:bool
  -> ?csr:bool
  -> ?loader:loader
  -> ?actions:action list
  -> string
  -> t

val layout : ?reset:bool -> ?loader:loader -> string -> t
val error : string -> t
val with_param : string -> param_type -> t -> t
val with_optional_param : string -> param_type -> t -> t
val with_rest_param : string -> t -> t
val in_group : string -> t -> t

val server_loader
  :  ?depends:string list
  -> (url:string -> params:(string * string) list -> load_result)
  -> loader

val universal_loader
  :  ?depends:string list
  -> (url:string -> params:(string * string) list -> load_result)
  -> loader

val default_action : (url:string -> form_data:Yojson.Safe.t -> action_result) -> action

val named_action
  :  string
  -> (url:string -> form_data:Yojson.Safe.t -> action_result)
  -> action

val data : Yojson.Safe.t -> load_result
val redirect : ?status:int -> string -> load_result
val error_response : int -> string -> load_result
val not_found : load_result
val success : ?data:Yojson.Safe.t -> unit -> action_result
val fail : ?status:int -> Yojson.Safe.t -> action_result
val action_redirect : string -> action_result
val action_error : string -> action_result

val param_type_to_json
  :  param_type
  -> ([> `Assoc of (string * 'a) list | `String of string ] as 'a)

val to_json
  :  t
  -> [> `Assoc of
          (string
          * [> `Assoc of
                 (string * [> `Bool of bool | `Int of int | `String of string ]) list
            | `Bool of bool
            | `List of
                [> `Assoc of
                     (string
                     * ([> `Assoc of (string * 'a) list
                        | `Bool of bool
                        | `String of string
                        ]
                        as
                        'a))
                       list
                ]
                  list
            | `String of string
            ])
            list
     ]
