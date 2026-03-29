type data = (string, string) Hashtbl.t

type entry =
  { data : data
  ; created_at : float
  ; mutable last_accessed : float
  ; mutable expires_at : float option
  }

type store =
  { get : string -> entry option
  ; set : string -> entry -> unit
  ; delete : string -> unit
  ; cleanup : unit -> unit
  }

val bytes_to_hex : string -> string
val generate_id : unit -> string
val default_cookie_name : string
val create_memory_store : ?max_age:float -> unit -> store
val create : store -> ?expires_in:float -> unit -> string
val get_entry : store -> string -> entry option
val set : store -> string -> string -> string -> bool
val get : store -> string -> string -> string option
val get_all : store -> string -> (string * string) list
val remove : store -> string -> string -> bool
val destroy : store -> string -> unit
val regenerate : store -> string -> string option
val get_id_from_request : ?cookie_name:string -> Kirin.Request.t -> string option

val set_session_cookie
  :  ?cookie_name:string
  -> ?path:string
  -> ?http_only:bool
  -> ?secure:bool
  -> ?same_site:[ `Lax | `None | `Strict ]
  -> ?max_age:int
  -> string
  -> Kirin.Response.t
  -> Kirin.Response.t

val clear_session_cookie
  :  ?cookie_name:string
  -> ?path:string
  -> Kirin.Response.t
  -> Kirin.Response.t

val middleware
  :  ?cookie_name:string
  -> ?create_if_missing:bool
  -> ?secure:bool
  -> store
  -> (Kirin.Request.t -> Kirin.Response.t)
  -> Kirin.Request.t
  -> Kirin.Response.t

val get_id : ?cookie_name:string -> Kirin.Request.t -> string option
