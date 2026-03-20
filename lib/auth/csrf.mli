val default_ttl : float
val default_field_name : string
val default_header_name : string
val string_to_hex : string -> string
val generate :
  secret:string -> session_id:string -> ?ttl:float -> unit -> string
val constant_time_compare : string -> string -> bool
val validate_token :
  secret:string -> session_id:string -> string -> (unit, string) result
val get_form_field : string -> Kirin.Request.t -> string option
val get_token_from_request :
  ?field_name:string ->
  ?header_name:string -> Kirin.Request.t -> string option
val validate :
  secret:string ->
  session_id:string ->
  ?field_name:string ->
  ?header_name:string -> Kirin.Request.t -> (unit, string) result
val middleware :
  secret:string ->
  ?field_name:string ->
  ?header_name:string ->
  ?safe_methods:string list ->
  (Kirin.Request.t -> string option) ->
  (Kirin.Request.t -> Kirin.Response.t) ->
  Kirin.Request.t -> Kirin.Response.t
val hidden_input : secret:string -> session_id:string -> unit -> string
val meta_tag : secret:string -> session_id:string -> unit -> string
