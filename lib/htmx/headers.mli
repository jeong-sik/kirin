val trigger : string list -> Kirin.Response.t -> Kirin.Response.t
val trigger_after_settle :
  string list -> Kirin.Response.t -> Kirin.Response.t
val trigger_after_swap : string list -> Kirin.Response.t -> Kirin.Response.t
val trigger_with_data :
  (string * Yojson.Safe.t) list -> Kirin.Response.t -> Kirin.Response.t
val push_url : string -> Kirin.Response.t -> Kirin.Response.t
val replace_url : string -> Kirin.Response.t -> Kirin.Response.t
val redirect : string -> Kirin.Response.t -> Kirin.Response.t
val refresh : Kirin.Response.t -> Kirin.Response.t
val retarget : string -> Kirin.Response.t -> Kirin.Response.t
val reswap : string -> Kirin.Response.t -> Kirin.Response.t
val reselect : string -> Kirin.Response.t -> Kirin.Response.t
type swap =
    Inner_html
  | Outer_html
  | Before_begin
  | After_begin
  | Before_end
  | After_end
  | Delete
  | None
val swap_to_string : swap -> string
val reswap_typed : swap -> Kirin.Response.t -> Kirin.Response.t
val reswap_with_modifiers :
  swap ->
  ?swap_delay:float ->
  ?settle_delay:float ->
  ?scroll:string ->
  ?show:string -> unit -> Kirin.Response.t -> Kirin.Response.t
val location :
  ?target:string ->
  ?swap:swap ->
  ?source:string -> string -> Kirin.Response.t -> Kirin.Response.t
val location_simple : string -> Kirin.Response.t -> Kirin.Response.t
val is_htmx_request : Kirin.Request.t -> bool
val is_boosted : Kirin.Request.t -> bool
val current_url : Kirin.Request.t -> string option
val trigger_id : Kirin.Request.t -> string option
val trigger_name : Kirin.Request.t -> string option
val target_id : Kirin.Request.t -> string option
val prompt_response : Kirin.Request.t -> string option
