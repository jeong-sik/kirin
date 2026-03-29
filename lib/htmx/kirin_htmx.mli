(** @since 1.0.0
    @status experimental
    This module's API may change in future versions. *)

module Headers = Headers
module Hyperscript = Hyperscript
module Alpine = Alpine
module Extensions = Extensions
module Oob = Oob
module Form = Form

val htmx_script : ?version:string -> unit -> string
val hyperscript_script : ?version:string -> unit -> string
val alpine_script : ?version:string -> unit -> string

val all_scripts
  :  ?htmx_ver:string
  -> ?hyperscript_ver:string
  -> ?alpine_ver:string
  -> unit
  -> string

val loading_css : string
val dismissable_alert : id:string -> ?class_:string -> string -> string

val infinite_scroll
  :  url:string
  -> trigger_id:string
  -> ?threshold:string
  -> unit
  -> string

val search_input
  :  url:string
  -> target:string
  -> ?debounce_ms:int
  -> ?placeholder:string
  -> unit
  -> string

val click_to_edit : get_url:string -> id:string -> string -> string

val delete_with_confirm
  :  url:string
  -> target:string
  -> confirm_msg:string
  -> string
  -> string

val lazy_load : url:string -> id:string -> ?placeholder:string -> unit -> string
val is_htmx : Kirin.Request.t -> bool
val is_boosted : Kirin.Request.t -> bool

val response
  :  ?trigger:string list
  -> ?push_url:string
  -> ?retarget:string
  -> ?reswap:Headers.swap
  -> string
  -> Kirin.Response.t
