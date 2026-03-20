type swap_strategy =
    True
  | Inner_html
  | Outer_html
  | Before_begin
  | After_begin
  | Before_end
  | After_end
  | Delete
  | None
val strategy_to_string : swap_strategy -> string
val attr : ?strategy:swap_strategy -> ?target:string -> unit -> string
val div : ?strategy:swap_strategy -> id:string -> string -> string
val div_target : strategy:swap_strategy -> target:string -> string -> string
val notification : id:string -> ?class_:string -> string -> string
val success_message : id:string -> string -> string
val error_message : id:string -> string -> string
val clear : id:string -> string
val delete : id:string -> string
val multi : string list -> string
val field_error : field_id:string -> string -> string
val clear_field_error : field_id:string -> string
val field_with_state : field_id:string -> state:string -> string -> string
val counter : id:string -> int -> string
val cart_badge : int -> string
val notification_badge : int -> string
val append_to_list : list_id:string -> string -> string
val prepend_to_list : list_id:string -> string -> string
val replace_item : item_id:string -> string -> string
val remove_item : item_id:string -> string
val response : main_content:string -> string list -> string
val response_oob_only : string list -> string
