val escape_for_html_embed : string -> string
val serialize : Yojson.Safe.t -> string
val script_tag : ?var_name:string -> Yojson.Safe.t -> string
val named_script_tag : name:string -> Yojson.Safe.t -> string
val multi_script_tags : (string * Yojson.Safe.t) list -> string

type payload =
  { initial_data : Yojson.Safe.t option
  ; dehydrated_state : Yojson.Safe.t option
  ; route_data : Yojson.Safe.t option
  ; user_data : Yojson.Safe.t option
  ; config_data : Yojson.Safe.t option
  }

val empty_payload : payload
val render_payload : payload -> string
val payload_from_assoc : (string * Yojson.Safe.t) list -> payload
val merge_payloads : payload -> payload -> payload

module QueryClient : sig
  type query =
    { query_key : string list
    ; state : Yojson.Safe.t
    }

  type dehydrated =
    { queries : query list
    ; mutations : Yojson.Safe.t list
    }

  val empty_dehydrated : dehydrated
  val add_query : key:string list -> data:Yojson.Safe.t -> dehydrated -> dehydrated

  val to_json
    :  dehydrated
    -> [> `Assoc of (string * [> `List of Yojson.Safe.t list ]) list ]

  val to_script_tag : dehydrated -> string
end

val script_tag_with_nonce : nonce:string -> ?var_name:string -> Yojson.Safe.t -> string
val generate_nonce : unit -> string
val current_nonce : string option ref
val set_nonce : string -> unit
val get_nonce : unit -> string option
val clear_nonce : unit -> unit
val is_safe_for_embed : Yojson.Safe.t -> bool
