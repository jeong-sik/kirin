(** @since 1.0.0
    @status experimental
    This module's API may change in future versions. *)

type element = Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t
type event = Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t
type location = { pathname : string; search : string; hash : string; }
val log : string -> unit
val error : string -> unit
val warn : string -> unit
val get_by_id : string -> element
val get_by_id_opt : string -> element option
val query : string -> element
val query_opt : string -> element option
val body : unit -> element
val get_html : element -> string
val set_html : element -> string -> unit
val get_text : element -> string
val set_text : element -> string -> unit
val get_attr : element -> string -> string option
val set_attr : element -> string -> string -> unit
val add_class : element -> string -> unit
val remove_class : element -> string -> unit
val toggle_class : element -> string -> bool
val has_class : element -> string -> bool
val create : string -> element
val append : element -> element -> unit
val remove : element -> unit
val on :
  element ->
  string -> ('a #Js_of_ocaml.Dom.event Js_of_ocaml__.Js.t -> unit) -> unit
val on_click :
  element -> ('a #Js_of_ocaml.Dom.event Js_of_ocaml__.Js.t -> unit) -> unit
val on_input :
  element -> ('a #Js_of_ocaml.Dom.event Js_of_ocaml__.Js.t -> unit) -> unit
val on_submit :
  element -> ('a #Js_of_ocaml.Dom.event Js_of_ocaml__.Js.t -> unit) -> unit
val get_value : element -> string
val set_value : element -> string -> unit
val location : unit -> location
val path : unit -> string
val navigate : string -> unit
val replace : string -> unit
val back : unit -> unit
val forward : unit -> unit
val navigate_callbacks : (location -> unit) list ref
val handle_popstate : 'a -> bool Js_of_ocaml.Js.t
val init_popstate : unit -> unit
val on_navigate : (location -> unit) -> unit
type http_method = GET | POST | PUT | DELETE | PATCH
val method_string : http_method -> string
type response = {
  ok : bool;
  status : int;
  _resp : Js_of_ocaml.Js.Unsafe.any;
}
val fetch :
  ?meth:http_method ->
  ?headers:(string * string) list ->
  ?body:string -> string -> (response -> 'a) -> unit
val response_text : response -> (string -> 'a) -> unit
val response_json : response -> (Yojson.Safe.t option -> 'a) -> unit
val get : string -> (response -> 'a) -> unit
val post_json : string -> Yojson.Safe.t -> (response -> 'a) -> unit
val set_timeout : int -> (unit -> unit) -> unit -> unit
val set_interval : int -> (unit -> unit) -> unit -> unit
val request_animation_frame : (unit -> unit) -> unit
val get_storage :
  unit -> Js_of_ocaml.Dom_html.storage Js_of_ocaml__.Js.t option
val storage_get : string -> string option
val storage_set : string -> string -> unit
val storage_remove : string -> unit
val data_prefix : string
val get_data : element -> string -> string option
val set_data : element -> string -> string -> unit
val components : (string * (element -> string -> unit)) list ref
val register_component : string -> (element -> string -> unit) -> unit
val find_components : element -> element list
val hydrate_element : element -> unit
val hydrate : element -> unit
module Router :
  sig
    type route_pattern = {
      pattern : string;
      segments : string list;
      handler : (string * string) list -> unit;
    }
    type t = {
      routes : route_pattern list;
      not_found : (string * string) list -> unit;
    }
    val route : string -> ((string * string) list -> unit) -> route_pattern
    val create :
      not_found:((string * string) list -> unit) -> route_pattern list -> t
    val match_path :
      string list -> string list -> (string * string) list option
    val find_route :
      t ->
      string ->
      (((string * string) list -> unit) * (string * string) list) option
    val navigate : t -> string -> unit
    val handle_current : t -> unit
    val start : t -> unit
  end
val start : ?root_id:string -> ?routing:bool -> unit -> element
