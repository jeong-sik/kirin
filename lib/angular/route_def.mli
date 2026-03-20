type render_mode = Client | Server | Prerender
type param_type = String | Int | Uuid | Slug | Optional of param_type
type param = { name : string; param_type : param_type; }
type guard = {
  guard_name : string;
  can_activate : bool;
  can_deactivate : bool;
  can_match : bool;
}
type resolver = { resolver_name : string; key : string; }
type t = {
  path : string;
  name : string option;
  component : string option;
  redirect_to : string option;
  path_match : string option;
  render_mode : render_mode;
  params : param list;
  guards : guard list;
  resolvers : resolver list;
  children : t list;
  data : (string * Yojson.Safe.t) list;
  title : string option;
  lazy_load : bool;
}
val empty : string -> t
val route : string -> string -> t
val redirect : from:string -> to_:string -> ?path_match:string -> unit -> t
val lazy_route : string -> string -> t
val with_render_mode : render_mode -> t -> t
val with_param : string -> param_type -> t -> t
val with_guard :
  string ->
  ?can_activate:bool -> ?can_deactivate:bool -> ?can_match:bool -> t -> t
val with_resolver : key:string -> string -> t -> t
val with_children : t list -> t -> t
val with_data : string -> Yojson.Safe.t -> t -> t
val with_title : string -> t -> t
val with_name : string -> t -> t
val ssr : string -> string -> t
val ssg : string -> string -> t
val csr : string -> string -> t
val render_mode_to_string : render_mode -> string
val param_type_to_string : param_type -> string
val to_json :
  t ->
  ([> `Assoc of
        (string *
         [> `Assoc of (string * Yojson.Safe.t) list
          | `List of 'a list
          | `String of string ])
        list ]
   as 'a)
