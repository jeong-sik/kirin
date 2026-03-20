type render_mode = SSR | Static | SPA
type t = {
  path : string;
  name : string option;
  render_mode : render_mode;
  layout : string option;
  loaders : string list;
  actions : string list;
  head : head_config option;
  on_request : string option;
  on_get : string option;
  on_post : string option;
  children : t list;
}
and head_config = {
  title : string option;
  meta : (string * string) list;
  links : link list;
}
and link = { rel : string; href : string; }
val empty : string -> t
val ssr : string -> t
val static : string -> t
val spa : string -> t
val with_name : string -> t -> t
val with_layout : string -> t -> t
val with_loader : string -> t -> t
val with_action : string -> t -> t
val with_head :
  ?title:string option ->
  ?meta:(string * string) list -> ?links:link list -> t -> t
val with_on_request : string -> t -> t
val with_on_get : string -> t -> t
val with_on_post : string -> t -> t
val with_children : t list -> t -> t
type param_type = String | Int | Slug | Optional of param_type
type param = { param_name : string; param_type : param_type; }
val extract_params : string -> param list
val matches : t -> string -> bool
val render_mode_to_string : render_mode -> string
val to_json :
  t ->
  ([> `Assoc of
        (string *
         [> `Assoc of
              (string *
               [> `Assoc of (string * string) list
                | `List of
                    [> `Assoc of (string * [> `String of string ]) list ]
                    list
                | `Null
                | `String of string ])
              list
          | `List of 'a list
          | `Null
          | `String of string ])
        list
    | `String of string ]
   as 'a)
val index : unit -> t
val layout : string -> t list -> t
val catch_all : unit -> t
val optional : string -> t
