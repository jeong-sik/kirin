type t = {
  title : string option;
  metas : meta list;
  links : link list;
  scripts : script list;
  styles : string list;
}
and meta = {
  meta_name : string option;
  meta_property : string option;
  meta_content : string;
  meta_http_equiv : string option;
}
and link = {
  link_rel : string;
  link_href : string;
  link_type : string option;
  link_sizes : string option;
  link_as : string option;
  link_crossorigin : bool;
}
and script = {
  script_src : string option;
  script_content : string option;
  script_type : string option;
  script_async : bool;
  script_defer : bool;
}
val empty : t
val with_title : string -> t -> t
val with_meta : name:string -> content:string -> t -> t
val with_property : property:string -> content:string -> t -> t
val with_link :
  rel:string ->
  href:string ->
  ?type_:string ->
  ?sizes:string -> ?as_:string -> ?crossorigin:bool -> unit -> t -> t
val with_script :
  ?src:string ->
  ?content:string ->
  ?type_:string -> ?async_:bool -> ?defer:bool -> unit -> t -> t
val with_style : string -> t -> t
val seo :
  title:string ->
  ?description:string -> ?canonical:string -> ?og_image:string -> unit -> t
val with_twitter_card :
  card_type:string -> ?site:string -> ?creator:string -> t -> t
val render_meta : meta -> string
val render_link : link -> string
val render_script : script -> string
val render_style : string -> string
val render : t -> string
val to_json :
  t ->
  [> `Assoc of
       (string *
        [> `List of
             [> `Assoc of (string * [> `Null | `String of string ]) list ]
             list
         | `Null
         | `String of string ])
       list ]
