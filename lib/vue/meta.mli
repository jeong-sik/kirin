type link =
  { rel : string
  ; href : string
  ; hreflang : string option
  ; type_ : string option
  ; sizes : string option
  ; media : string option
  ; crossorigin : string option
  }

type script =
  { src : string option
  ; content : string option
  ; type_ : string option
  ; async : bool
  ; defer : bool
  ; crossorigin : string option
  }

type meta =
  { name : string option
  ; property : string option
  ; content : string
  ; http_equiv : string option
  ; charset : string option
  }

type head =
  { title : string option
  ; title_template : string option
  ; base : string option
  ; links : link list
  ; metas : meta list
  ; scripts : script list
  ; styles : string list
  ; html_attrs : (string * string) list
  ; body_attrs : (string * string) list
  ; noscript : string list
  }

val empty : head
val with_title : string -> head -> head
val with_title_template : string -> head -> head
val with_meta : name:string -> content:string -> head -> head
val with_property : property:string -> content:string -> head -> head

val with_link
  :  rel:string
  -> href:string
  -> ?type_:string
  -> ?sizes:string
  -> head
  -> head

val with_script : src:string -> ?async:bool -> ?defer:bool -> head -> head
val with_inline_script : string -> head -> head
val with_style : string -> head -> head
val with_html_attr : name:string -> value:string -> head -> head
val with_body_attr : name:string -> value:string -> head -> head

type seo_meta =
  { title : string option
  ; og_title : string option
  ; description : string option
  ; og_description : string option
  ; og_image : string option
  ; og_url : string option
  ; og_type : string option
  ; og_site_name : string option
  ; og_locale : string option
  ; twitter_card : string option
  ; twitter_site : string option
  ; twitter_creator : string option
  ; twitter_title : string option
  ; twitter_description : string option
  ; twitter_image : string option
  ; robots : string option
  ; canonical : string option
  }

val empty_seo : seo_meta

val seo
  :  title:string
  -> ?description:string
  -> ?image:string
  -> ?url:string
  -> unit
  -> seo_meta

val render_meta : meta -> string
val render_link : link -> string
val render_script : script -> string
val render : head -> string
val seo_to_head : seo_meta -> head
val meta_to_json : meta -> [> `Assoc of (string * [> `String of string ]) list ]

val head_to_json
  :  head
  -> [> `Assoc of
          (string
          * [> `List of [> `Assoc of (string * [> `String of string ]) list ] list
            | `Null
            | `String of string
            ])
            list
     ]

val seo_to_json : seo_meta -> [> `Assoc of (string * [> `String of string ]) list ]
