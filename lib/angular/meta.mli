type meta_tag =
  | Name of
      { name : string
      ; content : string
      }
  | Property of
      { property : string
      ; content : string
      }
  | HttpEquiv of
      { http_equiv : string
      ; content : string
      }
  | Charset of string

type link_tag =
  { rel : string
  ; href : string
  ; hreflang : string option
  ; type_ : string option
  ; sizes : string option
  ; media : string option
  }

type script_tag =
  { src : string option
  ; content : string option
  ; type_ : string option
  ; defer : bool
  ; async_ : bool
  ; module_ : bool
  }

type head =
  { title : string option
  ; base_href : string option
  ; metas : meta_tag list
  ; links : link_tag list
  ; scripts : script_tag list
  }

val empty : head
val with_title : string -> head -> head
val with_base : string -> head -> head
val with_meta : name:string -> content:string -> head -> head
val with_property : property:string -> content:string -> head -> head
val with_charset : string -> head -> head
val with_http_equiv : http_equiv:string -> content:string -> head -> head

val with_link
  :  rel:string
  -> href:string
  -> ?hreflang:string
  -> ?type_:string
  -> ?sizes:string
  -> ?media:string
  -> unit
  -> head
  -> head

val with_script
  :  src:string
  -> ?type_:string
  -> ?defer:bool
  -> ?async_:bool
  -> ?module_:bool
  -> unit
  -> head
  -> head

val with_inline_script : string -> head -> head

val seo
  :  title:string
  -> ?description:string
  -> ?canonical:string
  -> ?og_title:string
  -> ?og_description:string
  -> ?og_image:string
  -> ?og_url:string
  -> unit
  -> head

val render_meta : meta_tag -> string
val render_link : link_tag -> string
val render_script : script_tag -> string
val render : head -> string
val meta_to_json : meta_tag -> [> `Assoc of (string * [> `String of string ]) list ]

val to_json
  :  head
  -> [> `Assoc of
          (string
          * [> `List of [> `Assoc of (string * [> `String of string ]) list ] list
            | `Null
            | `String of string
            ])
            list
     ]
