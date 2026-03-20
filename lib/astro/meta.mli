type meta_tag =
    Name of { name : string; content : string; }
  | Property of { property : string; content : string; }
  | HttpEquiv of { http_equiv : string; content : string; }
  | Charset of string
type link_tag = {
  rel : string;
  href : string;
  type_ : string option;
  sizes : string option;
  media : string option;
  crossorigin : string option;
}
type script_tag = {
  src : string option;
  inline : string option;
  type_ : string option;
  async : bool;
  defer : bool;
  module_ : bool;
}
type t = {
  title : string option;
  meta : meta_tag list;
  links : link_tag list;
  scripts : script_tag list;
  base : string option;
  canonical : string option;
}
val empty : t
val with_title : string -> t -> t
val with_meta : meta_tag -> t -> t
val with_link : link_tag -> t -> t
val with_script : script_tag -> t -> t
val with_base : string -> t -> t
val with_canonical : string -> t -> t
val description : string -> meta_tag
val keywords : string -> meta_tag
val author : string -> meta_tag
val viewport : string -> meta_tag
val robots : string -> meta_tag
val charset : string -> meta_tag
val og_title : string -> meta_tag
val og_description : string -> meta_tag
val og_image : string -> meta_tag
val og_url : string -> meta_tag
val og_type : string -> meta_tag
val og_site_name : string -> meta_tag
val twitter_card : string -> meta_tag
val twitter_title : string -> meta_tag
val twitter_description : string -> meta_tag
val twitter_image : string -> meta_tag
val twitter_site : string -> meta_tag
val stylesheet : string -> link_tag
val preconnect : string -> link_tag
val preload : href:string -> as_:string -> link_tag
val favicon : string -> link_tag
val apple_touch_icon : href:string -> sizes:string -> link_tag
val seo :
  title:string ->
  ?description:string ->
  ?og_image_url:string ->
  ?twitter_card_type:string -> ?canonical_url:string -> unit -> t
val render_meta : meta_tag -> string
val render_link : link_tag -> string
val render_script : script_tag -> string
val render : t -> string
