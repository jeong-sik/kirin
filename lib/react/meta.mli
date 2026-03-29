val escape_html : string -> string
val title : string -> string
val description : string -> string
val meta : name:string -> content:string -> string
val og : property:string -> content:string -> string
val twitter : name:string -> content:string -> string
val canonical : string -> string
val charset : ?encoding:string -> unit -> string
val viewport : ?content:string -> unit -> string
val robots : ?index:bool -> ?follow:bool -> unit -> string
val favicon : ?mime_type:string -> string -> string
val apple_touch_icon : ?sizes:string -> string -> string
val theme_color : string -> string
val alternate : hreflang:string -> href:string -> string
val preconnect : ?crossorigin:bool -> string -> string
val dns_prefetch : string -> string
val json_ld : Yojson.Safe.t -> string

type config =
  { title : string option
  ; description : string option
  ; keywords : string list
  ; author : string option
  ; canonical : string option
  ; og_title : string option
  ; og_description : string option
  ; og_image : string option
  ; og_url : string option
  ; og_type : string option
  ; og_site_name : string option
  ; twitter_card : string option
  ; twitter_site : string option
  ; twitter_creator : string option
  ; twitter_title : string option
  ; twitter_description : string option
  ; twitter_image : string option
  ; robots_index : bool
  ; robots_follow : bool
  ; theme_color : string option
  ; extra : string list
  }

val empty_config : config
val render_config : config -> string
val render_pairs : (string * string) list -> string
val common_head : unit -> string

val make
  :  ?title:string
  -> ?description:string
  -> ?keywords:string list
  -> ?author:string
  -> ?canonical:string
  -> ?og_title:string
  -> ?og_description:string
  -> ?og_image:string
  -> ?og_url:string
  -> ?og_type:string
  -> ?og_site_name:string
  -> ?twitter_card:string
  -> ?twitter_site:string
  -> ?twitter_creator:string
  -> ?twitter_title:string
  -> ?twitter_description:string
  -> ?twitter_image:string
  -> ?robots_index:bool
  -> ?robots_follow:bool
  -> ?theme_color:string
  -> ?extra:string list
  -> unit
  -> config
