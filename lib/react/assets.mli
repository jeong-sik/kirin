val url : ?base_url:string -> manifest:Manifest.t -> string -> string

val script_tag
  :  ?base_url:string
  -> ?module_:bool
  -> manifest:Manifest.t
  -> string
  -> string

val link_tag : ?base_url:string -> manifest:Manifest.t -> string -> string
val css_tags : ?base_url:string -> manifest:Manifest.t -> string -> string
val modulepreload_tag : ?base_url:string -> string -> string

val preload_tag
  :  ?base_url:string
  -> ?crossorigin:bool
  -> as_type:string
  -> string
  -> string

val detect_asset_type : string -> string
val preload_all : ?base_url:string -> manifest:Manifest.t -> string -> string
val head_tags : ?base_url:string -> manifest:Manifest.t -> entry:string -> unit -> string
val body_tags : ?base_url:string -> manifest:Manifest.t -> entry:string -> unit -> string

type config =
  { base_url : string
  ; manifest : Manifest.t
  }

val make_config : ?base_url:string -> Manifest.t -> config
val resolve_url : config -> string -> string
val script : config -> string -> string
val stylesheet : config -> string -> string
val all_styles : config -> string -> string
val all_preloads : config -> string -> string
val inline_css : ?dir:string -> string -> string
val integrity_hash : 'a -> 'b option
val script_tag_sri : ?base_url:string -> manifest:Manifest.t -> string -> string
