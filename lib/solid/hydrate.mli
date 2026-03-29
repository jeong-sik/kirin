type options =
  { title : string
  ; lang : string
  ; meta_tags : string
  ; scripts : string list
  ; styles : string list
  ; initial_data : Data.initial_data
  ; body_attrs : string
  ; root_id : string
  ; nonce : string option
  }

val default_options : options
val script_tag : ?nonce:string -> string -> string
val style_tag : string -> string
val render : ?ssr_html:string -> options -> string
val spa : title:string -> entry_script:string -> ?styles:string list -> unit -> string

val with_ssr
  :  title:string
  -> entry_script:string
  -> ssr_html:string
  -> ?styles:string list
  -> ?initial_data:Data.initial_data
  -> unit
  -> string

val with_vite
  :  title:string
  -> manifest:
       (string
       * [> `Assoc of
              (string * [> `List of [> `String of string ] list | `String of string ])
                list
         ])
         list
  -> entry:string
  -> ?dev_mode:bool
  -> unit
  -> string

val island : component_name:string -> props_json:Yojson.Safe.t -> string -> string
val lazy_island : component_path:string -> props_json:Yojson.Safe.t -> string
val streaming_placeholder : id:string -> fallback:string -> string
val streaming_replacement : id:string -> string -> string
