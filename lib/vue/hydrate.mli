type options = {
  title : string;
  lang : string;
  dir : string;
  head_tags : string;
  body_attrs : string;
  scripts : string list;
  styles : string list;
  preload_hints : string;
  nonce : string option;
}
val default_options : options
val spa : title:string -> ?entry_script:string -> unit -> string
val with_ssr :
  title:string ->
  entry_script:string -> ssr_html:string -> ?payload:string -> unit -> string
val render :
  options:options ->
  ssr_html:string -> payload:string -> entry_script:string -> string
val nuxt3_shell :
  title:string ->
  app_html:string -> payload:string -> entry_script:string -> string
val dev_shell : title:string -> ?vite_port:int -> unit -> string
val error_page : status:int -> message:string -> string
val streaming_placeholder : id:string -> string
val streaming_replacement : id:string -> html:string -> string
type priority = Eager | Lazy | Visible | Interact
val priority_to_string : priority -> string
val island_wrapper :
  id:string -> component:string -> priority:priority -> html:string -> string
val teleport_target : id:string -> string
val options_to_json :
  options ->
  [> `Assoc of
       (string *
        [> `List of [> `String of string ] list | `String of string ])
       list ]
