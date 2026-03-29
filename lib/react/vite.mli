val is_dev_server_running : ?host:string -> port:int -> unit -> bool
val is_dev : unit -> bool

type dev_config =
  { host : string
  ; port : int
  ; https : bool
  }

val default_dev_config : dev_config
val dev_server_url : ?config:dev_config -> string -> string

val dev_proxy_middleware
  :  ?_config:dev_config
  -> unit
  -> Kirin.handler
  -> Kirin.Request.t
  -> Kirin.Response.t

type prod_config =
  { manifest_path : string
  ; dist_dir : string
  ; base_url : string
  ; index_fallback : bool
  }

val default_prod_config : prod_config
val load_manifest : prod_config -> Manifest.t
val mime_type_of : string -> string
val serve_file : string -> Kirin.Response.t

val static_handler
  :  config:prod_config
  -> manifest:'a
  -> Kirin.Request.t
  -> Kirin.Response.t

val static_routes : ?config:prod_config -> unit -> Kirin.Router.route list

val routes
  :  ?dev_config:dev_config
  -> ?prod_config:prod_config
  -> unit
  -> Kirin.Router.route list

val dev_script_tag : ?config:dev_config -> entry:string -> unit -> string
val hmr_script : ?config:dev_config -> unit -> string
val react_refresh_preamble : ?config:dev_config -> unit -> string
val dev_head_scripts : ?config:dev_config -> unit -> string
val script_tag : manifest:Manifest.t -> entry:string -> unit -> string
val css_tags : manifest:Manifest.t -> entry:string -> unit -> string

type build_info =
  { mode : [ `Dev | `Prod ]
  ; manifest_loaded : bool
  ; entry_count : int
  ; vite_version : string option
  }

val get_build_info : ?prod_config:prod_config -> unit -> build_info
val build_info_to_string : build_info -> string
