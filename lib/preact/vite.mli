type config = {
  port : int;
  host : string;
  manifest_path : string;
  dist_dir : string;
  prefix : string;
  hmr : bool;
  compat_mode : bool;
}
val default_config : config
val is_dev : unit -> bool
val dev_url : config -> string
val dev_script_tag : config -> string -> string
val hmr_script : config -> string
val generate_vite_config : config -> string
val static_routes : ?config:config -> unit -> Kirin.Router.route list
val with_manifest :
  config ->
  (Manifest.t -> ('a, string) result) -> ('a, string) result
val production_html :
  config -> entry:string -> title:string -> (string, string) result
val development_html :
  config -> entry:string -> title:string -> (string, 'a) result
val html : config -> entry:string -> title:string -> (string, string) result
val refresh_preamble : config -> string
val config_to_json :
  config ->
  [> `Assoc of
       (string * [> `Bool of bool | `Int of int | `String of string ]) list ]
