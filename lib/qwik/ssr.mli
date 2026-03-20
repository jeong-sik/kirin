type config = {
  server_entry : string;
  manifest_path : string;
  workers : int;
  timeout_s : float;
  memory_limit_mb : int;
  restart_after : int;
  enable_cache : bool;
  cache_ttl_s : float;
  cache_max_size : int;
}
val default_config : config
type cache_entry = {
  html : string;
  container_state : Container.t option;
  created_at : float;
  ttl : float;
}
type cache = {
  mutable entries : (string * cache_entry) list;
  max_size : int;
  default_ttl : float;
}
val create_cache : max_size:int -> default_ttl:float -> cache
val cache_get : cache -> string -> cache_entry option
val cache_put :
  cache ->
  string ->
  string -> ?container_state:Container.t -> unit -> unit
type stats = {
  mutable total_renders : int;
  mutable cache_hits : int;
  mutable cache_misses : int;
  mutable errors : int;
  mutable timeouts : int;
  mutable avg_render_ms : float;
}
val create_stats : unit -> stats
type t = {
  config : config;
  cache : cache option;
  stats : stats;
  mutable running : bool;
  mutable next_id : int;
}
val create : config -> t
val is_running : t -> bool
val next_id : t -> int
val get_stats : t -> stats
val cache_hit_rate : stats -> float
val cache_key : url:'a -> headers:'b -> 'a
val render :
  t ->
  url:string ->
  ?headers:'a list ->
  ?container_attrs:'b list ->
  ?server_data:'c -> unit -> (string, string) result
val render_with_fallback : t -> url:string -> fallback:string -> string
val prerender :
  t -> routes:Route_def.t list -> output_dir:string -> unit
val shutdown : t -> unit
val stats_to_json :
  stats -> [> `Assoc of (string * [> `Float of float | `Int of int ]) list ]
val config_to_json :
  config ->
  [> `Assoc of
       (string *
        [> `Bool of bool | `Float of float | `Int of int | `String of string
        ])
       list ]
