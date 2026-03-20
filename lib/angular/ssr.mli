type config = {
  bundle : string;
  workers : int;
  timeout_s : float;
  memory_limit_mb : int;
  restart_after : int;
  enable_cache : bool;
  cache_size : int;
  prerender_routes : string list;
}
val default_config : config
type cache_entry = { html : string; timestamp : float; ttl : float; }
type cache = {
  mutable entries : (string * cache_entry) list;
  max_size : int;
}
val create_cache : int -> cache
val cache_get : cache -> string -> string option
val cache_set : cache -> string -> string -> ?ttl:float -> unit -> unit
type stats = {
  mutable total_renders : int;
  mutable cache_hits : int;
  mutable cache_misses : int;
  mutable errors : int;
  mutable timeouts : int;
  mutable total_time_ms : float;
}
val create_stats : unit -> stats
val cache_hit_rate : stats -> float
val avg_render_time : stats -> float
type t = {
  config : config;
  mutable running : bool;
  cache : cache;
  stats : stats;
}
val create : config -> t
val is_running : t -> bool
val shutdown : t -> unit
val get_stats : t -> stats
val render :
  t -> url:string -> ?headers:'a list -> unit -> (string, string) result
val render_with_fallback : t -> url:string -> fallback:string -> string
val prerender :
  t ->
  routes:string list -> output_dir:string -> (string * string * string) list
val stats_to_json :
  stats -> [> `Assoc of (string * [> `Float of float | `Int of int ]) list ]
val config_to_json :
  config ->
  [> `Assoc of
       (string *
        [> `Bool of bool | `Float of float | `Int of int | `String of string
        ])
       list ]
