type config =
  { workers : int
  ; timeout_s : float
  ; enable_cache : bool
  ; cache_ttl_s : float
  ; max_cache_entries : int
  ; memory_limit_mb : int
  ; restart_after : int
  ; dist_path : string
  ; integrations : Integration.t list
  }

val default_config : config

type cache_entry =
  { html : string
  ; head : string
  ; created_at : float
  ; ttl : float
  }

type cache =
  { mutable entries : (string * cache_entry) list
  ; max_entries : int
  }

val create_cache : int -> cache
val cache_get : cache -> string -> cache_entry option
val cache_put : cache -> string -> string -> string -> float -> unit
val cache_clear : cache -> unit

type stats =
  { mutable total_renders : int
  ; mutable cache_hits : int
  ; mutable cache_misses : int
  ; mutable errors : int
  ; mutable timeouts : int
  ; mutable avg_render_ms : float
  }

val create_stats : unit -> stats
val cache_hit_rate : stats -> float

type t =
  { config : config
  ; cache : cache option
  ; stats : stats
  ; mutable running : bool
  }

val create : config -> t
val get_stats : t -> stats
val shutdown : t -> unit
val cache_key : url:'a -> 'a

val render
  :  t
  -> url:string
  -> ?props:[> `Assoc of 'a list ]
  -> ?islands:'b list
  -> unit
  -> (string, string) result

val render_with_fallback : t -> url:string -> fallback:string -> string
val render_island : 'a -> Island.t -> ssr_html:string -> string
val render_islands : 'a -> Island.t list -> (string * string) list

val generate_static
  :  t
  -> url:string
  -> output_path:string
  -> unit
  -> (string, string) result

val generate_all_static : t -> Route_def.t list -> string -> (string, string) result list
val hot_reload_script : int -> string
val inject_dev_scripts : string -> int -> string
