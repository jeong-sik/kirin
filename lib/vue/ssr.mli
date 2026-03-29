type cache_strategy =
  | NoCache
  | Stale_while_revalidate of int
  | Cache_control of string

type config =
  { bundle : string
  ; workers : int
  ; timeout_s : float
  ; memory_limit_mb : int
  ; restart_after : int
  ; cache_strategy : cache_strategy
  ; prerender_routes : string list
  ; fallback_html : string option
  }

val default_config : config

type render_result =
  { html : string
  ; status : int
  ; headers : (string * string) list
  ; redirect : string option
  ; render_time_ms : float
  ; cache_hit : bool
  }

type stats =
  { total_renders : int
  ; cache_hits : int
  ; cache_misses : int
  ; errors : int
  ; timeouts : int
  ; avg_render_time_ms : float
  ; active_workers : int
  ; total_memory_mb : int
  }

val empty_stats : stats

type t =
  { config : config
  ; mutable stats : stats
  ; stats_mutex : Eio.Mutex.t
  ; mutable cache : (string, render_result * float) Hashtbl.t
  ; mutable running : bool
  }

val create : config -> t
val cache_ttl : config -> int
val check_cache : t -> string -> render_result option
val store_cache : t -> string -> render_result -> unit
val clear_cache : t -> unit

val render
  :  t
  -> url:string
  -> ?headers:(string * string) list
  -> ?payload:'a option
  -> unit
  -> (render_result, string) result

val render_with_fallback : t -> url:string -> fallback:string -> string
val prerender : t -> string list -> (string * string) list
val stats : t -> stats
val get_config : t -> config
val is_running : t -> bool
val shutdown : t -> unit
val cache_strategy_to_string : cache_strategy -> string

val stats_to_json
  :  stats
  -> [> `Assoc of (string * [> `Float of float | `Int of int ]) list ]

val config_to_json
  :  config
  -> [> `Assoc of
          (string
          * [> `Float of float
            | `Int of int
            | `List of [> `String of string ] list
            | `String of string
            ])
            list
     ]

type route_options =
  { ssr : bool
  ; cache : cache_strategy
  ; prerender : bool
  ; headers : (string * string) list
  }

val default_route_options : route_options

val with_route_options
  :  route_options
  -> t
  -> url:string
  -> (render_result, string) result
