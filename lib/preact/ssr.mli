type ssr_config = {
  bundle : string;
  pool_size : int;
  timeout : float;
  memory_limit_mb : int;
  restart_after : int;
  signals_ssr : bool;
}
val default_config : ssr_config
type worker_state = Idle | Busy | Dead
type worker = {
  mutable state : worker_state;
  mutable request_count : int;
  mutable last_error : string option;
}
type t = {
  config : ssr_config;
  workers : worker array;
  mutable total_renders : int;
  mutable errors : int;
  mutable timeouts : int;
  mutable cache_hits : int;
  mutable cache_misses : int;
}
type stats = {
  stat_total_renders : int;
  stat_errors : int;
  stat_timeouts : int;
  stat_avg_time_ms : float;
  stat_cache_hit_rate : float;
  stat_active_workers : int;
}
val create : ssr_config -> t
type render_result = {
  html : string;
  head : string option;
  signals_state : Yojson.Safe.t option;
}
val render_internal :
  'a ->
  url:string ->
  ?props:Yojson.Safe.t ->
  ?signals:Yojson.Safe.t -> unit -> (render_result, 'b) result
val render :
  t ->
  url:string ->
  ?props:Yojson.Safe.t ->
  ?signals:Yojson.Safe.t -> unit -> (string, 'a) result
val render_with_fallback : t -> url:string -> fallback:string -> string
val render_with_signals :
  t -> url:string -> signals:Yojson.Safe.t -> unit -> (string, 'a) result
val dehydrate_signals : Yojson.Safe.t -> string
val handler : t -> 'a -> Kirin.Response.t
val handler_with_fallback : fallback:string -> t -> 'a -> Kirin.Response.t
val stats : t -> stats
val shutdown : t -> unit
val is_healthy : t -> bool
val config_to_json :
  ssr_config ->
  [> `Assoc of
       (string *
        [> `Bool of bool | `Float of float | `Int of int | `String of string
        ])
       list ]
val stats_to_json :
  stats -> [> `Assoc of (string * [> `Float of float | `Int of int ]) list ]
