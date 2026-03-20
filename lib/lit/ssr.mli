type ssr_config = {
  bundle : string;
  pool_size : int;
  timeout : float;
  memory_limit_mb : int;
  restart_after : int;
  use_declarative_shadow_dom : bool;
  include_polyfill : bool;
}
val default_config : ssr_config
type worker_state = Idle | Busy | Dead
type worker = {
  mutable state : worker_state;
  request_count : int Atomic.t;
  mutable last_error : string option;
}
type t = {
  config : ssr_config;
  workers : worker array;
  total_renders : int Atomic.t;
  errors : int Atomic.t;
  timeouts : int Atomic.t;
  cache_hits : int Atomic.t;
  cache_misses : int Atomic.t;
}
type ssr_stats = {
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
  components : string list;
  has_shadow_dom : bool;
}
val render_internal :
  t ->
  tag_name:string ->
  ?props:[> `Assoc of (string * Yojson.Safe.t) list | `Null ] ->
  ?slots:(string option * string) list -> unit -> (render_result, 'a) result
val render :
  t ->
  tag_name:string ->
  ?props:[> `Assoc of (string * Yojson.Safe.t) list | `Null ] ->
  ?slots:(string option * string) list -> unit -> (string, 'a) result
val render_with_fallback : t -> tag_name:string -> fallback:string -> string
val render_page :
  t ->
  components:(string * [> `Assoc of (string * Yojson.Safe.t) list | `Null ] *
              (string option * string) list)
             list ->
  string
type stream_chunk = Shell of string | Component of string | Complete
val render_stream :
  t -> tag_name:string -> on_chunk:(stream_chunk -> unit) -> unit
val handler : t -> tag_name:string -> 'a -> Kirin.Response.t
val handler_with_props :
  get_props:('a -> [> `Assoc of (string * Yojson.Safe.t) list | `Null ]) ->
  t -> tag_name:string -> 'a -> Kirin.Response.t
val stats : t -> ssr_stats
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
  ssr_stats ->
  [> `Assoc of (string * [> `Float of float | `Int of int ]) list ]
val result_to_json :
  render_result ->
  [> `Assoc of
       (string *
        [> `Bool of bool
         | `List of [> `String of string ] list
         | `String of string ])
       list ]
