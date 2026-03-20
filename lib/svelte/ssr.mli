type config = {
  bundle : string;
  num_workers : int;
  timeout : float;
  memory_limit_mb : int;
  restart_after : int;
  cache_ttl : int;
  cache_max_size : int;
}
val default_config : config
type cache_entry = {
  response : Protocol.render_response;
  expires_at : float;
}
type cache = {
  mutable entries : (string, cache_entry) Hashtbl.t;
  max_size : int;
  ttl : int;
  mutex : Eio.Mutex.t;
}
val create_cache : max_size:int -> ttl:int -> cache
val cache_key : string -> Yojson.Safe.t -> string
val cache_get :
  cache -> string -> Protocol.render_response option
val cache_put :
  cache -> string -> Protocol.render_response -> unit
type t = {
  config : config;
  workers : Worker.t array;
  cache : cache;
  next_worker : int Atomic.t;
  total_renders : int Atomic.t;
  cache_hits : int Atomic.t;
  errors : int Atomic.t;
}
val create : config -> t
val get_worker : t -> Worker.t option
val render :
  t ->
  url:string ->
  ?props:Yojson.Safe.t ->
  ?route_id:string ->
  ?use_cache:bool ->
  unit -> (Protocol.render_response, string) result
val render_with_fallback :
  t ->
  url:string -> ?props:Yojson.Safe.t -> fallback:string -> unit -> string
val preload : t -> string list -> unit
val invalidate : t -> string -> unit
val clear_cache : t -> unit
val maintain_workers : t -> unit
val ready_workers : t -> int
type stats = {
  total_renders : int;
  cache_hits : int;
  cache_hit_rate : float;
  errors : int;
  cache_size : int;
  workers_ready : int;
  workers_total : int;
}
val get_stats : t -> stats
val stats_to_json :
  stats -> [> `Assoc of (string * [> `Float of float | `Int of int ]) list ]
val start : t -> unit
val shutdown : t -> unit
