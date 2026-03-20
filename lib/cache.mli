(** Kirin In-Memory Cache

    LRU cache with TTL support. Thread-safe via Eio.Mutex.

    @since 1.0.0
    @status stable

    {b Features:}
    - LRU eviction policy
    - Optional TTL (time-to-live) per entry
    - Configurable maximum size
    - Statistics tracking *)

(** {1 Types} *)

(** Cache entry with metadata. Abstract to hide internal representation. *)
type 'a entry

(** Cache statistics. *)
type stats = {
  hits : int;
  misses : int;
  evictions : int;
  expirations : int;
  current_size : int;
  max_size : int;
}

(** Cache configuration. *)
type config = {
  max_size : int;
  default_ttl : float option;
  cleanup_interval : float;
}

(** LRU cache keyed by ['k] with values of type ['v]. Abstract to hide internal representation. *)
type ('k, 'v) t

(** {1 Configuration} *)

(** Default configuration (max_size=1000, no TTL, cleanup every 60s). *)
val default_config : config

(** {1 Cache Creation} *)

(** [create ?max_size ?default_ttl ?cleanup_interval ()] creates a new cache.
    @param max_size Maximum number of entries (default: 1000)
    @param default_ttl Default TTL in seconds (default: no expiration)
    @param cleanup_interval Cleanup interval in seconds (default: 60.0) *)
val create : ?max_size:int -> ?default_ttl:float -> ?cleanup_interval:float -> unit -> ('k, 'v) t

(** {1 Cache Operations} *)

(** [get cache key] returns [Some value] if found and not expired, [None] otherwise.
    Updates access time for LRU tracking. *)
val get : ('k, 'v) t -> 'k -> 'v option

(** [set ?ttl cache key value] sets a value in the cache.
    @param ttl Optional TTL override in seconds *)
val set : ?ttl:float -> ('k, 'v) t -> 'k -> 'v -> unit

(** [remove cache key] removes a key. Returns [true] if the key existed. *)
val remove : ('k, 'v) t -> 'k -> bool

(** [mem cache key] returns [true] if the key exists and is not expired. *)
val mem : ('k, 'v) t -> 'k -> bool

(** [get_or_set ?ttl cache key compute] returns cached value or computes and caches it. *)
val get_or_set : ?ttl:float -> ('k, 'v) t -> 'k -> (unit -> 'v) -> 'v

(** {1 Bulk Operations} *)

(** [clear cache] removes all entries. *)
val clear : ('k, 'v) t -> unit

(** [cleanup cache] removes expired entries. Returns the number removed. *)
val cleanup : ('k, 'v) t -> int

(** [keys cache] returns all current keys. *)
val keys : ('k, 'v) t -> 'k list

(** {1 Statistics} *)

(** [stats cache] returns current cache statistics. *)
val stats : ('k, 'v) t -> stats

(** [hit_rate cache] returns the hit rate as a float (0.0 to 1.0). *)
val hit_rate : ('k, 'v) t -> float

(** [size cache] returns the current number of entries. *)
val size : ('k, 'v) t -> int

(** {1 Iteration} *)

(** [iter f cache] calls [f key value] for each non-expired entry. *)
val iter : ('k -> 'v -> unit) -> ('k, 'v) t -> unit

(** [fold f cache init] folds over non-expired entries. *)
val fold : ('k -> 'v -> 'acc -> 'acc) -> ('k, 'v) t -> 'acc -> 'acc

(** {1 Entry Metadata} *)

(** [entry_info cache key] returns [(age, ttl_remaining, access_count)] or [None]. *)
val entry_info : ('k, 'v) t -> 'k -> (float * float option * int) option

(** {1 Specialized Caches} *)

(** String-keyed cache. *)
module StringCache : sig
  type nonrec 'v t = (string, 'v) t
  val create : ?max_size:int -> ?default_ttl:float -> ?cleanup_interval:float -> unit -> 'v t
  val get : 'v t -> string -> 'v option
  val set : ?ttl:float -> 'v t -> string -> 'v -> unit
  val remove : 'v t -> string -> bool
  val mem : 'v t -> string -> bool
  val clear : 'v t -> unit
  val stats : 'v t -> stats
  val size : 'v t -> int
end

(** Integer-keyed cache. *)
module IntCache : sig
  type nonrec 'v t = (int, 'v) t
  val create : ?max_size:int -> ?default_ttl:float -> ?cleanup_interval:float -> unit -> 'v t
  val get : 'v t -> int -> 'v option
  val set : ?ttl:float -> 'v t -> int -> 'v -> unit
  val remove : 'v t -> int -> bool
  val mem : 'v t -> int -> bool
  val clear : 'v t -> unit
  val stats : 'v t -> stats
  val size : 'v t -> int
end

(** {1 Cache Middleware Helper} *)

(** [memoize ?ttl cache ~key_fn f] creates a caching wrapper.
    Computes [f arg] on cache miss and stores the result. *)
val memoize : ?ttl:float -> ('k, 'v) t -> key_fn:('a -> 'k) -> ('a -> 'v) -> 'a -> 'v

(** [make_key parts] joins string parts with [":"] for cache key construction. *)
val make_key : string list -> string
