(** Kirin In-Memory Cache

    High-performance LRU cache with TTL support.
    Thread-safe and configurable for web application caching.

    {b Features:}
    - LRU eviction policy
    - Optional TTL (time-to-live) per entry
    - Configurable maximum size
    - Statistics tracking
    - Thread-safe operations

    {b Example - Basic Cache:}
    {[
      let cache = Cache.create ~max_size:1000 () in
      Cache.set cache "key" "value";
      match Cache.get cache "key" with
      | Some v -> Printf.printf "Found: %s\n" v
      | None -> Printf.printf "Not found\n"
    ]}

    {b Example - Cache with TTL:}
    {[
      let cache = Cache.create ~max_size:100 ~default_ttl:60.0 () in
      Cache.set cache "session" session_data;
      (* Entry expires after 60 seconds *)
    ]}

    {b Example - Response Caching:}
    {[
      let response_cache = Cache.create ~max_size:500 ~default_ttl:300.0 () in

      let cached_handler cache key handler req =
        match Cache.get cache key with
        | Some resp -> resp
        | None ->
          let resp = handler req in
          Cache.set cache key resp;
          resp
    ]}
*)

(** {1 Types} *)

(** Cache entry with metadata *)
type 'a entry = {
  value : 'a;
  expires_at : float option;
  mutable last_access : float;
  mutable access_count : int;
}

(** Cache statistics *)
type stats = {
  hits : int;
  misses : int;
  evictions : int;
  expirations : int;
  current_size : int;
  max_size : int;
}

(** Cache configuration *)
type config = {
  max_size : int;
  default_ttl : float option;
  cleanup_interval : float;
}

(** LRU Cache *)
type ('k, 'v) t = {
  mutable entries : ('k, 'v entry) Hashtbl.t;
  mutable access_order : 'k list;  (* Most recent first *)
  config : config;
  mutable stats : stats;
  mutex : Mutex.t;
}

(** {1 Configuration} *)

(** Default configuration *)
let default_config = {
  max_size = 1000;
  default_ttl = None;
  cleanup_interval = 60.0;
}

(** {1 Helpers} *)

let now () = Unix.gettimeofday ()

let with_lock mutex f =
  Mutex.lock mutex;
  Fun.protect ~finally:(fun () -> Mutex.unlock mutex) f

(** Move key to front of access order *)
let touch_key key access_order =
  key :: (List.filter (fun k -> k <> key) access_order)

(** Check if entry is expired *)
let is_expired entry =
  match entry.expires_at with
  | None -> false
  | Some exp -> now () > exp

(** {1 Cache Creation} *)

(** Create a new cache.

    @param max_size Maximum number of entries (default: 1000)
    @param default_ttl Default TTL in seconds (default: None = no expiration)
*)
let create ?(max_size = 1000) ?default_ttl ?(cleanup_interval = 60.0) () =
  let config = { max_size; default_ttl; cleanup_interval } in
  let stats = {
    hits = 0;
    misses = 0;
    evictions = 0;
    expirations = 0;
    current_size = 0;
    max_size;
  } in
  {
    entries = Hashtbl.create max_size;
    access_order = [];
    config;
    stats;
    mutex = Mutex.create ();
  }

(** {1 Cache Operations} *)

(** Get a value from the cache.

    Returns [Some value] if found and not expired, [None] otherwise.
    Updates access time for LRU tracking.
*)
let get cache key =
  with_lock cache.mutex (fun () ->
    match Hashtbl.find_opt cache.entries key with
    | None ->
      cache.stats <- { cache.stats with misses = cache.stats.misses + 1 };
      None
    | Some entry when is_expired entry ->
      (* Remove expired entry *)
      Hashtbl.remove cache.entries key;
      cache.access_order <- List.filter (fun k -> k <> key) cache.access_order;
      cache.stats <- {
        cache.stats with
        misses = cache.stats.misses + 1;
        expirations = cache.stats.expirations + 1;
        current_size = cache.stats.current_size - 1;
      };
      None
    | Some entry ->
      (* Update access time and count *)
      entry.last_access <- now ();
      entry.access_count <- entry.access_count + 1;
      cache.access_order <- touch_key key cache.access_order;
      cache.stats <- { cache.stats with hits = cache.stats.hits + 1 };
      Some entry.value
  )

(** Set a value in the cache.

    @param ttl Optional TTL override (uses default_ttl if not specified)
*)
let set ?ttl cache key value =
  with_lock cache.mutex (fun () ->
    let time = now () in
    let ttl = match ttl with
      | Some t -> Some t
      | None -> cache.config.default_ttl
    in
    let expires_at = Option.map (fun t -> time +. t) ttl in
    let entry = {
      value;
      expires_at;
      last_access = time;
      access_count = 1;
    } in

    (* Check if key already exists *)
    let is_new = not (Hashtbl.mem cache.entries key) in

    (* Evict if necessary (only for new entries) *)
    if is_new && Hashtbl.length cache.entries >= cache.config.max_size then begin
      (* Remove least recently used (last in access_order) *)
      match List.rev cache.access_order with
      | [] -> ()
      | lru_key :: rest ->
        Hashtbl.remove cache.entries lru_key;
        cache.access_order <- List.rev rest;
        cache.stats <- { cache.stats with
          evictions = cache.stats.evictions + 1;
          current_size = cache.stats.current_size - 1;
        }
    end;

    Hashtbl.replace cache.entries key entry;
    cache.access_order <- touch_key key cache.access_order;
    if is_new then
      cache.stats <- { cache.stats with current_size = cache.stats.current_size + 1 }
  )

(** Remove a key from the cache *)
let remove cache key =
  with_lock cache.mutex (fun () ->
    if Hashtbl.mem cache.entries key then begin
      Hashtbl.remove cache.entries key;
      cache.access_order <- List.filter (fun k -> k <> key) cache.access_order;
      cache.stats <- { cache.stats with current_size = cache.stats.current_size - 1 };
      true
    end else
      false
  )

(** Check if a key exists (without updating access time) *)
let mem cache key =
  with_lock cache.mutex (fun () ->
    match Hashtbl.find_opt cache.entries key with
    | None -> false
    | Some entry -> not (is_expired entry)
  )

(** Get value or compute and cache it *)
let get_or_set ?ttl cache key compute =
  match get cache key with
  | Some v -> v
  | None ->
    let v = compute () in
    set ?ttl cache key v;
    v

(** {1 Bulk Operations} *)

(** Clear all entries *)
let clear cache =
  with_lock cache.mutex (fun () ->
    Hashtbl.clear cache.entries;
    cache.access_order <- [];
    cache.stats <- { cache.stats with current_size = 0 }
  )

(** Remove all expired entries *)
let cleanup cache =
  with_lock cache.mutex (fun () ->
    let expired_keys = Hashtbl.fold (fun k entry acc ->
      if is_expired entry then k :: acc else acc
    ) cache.entries [] in
    List.iter (fun k ->
      Hashtbl.remove cache.entries k;
      cache.access_order <- List.filter (fun key -> key <> k) cache.access_order
    ) expired_keys;
    let count = List.length expired_keys in
    cache.stats <- {
      cache.stats with
      expirations = cache.stats.expirations + count;
      current_size = cache.stats.current_size - count;
    };
    count
  )

(** Get all keys (for debugging) *)
let keys cache =
  with_lock cache.mutex (fun () ->
    Hashtbl.fold (fun k _ acc -> k :: acc) cache.entries []
  )

(** {1 Statistics} *)

(** Get cache statistics *)
let stats cache =
  with_lock cache.mutex (fun () ->
    { cache.stats with current_size = Hashtbl.length cache.entries }
  )

(** Get hit rate (0.0 - 1.0) *)
let hit_rate cache =
  let s = stats cache in
  let total = s.hits + s.misses in
  if total = 0 then 0.0
  else float_of_int s.hits /. float_of_int total

(** Get current size *)
let size cache =
  with_lock cache.mutex (fun () ->
    Hashtbl.length cache.entries
  )

(** {1 Iteration} *)

(** Iterate over all non-expired entries *)
let iter f cache =
  with_lock cache.mutex (fun () ->
    Hashtbl.iter (fun k entry ->
      if not (is_expired entry) then f k entry.value
    ) cache.entries
  )

(** Fold over all non-expired entries *)
let fold f cache init =
  with_lock cache.mutex (fun () ->
    Hashtbl.fold (fun k entry acc ->
      if not (is_expired entry) then f k entry.value acc else acc
    ) cache.entries init
  )

(** {1 Entry Metadata} *)

(** Get entry metadata (for debugging/monitoring) *)
let entry_info cache key =
  with_lock cache.mutex (fun () ->
    match Hashtbl.find_opt cache.entries key with
    | None -> None
    | Some entry ->
      let age = now () -. entry.last_access in
      let ttl_remaining = Option.map (fun exp -> exp -. now ()) entry.expires_at in
      Some (age, ttl_remaining, entry.access_count)
  )

(** {1 Specialized Caches} *)

(** String-keyed cache (most common) *)
module StringCache = struct
  type nonrec 'v t = (string, 'v) t

  let create = create
  let get = get
  let set = set
  let remove = remove
  let mem = mem
  let clear = clear
  let stats = stats
  let size = size
end

(** Integer-keyed cache *)
module IntCache = struct
  type nonrec 'v t = (int, 'v) t

  let create = create
  let get = get
  let set = set
  let remove = remove
  let mem = mem
  let clear = clear
  let stats = stats
  let size = size
end

(** {1 Cache Middleware Helper} *)

(** Create a caching wrapper for request handlers

    {[
      let handler = Cache.memoize cache ~key_fn:(fun req -> Request.path req)
        (fun req -> expensive_computation req)
    ]}
*)
let memoize ?ttl cache ~key_fn f =
  fun arg ->
    let key = key_fn arg in
    get_or_set ?ttl cache key (fun () -> f arg)

(** Create cache key from multiple parts *)
let make_key parts =
  String.concat ":" parts
