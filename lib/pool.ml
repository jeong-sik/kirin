(** Kirin Connection Pool

    Generic, high-performance connection pool for managing expensive resources
    like database connections, HTTP clients, or any reusable connections.

    {b Features:}
    - Generic over any connection type
    - Configurable pool size (min/max)
    - Health checks with configurable interval
    - Idle connection timeout
    - Wait queue when pool is exhausted
    - Thread-safe via Eio Mutex
    - Direct-style async (no callbacks)

    {b Example - Database Pool:}
    {[
      let pool = Pool.create
        ~min_size:2
        ~max_size:10
        ~create:(fun () -> Db.connect "postgres://localhost/mydb")
        ~destroy:(fun conn -> Db.close conn)
        ~validate:(fun conn -> Db.ping conn)
        ()

      (* Use a connection *)
      Pool.use pool (fun conn ->
        Db.query conn "SELECT * FROM users")
    ]}

    {b Example - HTTP Client Pool:}
    {[
      let http_pool = Pool.create
        ~max_size:20
        ~create:(fun () -> Http_client.create ())
        ~destroy:(fun client -> Http_client.close client)
        ()

      Pool.use http_pool (fun client ->
        Http_client.get client "https://api.example.com/data")
    ]}
*)

(** {1 Types} *)

(** Pool configuration *)
type config = {
  min_size : int;           (** Minimum connections to keep (default: 1) *)
  max_size : int;           (** Maximum connections allowed (default: 10) *)
  idle_timeout : float;     (** Seconds before idle connection is closed (default: 300.0) *)
  max_wait_time : float;    (** Max seconds to wait for connection (default: 30.0) *)
  health_check_interval : float;  (** Seconds between health checks (default: 60.0) *)
}

(** Pool statistics *)
type stats = {
  total_connections : int;      (** Total connections created *)
  active_connections : int;     (** Currently in use *)
  idle_connections : int;       (** Available in pool *)
  waiting_requests : int;       (** Requests waiting for connection *)
  total_acquisitions : int;     (** Total successful acquires *)
  total_timeouts : int;         (** Total timeout errors *)
  total_errors : int;           (** Total connection errors *)
}

(** Pooled connection wrapper *)
type 'a pooled = {
  conn : 'a;
  created_at : float;
  mutable last_used : float;
  mutable use_count : int;
}

(** Connection pool *)
type 'a t = {
  config : config;
  create : unit -> 'a;
  destroy : 'a -> unit;
  validate : ('a -> bool) option;
  mutable connections : 'a pooled list;
  mutable in_use : int;
  mutable waiting : int;
  mutable stats : stats;
  mutex : Eio.Mutex.t;
  condition : Eio.Condition.t;
}

(** Pool errors *)
type error =
  | Timeout
  | Pool_exhausted
  | Connection_failed of string
  | Validation_failed

exception Pool_error of error

(** {1 Configuration} *)

(** Default pool configuration *)
let default_config = {
  min_size = 1;
  max_size = 10;
  idle_timeout = 300.0;
  max_wait_time = 30.0;
  health_check_interval = 60.0;
}

(** {1 Pool Creation} *)

(** Create a new connection pool.

    @param min_size Minimum connections to maintain (default: 1)
    @param max_size Maximum connections allowed (default: 10)
    @param idle_timeout Seconds before closing idle connection (default: 300)
    @param max_wait_time Max seconds to wait when pool exhausted (default: 30)
    @param create Function to create a new connection
    @param destroy Function to close a connection
    @param validate Optional function to check connection health
*)
let create
    ?(min_size = 1)
    ?(max_size = 10)
    ?(idle_timeout = 300.0)
    ?(max_wait_time = 30.0)
    ?(health_check_interval = 60.0)
    ~create
    ~destroy
    ?validate
    () =
  let config = {
    min_size;
    max_size;
    idle_timeout;
    max_wait_time;
    health_check_interval;
  } in
  let stats = {
    total_connections = 0;
    active_connections = 0;
    idle_connections = 0;
    waiting_requests = 0;
    total_acquisitions = 0;
    total_timeouts = 0;
    total_errors = 0;
  } in
  {
    config;
    create;
    destroy;
    validate;
    connections = [];
    in_use = 0;
    waiting = 0;
    stats;
    mutex = Eio.Mutex.create ();
    condition = Eio.Condition.create ();
  }

(** {1 Internal Helpers} *)

let now () = Time_compat.now ()

(** Run a function with the mutex locked *)
let with_lock mutex f =
  Eio.Mutex.use_rw ~protect:true mutex f

(** Create a new pooled connection *)
let create_pooled pool =
  let conn = pool.create () in
  let time = now () in
  {
    conn;
    created_at = time;
    last_used = time;
    use_count = 0;
  }

(** Validate a connection if validator is set *)
let is_valid pool pooled =
  match pool.validate with
  | None -> true
  | Some validate ->
    try validate pooled.conn
    with _ -> false

(** Check if connection is too old *)
let is_expired pool pooled =
  let age = now () -. pooled.last_used in
  age > pool.config.idle_timeout

(** Update stats *)
let update_stats pool f =
  pool.stats <- f pool.stats

(** {1 Pool Operations} *)

(** Get current pool statistics *)
let stats pool =
  Eio.Mutex.use_ro pool.mutex (fun () ->
    { pool.stats with
      active_connections = pool.in_use;
      idle_connections = List.length pool.connections;
      waiting_requests = pool.waiting;
    }
  )

(** Get pool size info *)
let size pool =
  Eio.Mutex.use_ro pool.mutex (fun () ->
    (pool.in_use, List.length pool.connections, pool.config.max_size)
  )

(** Acquire a connection from the pool.

    Blocks if no connections are available and pool is at max capacity.
    Raises Pool_error(Timeout) if max_wait_time is exceeded.
*)
let acquire pool =
  let start_time = now () in

  let try_acquire () =
    with_lock pool.mutex (fun () ->
      (* Try to get an idle connection *)
      match pool.connections with
      | pooled :: rest when is_valid pool pooled && not (is_expired pool pooled) ->
        pool.connections <- rest;
        pool.in_use <- pool.in_use + 1;
        pooled.last_used <- now ();
        pooled.use_count <- pooled.use_count + 1;
        update_stats pool (fun s -> { s with total_acquisitions = s.total_acquisitions + 1 });
        Some pooled

      | pooled :: rest ->
        (* Connection invalid or expired, destroy and try again *)
        pool.connections <- rest;
        pool.destroy pooled.conn;
        update_stats pool (fun s -> { s with total_connections = s.total_connections - 1 });
        None

      | [] when pool.in_use < pool.config.max_size ->
        (* Pool not full, create new connection *)
        let pooled = create_pooled pool in
        pool.in_use <- pool.in_use + 1;
        pooled.use_count <- 1;
        update_stats pool (fun s ->
          { s with
            total_connections = s.total_connections + 1;
            total_acquisitions = s.total_acquisitions + 1;
          });
        Some pooled

      | [] ->
        (* Pool exhausted, need to wait *)
        None
    )
  in

  let rec wait_for_connection () =
    let elapsed = now () -. start_time in
    if elapsed > pool.config.max_wait_time then begin
      update_stats pool (fun s -> { s with total_timeouts = s.total_timeouts + 1 });
      raise (Pool_error Timeout)
    end;

    match try_acquire () with
    | Some pooled -> pooled
    | None ->
      (* Wait for a connection to be released *)
      Eio.Mutex.use_rw ~protect:true pool.mutex (fun () ->
        pool.waiting <- pool.waiting + 1;
        Eio.Condition.await pool.condition pool.mutex;
        pool.waiting <- pool.waiting - 1
      );
      wait_for_connection ()
  in

  wait_for_connection ()

(** Release a connection back to the pool *)
let release pool pooled =
  with_lock pool.mutex (fun () ->
    pool.in_use <- pool.in_use - 1;
    pooled.last_used <- now ();

    (* Check if we should keep this connection *)
    let total = pool.in_use + List.length pool.connections in
    if total < pool.config.max_size && is_valid pool pooled then begin
      pool.connections <- pooled :: pool.connections;
    end else begin
      (* Pool full or connection invalid, destroy it *)
      pool.destroy pooled.conn;
      update_stats pool (fun s -> { s with total_connections = s.total_connections - 1 });
    end;

    (* Signal waiting threads *)
    Eio.Condition.broadcast pool.condition
  )

(** Use a connection from the pool with automatic release.

    The connection is automatically returned to the pool when the
    function completes, even if an exception is raised.

    {[
      Pool.use pool (fun conn ->
        Db.query conn "SELECT * FROM users")
    ]}
*)
let use pool f =
  let pooled = acquire pool in
  Fun.protect
    ~finally:(fun () -> release pool pooled)
    (fun () -> f pooled.conn)

(** Use a connection with the pooled wrapper (includes metadata) *)
let use_pooled pool f =
  let pooled = acquire pool in
  Fun.protect
    ~finally:(fun () -> release pool pooled)
    (fun () -> f pooled)

(** {1 Pool Maintenance} *)

(** Remove idle connections that have exceeded idle_timeout *)
let cleanup_idle pool =
  with_lock pool.mutex (fun () ->
    let (keep, remove) = List.partition (fun p -> not (is_expired pool p)) pool.connections in
    pool.connections <- keep;
    List.iter (fun p ->
      pool.destroy p.conn;
      update_stats pool (fun s -> { s with total_connections = s.total_connections - 1 })
    ) remove;
    List.length remove
  )

(** Ensure minimum connections are maintained *)
let ensure_minimum pool =
  with_lock pool.mutex (fun () ->
    let current = pool.in_use + List.length pool.connections in
    let needed = pool.config.min_size - current in
    if needed > 0 then begin
      for _ = 1 to needed do
        try
          let pooled = create_pooled pool in
          pool.connections <- pooled :: pool.connections;
          update_stats pool (fun s -> { s with total_connections = s.total_connections + 1 })
        with _ ->
          update_stats pool (fun s -> { s with total_errors = s.total_errors + 1 })
      done;
      needed
    end else
      0
  )

(** Validate all idle connections, removing invalid ones *)
let validate_all pool =
  with_lock pool.mutex (fun () ->
    let (valid, invalid) = List.partition (is_valid pool) pool.connections in
    pool.connections <- valid;
    List.iter (fun p ->
      pool.destroy p.conn;
      update_stats pool (fun s -> { s with total_connections = s.total_connections - 1 })
    ) invalid;
    List.length invalid
  )

(** {1 Pool Lifecycle} *)

(** Initialize the pool with minimum connections *)
let init pool =
  ignore (ensure_minimum pool)

(** Close all connections and shutdown the pool *)
let shutdown pool =
  with_lock pool.mutex (fun () ->
    List.iter (fun p -> pool.destroy p.conn) pool.connections;
    pool.connections <- [];
    update_stats pool (fun s -> { s with total_connections = 0 })
  )

(** {1 Error Handling} *)

(** Convert error to string *)
let error_to_string = function
  | Timeout -> "Connection pool timeout"
  | Pool_exhausted -> "Connection pool exhausted"
  | Connection_failed msg -> Printf.sprintf "Connection failed: %s" msg
  | Validation_failed -> "Connection validation failed"

(** {1 Utilities} *)

(** Get connection metadata *)
let connection_info pooled =
  let age = now () -. pooled.created_at in
  let idle = now () -. pooled.last_used in
  (age, idle, pooled.use_count)

(** Check if pool has available connections *)
let has_available pool =
  Eio.Mutex.use_ro pool.mutex (fun () ->
    List.length pool.connections > 0 || pool.in_use < pool.config.max_size
  )

(** Get number of idle connections *)
let idle_count pool =
  Eio.Mutex.use_ro pool.mutex (fun () ->
    List.length pool.connections
  )

(** Get number of active connections *)
let active_count pool =
  Eio.Mutex.use_ro pool.mutex (fun () ->
    pool.in_use
  )
