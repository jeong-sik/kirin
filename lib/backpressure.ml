(** Kirin Backpressure Module

    Flow control for streaming data between producers and consumers.
    Prevents fast producers from overwhelming slow consumers.

    Uses Eio.Mutex/Eio.Condition for cooperative scheduling
    (non-blocking to the Eio event loop).

    {b Features:}
    - Bounded buffers with configurable capacity
    - Async channels for producer-consumer patterns
    - Rate limiting for controlled throughput
    - Windowed flow control

    {b Example - Bounded Channel:}
    {[
      let ch = Backpressure.Channel.create ~capacity:100 () in

      (* Producer - blocks when buffer full *)
      Backpressure.Channel.send ch "data";

      (* Consumer - blocks when buffer empty *)
      let data = Backpressure.Channel.recv ch in
    ]}

    {b Example - Rate Limited Stream:}
    {[
      let limiter = Backpressure.RateLimiter.create ~rate:1000.0 () in

      Stream.response (fun yield ->
        for i = 1 to 10000 do
          Backpressure.RateLimiter.acquire limiter;
          yield (Printf.sprintf "Item %d\n" i)
        done)
    ]}
*)

(** {1 Types} *)

(** Backpressure strategy *)
type strategy =
  | Block        (** Block producer until consumer catches up *)
  | Drop_oldest  (** Drop oldest items when buffer full *)
  | Drop_newest  (** Drop newest items when buffer full *)
  | Error        (** Raise exception when buffer full *)

(** Buffer overflow error *)
exception Buffer_overflow

(** Channel closed error *)
exception Channel_closed

(** {1 Bounded Buffer} *)

module Buffer = struct
  (** Bounded buffer with backpressure *)
  type 'a t = {
    mutable items : 'a list;
    mutable size : int;
    capacity : int;
    strategy : strategy;
    mutex : Eio.Mutex.t;
    not_full : Eio.Condition.t;
    not_empty : Eio.Condition.t;
  }

  (** Create a bounded buffer *)
  let create ?(capacity = 1000) ?(strategy = Block) () = {
    items = [];
    size = 0;
    capacity;
    strategy;
    mutex = Eio.Mutex.create ();
    not_full = Eio.Condition.create ();
    not_empty = Eio.Condition.create ();
  }

  (** Check if buffer is full *)
  let is_full t = t.size >= t.capacity

  (** Check if buffer is empty *)
  let is_empty t = t.size = 0

  (** Current buffer size *)
  let length t = t.size

  (** Push item to buffer (may block or drop based on strategy) *)
  let push t item =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      (* Handle full buffer based on strategy *)
      while is_full t do
        match t.strategy with
        | Block ->
          Eio.Condition.await t.not_full t.mutex
        | Drop_oldest ->
          (* Remove oldest (last in reversed list) *)
          (match List.rev t.items with
           | [] -> ()
           | _ :: rest ->
             t.items <- List.rev rest;
             t.size <- t.size - 1)
        | Drop_newest ->
          (* Just don't add the new item *)
          ()
        | Error ->
          raise Buffer_overflow
      done;

      (* Add item if not using Drop_newest when full *)
      if not (is_full t && t.strategy = Drop_newest) then begin
        t.items <- item :: t.items;
        t.size <- t.size + 1;
        Eio.Condition.broadcast t.not_empty
      end
    )

  (** Pop item from buffer (blocks if empty) *)
  let pop t =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      while is_empty t do
        Eio.Condition.await t.not_empty t.mutex
      done;

      match List.rev t.items with
      | [] -> failwith "Buffer unexpectedly empty"
      | item :: rest ->
        t.items <- List.rev rest;
        t.size <- t.size - 1;
        Eio.Condition.broadcast t.not_full;
        item
    )

  (** Try to pop without blocking *)
  let try_pop t =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      if is_empty t then None
      else
        match List.rev t.items with
        | [] -> None
        | item :: rest ->
          t.items <- List.rev rest;
          t.size <- t.size - 1;
          Eio.Condition.broadcast t.not_full;
          Some item
    )

  (** Clear all items *)
  let clear t =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      t.items <- [];
      t.size <- 0;
      Eio.Condition.broadcast t.not_full
    )
end

(** {1 Async Channel} *)

module Channel = struct
  (** Async channel for producer-consumer patterns *)
  type 'a t = {
    buffer : 'a Buffer.t;
    mutable closed : bool;
    mutex : Eio.Mutex.t;
  }

  (** Create a channel with bounded capacity *)
  let create ?(capacity = 1000) ?(strategy = Block) () = {
    buffer = Buffer.create ~capacity ~strategy ();
    closed = false;
    mutex = Eio.Mutex.create ();
  }

  (** Send a value (may block based on strategy) *)
  let send t value =
    let is_closed = Eio.Mutex.use_ro t.mutex (fun () -> t.closed) in
    if is_closed then raise Channel_closed;
    Buffer.push t.buffer value

  (** Receive a value (blocks if empty) *)
  let recv t =
    let is_closed = Eio.Mutex.use_ro t.mutex (fun () -> t.closed) in
    if is_closed && Buffer.is_empty t.buffer then raise Channel_closed;
    Buffer.pop t.buffer

  (** Try to receive without blocking *)
  let try_recv t =
    let is_closed = Eio.Mutex.use_ro t.mutex (fun () -> t.closed) in
    if is_closed && Buffer.is_empty t.buffer then None
    else Buffer.try_pop t.buffer

  (** Close the channel *)
  let close t =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      t.closed <- true
    )

  (** Check if channel is closed *)
  let is_closed t =
    Eio.Mutex.use_ro t.mutex (fun () -> t.closed)

  (** Number of items in channel *)
  let length t = Buffer.length t.buffer

  (** Iterate over channel until closed *)
  let iter f t =
    try
      while true do
        let value = recv t in
        f value
      done
    with Channel_closed -> ()

  (** Convert channel to a Seq *)
  let to_seq t =
    let rec next () =
      match try_recv t with
      | Some v -> Seq.Cons (v, next)
      | None ->
        if is_closed t then Seq.Nil
        else begin
          (* Cooperative sleep - yields to Eio scheduler *)
          Time_compat.sleep 0.001;
          next ()
        end
    in
    next
end

(** {1 Rate Limiter} *)

module RateLimiter = struct
  (** Token bucket rate limiter *)
  type t = {
    rate : float;           (** Tokens per second *)
    burst : int;            (** Maximum burst size *)
    mutable tokens : float;
    mutable last_update : float;
    mutex : Eio.Mutex.t;
  }

  (** Create a rate limiter *)
  let create ?(rate = 100.0) ?(burst = 10) () = {
    rate;
    burst;
    tokens = float_of_int burst;
    last_update = Time_compat.now ();
    mutex = Eio.Mutex.create ();
  }

  (** Refill tokens based on elapsed time *)
  let refill t =
    let now = Time_compat.now () in
    let elapsed = now -. t.last_update in
    let new_tokens = t.tokens +. (elapsed *. t.rate) in
    t.tokens <- min (float_of_int t.burst) new_tokens;
    t.last_update <- now

  (** Acquire a token (blocks cooperatively if none available) *)
  let acquire t =
    let rec loop () =
      let acquired = Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        refill t;
        if t.tokens >= 1.0 then begin
          t.tokens <- t.tokens -. 1.0;
          true
        end else
          false
      ) in
      if not acquired then begin
        let wait_time = Eio.Mutex.use_ro t.mutex (fun () ->
          (1.0 -. t.tokens) /. t.rate
        ) in
        (* Cooperative sleep - yields to Eio scheduler *)
        Time_compat.sleep (min wait_time 0.1);
        loop ()
      end
    in
    loop ()

  (** Try to acquire a token without blocking *)
  let try_acquire t =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      refill t;
      if t.tokens >= 1.0 then begin
        t.tokens <- t.tokens -. 1.0;
        true
      end else
        false
    )

  (** Acquire multiple tokens *)
  let acquire_n t n =
    for _ = 1 to n do
      acquire t
    done

  (** Get current token count *)
  let available t =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      refill t;
      int_of_float t.tokens
    )
end

(** {1 Windowed Flow Control} *)

module Window = struct
  (** Sliding window for flow control *)
  type t = {
    mutable size : int;          (** Current window size *)
    max_size : int;              (** Maximum window size *)
    mutable in_flight : int;     (** Items in flight *)
    mutex : Eio.Mutex.t;
    space_available : Eio.Condition.t;
  }

  (** Create a window *)
  let create ?(initial_size = 65536) ?(max_size = 1048576) () = {
    size = initial_size;
    max_size;
    in_flight = 0;
    mutex = Eio.Mutex.create ();
    space_available = Eio.Condition.create ();
  }

  (** Reserve space in the window (blocks cooperatively if full) *)
  let reserve t amount =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      while t.in_flight + amount > t.size do
        Eio.Condition.await t.space_available t.mutex
      done;
      t.in_flight <- t.in_flight + amount
    )

  (** Release space in the window *)
  let release t amount =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      t.in_flight <- max 0 (t.in_flight - amount);
      Eio.Condition.broadcast t.space_available
    )

  (** Update window size (e.g., from WINDOW_UPDATE frame) *)
  let update_size t new_size =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      t.size <- min new_size t.max_size;
      Eio.Condition.broadcast t.space_available
    )

  (** Get available space *)
  let available t =
    Eio.Mutex.use_ro t.mutex (fun () ->
      max 0 (t.size - t.in_flight)
    )

  (** Get current window size *)
  let current_size t = t.size

  (** Get in-flight count *)
  let in_flight t = t.in_flight
end

(** {1 Flow Control Helpers} *)

(** Apply backpressure to a producer function *)
let with_backpressure ~buffer producer =
  fun yield ->
    producer (fun item ->
      Buffer.push buffer item;
      yield item
    )

(** Rate-limited producer *)
let with_rate_limit ~limiter producer =
  fun yield ->
    producer (fun item ->
      RateLimiter.acquire limiter;
      yield item
    )

(** Windowed producer *)
let with_window ~window ~item_size producer =
  fun yield ->
    producer (fun item ->
      Window.reserve window item_size;
      yield item;
      Window.release window item_size
    )

(** {1 Statistics} *)

type stats = {
  items_sent : int;
  items_dropped : int;
  total_wait_time : float;
  max_buffer_size : int;
}

let empty_stats = {
  items_sent = 0;
  items_dropped = 0;
  total_wait_time = 0.0;
  max_buffer_size = 0;
}
