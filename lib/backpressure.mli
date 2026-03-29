(** Kirin Backpressure Module

    Flow control for streaming data between producers and consumers.
    Prevents fast producers from overwhelming slow consumers.

    @since 1.0.0
    @status stable

    {b Features:}
    - Bounded buffers with configurable capacity
    - Async channels for producer-consumer patterns
    - Rate limiting for controlled throughput
    - Windowed flow control *)

(** {1 Types} *)

(** Backpressure strategy. *)
type strategy =
  | Block (** Block producer until consumer catches up *)
  | Drop_oldest (** Drop oldest items when buffer full *)
  | Drop_newest (** Drop newest items when buffer full *)
  | Error (** Raise exception when buffer full *)

(** Raised when pushing to a full buffer with [Error] strategy. *)
exception Buffer_overflow

(** Raised when operating on a closed channel. *)
exception Channel_closed

(** {1 Bounded Buffer} *)

module Buffer : sig
  (** Bounded buffer with backpressure. *)
  type 'a t

  (** [create ?capacity ?strategy ()] creates a bounded buffer.
      @param capacity Maximum number of items (default: 1000)
      @param strategy Overflow strategy (default: [Block]) *)
  val create : ?capacity:int -> ?strategy:strategy -> unit -> 'a t

  (** [is_full t] returns [true] if the buffer is at capacity. *)
  val is_full : 'a t -> bool

  (** [is_empty t] returns [true] if the buffer has no items. *)
  val is_empty : 'a t -> bool

  (** [length t] returns the current number of items. *)
  val length : 'a t -> int

  (** [push t item] adds an item to the buffer.
      Behavior when full depends on the strategy. *)
  val push : 'a t -> 'a -> unit

  (** [pop t] removes and returns the oldest item (blocks if empty). *)
  val pop : 'a t -> 'a

  (** [try_pop t] removes the oldest item without blocking.
      Returns [None] if empty. *)
  val try_pop : 'a t -> 'a option

  (** [clear t] removes all items from the buffer. *)
  val clear : 'a t -> unit
end

(** {1 Async Channel} *)

module Channel : sig
  (** Async channel for producer-consumer patterns. *)
  type 'a t

  (** [create ?capacity ?strategy ()] creates a bounded channel.
      @param capacity Maximum buffered items (default: 1000)
      @param strategy Overflow strategy (default: [Block]) *)
  val create : ?capacity:int -> ?strategy:strategy -> unit -> 'a t

  (** [send t value] sends a value into the channel.
      @raise Channel_closed if the channel is closed. *)
  val send : 'a t -> 'a -> unit

  (** [recv t] receives a value (blocks if empty).
      @raise Channel_closed if closed and empty. *)
  val recv : 'a t -> 'a

  (** [try_recv t] receives without blocking. Returns [None] if empty or closed. *)
  val try_recv : 'a t -> 'a option

  (** [close t] closes the channel. Further sends raise [Channel_closed]. *)
  val close : 'a t -> unit

  (** [is_closed t] returns [true] if the channel is closed. *)
  val is_closed : 'a t -> bool

  (** [length t] returns the number of buffered items. *)
  val length : 'a t -> int

  (** [iter f t] calls [f] on each received value until the channel closes. *)
  val iter : ('a -> unit) -> 'a t -> unit

  (** [to_seq t] converts the channel to a lazy sequence. *)
  val to_seq : 'a t -> 'a Seq.t
end

(** {1 Rate Limiter} *)

module RateLimiter : sig
  (** Token bucket rate limiter. *)
  type t

  (** [create ?rate ?burst ()] creates a rate limiter.
      @param rate Tokens per second (default: 100.0)
      @param burst Maximum burst size (default: 10) *)
  val create : ?rate:float -> ?burst:int -> unit -> t

  (** [acquire t] acquires one token, blocking until available. *)
  val acquire : t -> unit

  (** [try_acquire t] tries to acquire a token without blocking.
      Returns [true] if successful. *)
  val try_acquire : t -> bool

  (** [acquire_n t n] acquires [n] tokens sequentially. *)
  val acquire_n : t -> int -> unit

  (** [available t] returns the current number of available tokens. *)
  val available : t -> int
end

(** {1 Windowed Flow Control} *)

module Window : sig
  (** Sliding window for flow control. *)
  type t

  (** [create ?initial_size ?max_size ()] creates a flow control window.
      @param initial_size Initial window size in bytes (default: 65536)
      @param max_size Maximum window size (default: 1048576) *)
  val create : ?initial_size:int -> ?max_size:int -> unit -> t

  (** [reserve t amount] reserves space, blocking if the window is full. *)
  val reserve : t -> int -> unit

  (** [release t amount] releases space back to the window. *)
  val release : t -> int -> unit

  (** [update_size t new_size] adjusts the window size (capped at max). *)
  val update_size : t -> int -> unit

  (** [available t] returns the available space. *)
  val available : t -> int

  (** [current_size t] returns the current window size. *)
  val current_size : t -> int

  (** [in_flight t] returns the number of bytes in flight. *)
  val in_flight : t -> int
end

(** {1 Flow Control Helpers} *)

(** [with_backpressure ~buffer producer] wraps a producer with buffer-based backpressure.
    Returns a new producer that pushes items through the buffer before yielding. *)
val with_backpressure : buffer:'a Buffer.t -> (('a -> 'b) -> 'c) -> ('a -> 'b) -> 'c

(** [with_rate_limit ~limiter producer] wraps a producer with rate limiting.
    Returns a new producer that acquires a token before each yield. *)
val with_rate_limit : limiter:RateLimiter.t -> (('a -> 'b) -> 'c) -> ('a -> 'b) -> 'c

(** [with_window ~window ~item_size producer] wraps a producer with windowed flow control.
    Returns a new producer that reserves/releases window space around each yield. *)
val with_window
  :  window:Window.t
  -> item_size:int
  -> (('a -> unit) -> 'b)
  -> ('a -> unit)
  -> 'b

(** {1 Statistics} *)

(** Backpressure statistics. *)
type stats =
  { items_sent : int
  ; items_dropped : int
  ; total_wait_time : float
  ; max_buffer_size : int
  }

(** Empty statistics record. *)
val empty_stats : stats
