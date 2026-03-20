val set_clock : float Eio.Time.clock_ty Eio.Resource.t -> unit
val clear_clock : unit -> unit
val has_clock : unit -> bool
val now : unit -> float
val now_ms : unit -> int
val now_us : unit -> Int64.t
val sleep : float -> unit
val timed : (unit -> 'a) -> 'a * float
val timed_ms : (unit -> 'a) -> 'a * int
