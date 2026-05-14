(** Backpressure tests (Phase 9) *)

open Alcotest
open Test_helpers

module BP = Kirin.Backpressure

let test_buffer_create () =
  let buf = BP.Buffer.create ~capacity:10 () in
  check bool "is empty" true (BP.Buffer.is_empty buf);
  check bool "not full" false (BP.Buffer.is_full buf);
  check int "length" 0 (BP.Buffer.length buf)

let test_buffer_push_pop () =
  let buf = BP.Buffer.create ~capacity:5 () in
  BP.Buffer.push buf 1;
  BP.Buffer.push buf 2;
  BP.Buffer.push buf 3;
  check int "length after push" 3 (BP.Buffer.length buf);
  let v1 = BP.Buffer.pop buf in
  check int "first pop" 1 v1;
  let v2 = BP.Buffer.pop buf in
  check int "second pop" 2 v2;
  check int "length after pop" 1 (BP.Buffer.length buf)

let test_buffer_try_pop () =
  let buf = BP.Buffer.create ~capacity:5 () in
  check (option int) "empty try_pop" None (BP.Buffer.try_pop buf);
  BP.Buffer.push buf 42;
  check (option int) "non-empty try_pop" (Some 42) (BP.Buffer.try_pop buf);
  check (option int) "empty again" None (BP.Buffer.try_pop buf)

let test_buffer_drop_oldest () =
  let buf = BP.Buffer.create ~capacity:3 ~strategy:Drop_oldest () in
  BP.Buffer.push buf 1;
  BP.Buffer.push buf 2;
  BP.Buffer.push buf 3;
  (* Buffer full, next push should drop oldest *)
  BP.Buffer.push buf 4;
  check int "length still 3" 3 (BP.Buffer.length buf);
  let v1 = BP.Buffer.pop buf in
  check int "oldest dropped" 2 v1  (* 1 was dropped *)

let test_channel_create () =
  let ch = BP.Channel.create ~capacity:100 () in
  check bool "not closed" false (BP.Channel.is_closed ch);
  check int "empty length" 0 (BP.Channel.length ch)

let test_channel_send_recv () =
  let ch = BP.Channel.create ~capacity:10 () in
  BP.Channel.send ch "hello";
  BP.Channel.send ch "world";
  check int "length" 2 (BP.Channel.length ch);
  let v1 = BP.Channel.recv ch in
  check string "first recv" "hello" v1;
  let v2 = BP.Channel.recv ch in
  check string "second recv" "world" v2

let test_channel_close () =
  let ch = BP.Channel.create ~capacity:10 () in
  BP.Channel.send ch "data";
  BP.Channel.close ch;
  check bool "is closed" true (BP.Channel.is_closed ch);
  (* Can still receive buffered data *)
  let v = BP.Channel.recv ch in
  check string "recv after close" "data" v;
  (* After buffer empty, raises Channel_closed *)
  check_raises "recv on closed empty" BP.Channel_closed (fun () ->
    ignore (BP.Channel.recv ch))

let test_rate_limiter_create () =
  let limiter = BP.RateLimiter.create ~rate:100.0 ~burst:10 () in
  let available = BP.RateLimiter.available limiter in
  check bool "has tokens" true (available > 0)

let test_rate_limiter_acquire () =
  let limiter = BP.RateLimiter.create ~rate:1000.0 ~burst:5 () in
  (* Should be able to acquire burst tokens immediately *)
  for _ = 1 to 5 do
    check bool "acquire" true (BP.RateLimiter.try_acquire limiter)
  done;
  (* After burst, should be empty (or nearly) *)
  check bool "burst exhausted" true (BP.RateLimiter.available limiter <= 1)

let test_window_create () =
  let win = BP.Window.create ~initial_size:1000 ~max_size:65536 () in
  check int "initial size" 1000 (BP.Window.current_size win);
  check int "available" 1000 (BP.Window.available win);
  check int "in flight" 0 (BP.Window.in_flight win)

let test_window_reserve_release () =
  let win = BP.Window.create ~initial_size:100 () in
  BP.Window.reserve win 30;
  check int "after reserve" 70 (BP.Window.available win);
  check int "in flight" 30 (BP.Window.in_flight win);
  BP.Window.release win 30;
  check int "after release" 100 (BP.Window.available win);
  check int "in flight after" 0 (BP.Window.in_flight win)

let test_window_update_size () =
  let win = BP.Window.create ~initial_size:100 ~max_size:500 () in
  BP.Window.update_size win 300;
  check int "new size" 300 (BP.Window.current_size win);
  (* Cannot exceed max *)
  BP.Window.update_size win 1000;
  check int "capped at max" 500 (BP.Window.current_size win)

(* Before this PR, [Drop_newest] inside [Buffer.push] was a fatal
   silent bug: the strategy's loop body was [()] inside
   [while is_full t], so once the buffer hit capacity the fiber
   spun the CPU forever — the strategy "drop the incoming item if
   the buffer is full" was effectively unimplemented. *)

let test_buffer_drop_newest_does_not_spin () =
  let buf = BP.Buffer.create ~capacity:2 ~strategy:Drop_newest () in
  BP.Buffer.push buf 1;
  BP.Buffer.push buf 2;
  (* Buffer at capacity.  These calls would spin forever before
     this PR; if the test returns at all, the push loop
     terminates. *)
  BP.Buffer.push buf 3;
  BP.Buffer.push buf 4;
  check int "size capped at capacity" 2 (BP.Buffer.length buf);
  (* The retained items are the original two in FIFO order; the
     newer ones must have been dropped. *)
  check int "first stays" 1 (BP.Buffer.pop buf);
  check int "second stays" 2 (BP.Buffer.pop buf);
  check int "buffer drained" 0 (BP.Buffer.length buf)

let test_buffer_drop_newest_room_after_pop () =
  (* Once the consumer drains an item the producer can refill the
     slot — Drop_newest is a per-push decision, not a permanent
     "buffer closed" state. *)
  let buf = BP.Buffer.create ~capacity:1 ~strategy:Drop_newest () in
  BP.Buffer.push buf 1;
  BP.Buffer.push buf 2;  (* dropped *)
  check int "first kept" 1 (BP.Buffer.pop buf);
  BP.Buffer.push buf 3;  (* room is back *)
  check int "third kept after drain" 3 (BP.Buffer.pop buf)

let test_buffer_capacity_zero_rejected () =
  check_raises "capacity 0"
    (Invalid_argument "Backpressure.Buffer.create: capacity must be > 0 (got 0)")
    (fun () -> ignore (BP.Buffer.create ~capacity:0 ()));
  check_raises "capacity -1"
    (Invalid_argument "Backpressure.Buffer.create: capacity must be > 0 (got -1)")
    (fun () -> ignore (BP.Buffer.create ~capacity:(-1) ()))

let tests = [
  test_case "buffer create" `Quick (with_eio test_buffer_create);
  test_case "buffer push pop" `Quick (with_eio test_buffer_push_pop);
  test_case "buffer try_pop" `Quick (with_eio test_buffer_try_pop);
  test_case "buffer drop oldest" `Quick (with_eio test_buffer_drop_oldest);
  test_case "buffer drop newest terminates" `Quick (with_eio test_buffer_drop_newest_does_not_spin);
  test_case "buffer drop newest room after pop" `Quick (with_eio test_buffer_drop_newest_room_after_pop);
  test_case "buffer capacity <= 0 rejected" `Quick (with_eio test_buffer_capacity_zero_rejected);
  test_case "channel create" `Quick (with_eio test_channel_create);
  test_case "channel send recv" `Quick (with_eio test_channel_send_recv);
  test_case "channel close" `Quick (with_eio test_channel_close);
  test_case "rate limiter create" `Quick (with_eio test_rate_limiter_create);
  test_case "rate limiter acquire" `Quick (with_eio test_rate_limiter_acquire);
  test_case "window create" `Quick (with_eio test_window_create);
  test_case "window reserve release" `Quick (with_eio test_window_reserve_release);
  test_case "window update size" `Quick (with_eio test_window_update_size);
]
