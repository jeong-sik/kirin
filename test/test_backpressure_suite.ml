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

(* Window validation regression tests.  Before this PR,
   [Window.create] and [Window.reserve] accepted misconfigurations
   that silently turned into permanent fiber hangs:

   - [create ~initial_size:0] / [~max_size:0] / [initial > max]
     left the wait loop unable to fall through
   - [reserve ~amount:0] or negative — wait condition is trivially
     false, falls through, [in_flight] decreased, invariant broken
   - [reserve ~amount:N] with [N > max_size] — wait condition can
     never become false even with full [release] / [update_size],
     so the fiber hangs forever *)

let test_window_create_rejects_zero_max () =
  check_raises "max_size 0"
    (Invalid_argument
       "Backpressure.Window.create: max_size must be > 0 (got 0)")
    (fun () -> ignore (BP.Window.create ~max_size:0 ()))

let test_window_create_rejects_negative_max () =
  check_raises "max_size -1"
    (Invalid_argument
       "Backpressure.Window.create: max_size must be > 0 (got -1)")
    (fun () -> ignore (BP.Window.create ~max_size:(-1) ()))

let test_window_create_rejects_zero_initial () =
  check_raises "initial 0"
    (Invalid_argument
       "Backpressure.Window.create: initial_size must be > 0 (got 0)")
    (fun () -> ignore (BP.Window.create ~initial_size:0 ()))

let test_window_create_rejects_initial_above_max () =
  check_raises "initial > max"
    (Invalid_argument
       "Backpressure.Window.create: initial_size (500) > max_size (100)")
    (fun () -> ignore (BP.Window.create ~initial_size:500 ~max_size:100 ()))

let test_window_reserve_rejects_nonpositive_amount () =
  let win = BP.Window.create ~initial_size:100 ~max_size:1000 () in
  check_raises "amount 0"
    (Invalid_argument
       "Backpressure.Window.reserve: amount must be > 0 (got 0)")
    (fun () -> BP.Window.reserve win 0);
  check_raises "amount -5"
    (Invalid_argument
       "Backpressure.Window.reserve: amount must be > 0 (got -5)")
    (fun () -> BP.Window.reserve win (-5));
  (* Invariant: in_flight stays at 0 — the rejected calls did not
     corrupt the counter. *)
  check int "in_flight unchanged" 0 (BP.Window.in_flight win)

let test_window_reserve_rejects_amount_above_max () =
  let win = BP.Window.create ~initial_size:100 ~max_size:200 () in
  check_raises "amount > max_size"
    (Invalid_argument
       "Backpressure.Window.reserve: amount 300 exceeds max_size 200 (would block forever)")
    (fun () -> BP.Window.reserve win 300);
  check int "in_flight unchanged" 0 (BP.Window.in_flight win)

let test_window_reserve_at_max_succeeds () =
  (* Boundary: exactly [max_size] amount is allowed (it will
     block until the window grows, but it is not an a-priori
     deadlock).  Pin so a future "off-by-one tightening" cannot
     regress the contract. *)
  let win = BP.Window.create ~initial_size:200 ~max_size:200 () in
  BP.Window.reserve win 200;
  check int "in_flight at cap" 200 (BP.Window.in_flight win)

let tests = [
  test_case "buffer create" `Quick (with_eio test_buffer_create);
  test_case "buffer push pop" `Quick (with_eio test_buffer_push_pop);
  test_case "buffer try_pop" `Quick (with_eio test_buffer_try_pop);
  test_case "buffer drop oldest" `Quick (with_eio test_buffer_drop_oldest);
  test_case "channel create" `Quick (with_eio test_channel_create);
  test_case "channel send recv" `Quick (with_eio test_channel_send_recv);
  test_case "channel close" `Quick (with_eio test_channel_close);
  test_case "rate limiter create" `Quick (with_eio test_rate_limiter_create);
  test_case "rate limiter acquire" `Quick (with_eio test_rate_limiter_acquire);
  test_case "window create" `Quick (with_eio test_window_create);
  test_case "window reserve release" `Quick (with_eio test_window_reserve_release);
  test_case "window update size" `Quick (with_eio test_window_update_size);
  test_case "window create rejects zero max" `Quick (with_eio test_window_create_rejects_zero_max);
  test_case "window create rejects negative max" `Quick (with_eio test_window_create_rejects_negative_max);
  test_case "window create rejects zero initial" `Quick (with_eio test_window_create_rejects_zero_initial);
  test_case "window create rejects initial above max" `Quick (with_eio test_window_create_rejects_initial_above_max);
  test_case "window reserve rejects nonpositive amount" `Quick (with_eio test_window_reserve_rejects_nonpositive_amount);
  test_case "window reserve rejects amount above max" `Quick (with_eio test_window_reserve_rejects_amount_above_max);
  test_case "window reserve at exact max succeeds" `Quick (with_eio test_window_reserve_at_max_succeeds);
]
