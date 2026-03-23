(** Time Compatibility Layer - Eio-native timestamps with fallback.

    Provides a unified timestamp API for gradual migration from
    Unix.gettimeofday to Eio.Time.now.

    Usage:
    1. At server startup: [Time_compat.set_clock (Eio.Stdenv.clock env)]
    2. In code: [Time_compat.now ()] instead of [Unix.gettimeofday ()]

    When clock is not set (non-Eio contexts), falls back to Unix.gettimeofday.

    Originally from mcp-protocol-sdk, inlined to remove the hard dependency. *)

let global_clock : float Eio.Time.clock_ty Eio.Resource.t option ref = ref None

let set_clock clock = global_clock := Some clock
let clear_clock () = global_clock := None
let has_clock () = Option.is_some !global_clock

let now () =
  match !global_clock with
  | Some clock -> Eio.Time.now clock
  | None -> Unix.gettimeofday ()

let now_ms () = int_of_float (now () *. 1000.0)
let now_us () = Int64.of_float (now () *. 1_000_000.0)

let sleep seconds =
  match !global_clock with
  | Some clock -> Eio.Time.sleep clock seconds
  | None ->
      if seconds > 0.01 then
        Printf.eprintf "[WARN] [Time_compat] sleep %.3fs with Unix.sleepf (no Eio clock set)\n%!" seconds;
      Unix.sleepf seconds

let timed f =
  let start = now () in
  let result = f () in
  let duration = now () -. start in
  (result, duration)

let timed_ms f =
  let (result, duration) = timed f in
  (result, int_of_float (duration *. 1000.0))
