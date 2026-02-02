(** Time Compatibility Layer - Eio-native timestamps with fallback

    Provides a unified timestamp API for gradual migration from
    Unix.gettimeofday to Eio.Time.now.

    Usage:
    1. At server startup: [Time_compat.set_clock (Eio.Stdenv.clock env)]
    2. In code: [Time_compat.now ()] instead of [Unix.gettimeofday ()]

    When clock is not set (non-Eio contexts), falls back to Unix.gettimeofday.
    This allows incremental migration without changing all call sites at once.

    @since 2026-02 - Async blocking pattern fixes
*)

(** Global clock reference - set at Eio_main.run startup *)
let global_clock : float Eio.Time.clock_ty Eio.Resource.t option ref = ref None

(** Set the global Eio clock. Call once at server startup.
    @param clock The Eio clock from [Eio.Stdenv.clock env] *)
let set_clock clock = global_clock := Some clock

(** Get current timestamp using Eio clock if available, Unix fallback otherwise.
    @return Current time as float (seconds since Unix epoch) *)
let now () =
  match !global_clock with
  | Some clock -> Eio.Time.now clock
  | None -> Unix.gettimeofday ()

(** Sleep for given duration using Eio if available, Unix fallback otherwise.
    @param seconds Duration to sleep *)
let sleep seconds =
  match !global_clock with
  | Some clock -> Eio.Time.sleep clock seconds
  | None -> Unix.sleepf seconds
