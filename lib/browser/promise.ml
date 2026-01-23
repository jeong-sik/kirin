(** Kirin Browser - Promise Effect System

    Direct-style Promise handling using OCaml 5 effects.
    No Lwt dependency - pure effect-based async.

    {b Example:}
    {[
      Promise.run (fun () ->
        let response = Fetch.get "/api/users" in
        let data = Response.text response in
        Console.log data
      )
    ]}
*)

open Js_of_ocaml

(** {1 Types} *)

(** JavaScript Promise (opaque type) *)
type 'a t = Js.Unsafe.any

(** {1 Effect Definition} *)

(** Await effect - suspend until promise resolves *)
type _ Effect.t += Await : 'a t -> 'a Effect.t

(** {1 Direct-style API} *)

(** Await a promise (direct-style, performs Await effect) *)
let await (p : 'a t) : 'a = Effect.perform (Await p)

(** {1 Promise Creation} *)

(** Create a resolved promise *)
let resolve (x : 'a) : 'a t =
  Js.Unsafe.global##._Promise##resolve x

(** Create a rejected promise *)
let reject (exn : exn) : 'a t =
  Js.Unsafe.global##._Promise##reject (Js.Unsafe.inject (Printexc.to_string exn))

(** Create a new promise with resolver *)
let make (f : (('a -> unit) -> (exn -> unit) -> unit)) : 'a t =
  Js.Unsafe.new_obj Js.Unsafe.global##._Promise [|
    Js.Unsafe.inject (Js.wrap_callback (fun resolve reject ->
      f
        (fun v -> Js.Unsafe.fun_call resolve [| Js.Unsafe.inject v |] |> ignore)
        (fun e -> Js.Unsafe.fun_call reject [| Js.Unsafe.inject (Printexc.to_string e) |] |> ignore)
    ))
  |]

(** {1 Promise Combinators} *)

(** Map over a promise *)
let map (f : 'a -> 'b) (p : 'a t) : 'b t =
  Js.Unsafe.meth_call p "then" [|
    Js.Unsafe.inject (Js.wrap_callback f)
  |]

(** Chain promises (flatMap/bind) *)
let bind (p : 'a t) (f : 'a -> 'b t) : 'b t =
  Js.Unsafe.meth_call p "then" [|
    Js.Unsafe.inject (Js.wrap_callback f)
  |]

(** Catch errors *)
let catch (p : 'a t) (f : Js.Unsafe.any -> 'a t) : 'a t =
  Js.Unsafe.meth_call p "catch" [|
    Js.Unsafe.inject (Js.wrap_callback f)
  |]

(** Finally (always runs) *)
let finally (p : 'a t) (f : unit -> unit) : 'a t =
  Js.Unsafe.meth_call p "finally" [|
    Js.Unsafe.inject (Js.wrap_callback f)
  |]

(** {1 Runner} *)

(** Exception wrapper for JS errors *)
exception Js_error of string

(** Run a direct-style computation with Promise effect handler.

    This sets up an effect handler that converts Promise awaits
    into proper JavaScript Promise chaining.

    Returns a JS Promise that resolves when the computation completes.
*)
let run : 'a. (unit -> 'a) -> 'a t = fun f ->
  let open Effect.Deep in
  match_with f ()
    {
      retc = (fun x -> resolve x);
      exnc = (fun e -> reject e);
      effc = fun (type b) (eff : b Effect.t) ->
        match eff with
        | Await p ->
          Some (fun (k : (b, 'a t) continuation) ->
            (* Chain promise: when p resolves, continue with result *)
            bind p (fun result ->
              (* Continue the computation and recursively handle effects *)
              continue k result
            )
          )
        | _ -> None
    }

(** Run and forget (fire-and-forget style) *)
let run_ignore (f : unit -> unit) : unit =
  let (_ : Js.Unsafe.any) = run f in ()

(** {1 Utilities} *)

(** Wait for all promises *)
let all (ps : 'a t list) : 'a list t =
  let arr = Js.Unsafe.inject (Js.array (Array.of_list ps)) in
  let result = Js.Unsafe.global##._Promise##all arr in
  map (fun arr -> Array.to_list (Js.to_array arr)) result

(** Wait for first promise to resolve *)
let race (ps : 'a t list) : 'a t =
  let arr = Js.Unsafe.inject (Js.array (Array.of_list ps)) in
  Js.Unsafe.global##._Promise##race arr

(** Create a promise that resolves after delay (milliseconds) *)
let sleep (ms : int) : unit t =
  make (fun on_resolve _reject ->
    let callback = Js.wrap_callback (fun () -> on_resolve ()) in
    let _ = Dom_html.window##setTimeout callback (Js.float (float_of_int ms)) in
    ()
  )

(** {1 Conversion} *)

(** Wrap any JS value as a promise type *)
let of_js (p : Js.Unsafe.any) : 'a t = p

(** Unwrap to raw JS value *)
let to_js (p : 'a t) : Js.Unsafe.any = p
