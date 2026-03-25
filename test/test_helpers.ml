(** Shared test helpers for Kirin test suite *)

open Alcotest

(** Convert response body to string for testing *)
let response_body_to_string body =
  match body with
  | Kirin.Response.String s -> s
  | Kirin.Response.Stream _ -> "<stream>"
  | Kirin.Response.Producer _ -> "<producer>"

(** Construct a test request with optional method, headers, and body *)
let make_test_request ?(meth=`GET) ?(headers=[]) ?(body="") path =
  let raw = Http.Request.make ~meth ~headers:(Http.Header.of_list headers) path in
  let body_source = Eio.Flow.string_source body |> Eio.Buf_read.of_flow ~max_size:(max 1024 (String.length body + 1)) in
  Kirin.Request.make ~raw ~body_source

(** Check if [needle] is a substring of [haystack] *)
let string_contains haystack needle =
  let needle_len = String.length needle in
  let haystack_len = String.length haystack in
  if needle_len > haystack_len then false
  else
    let rec check i =
      if i > haystack_len - needle_len then false
      else if String.sub haystack i needle_len = needle then true
      else check (i + 1)
    in
    check 0

(** Run a test function inside the Eio runtime (simple, no switch/clock) *)
let with_eio f () = Eio_main.run @@ fun _env -> f ()

(** Alcotest re-exports for convenience *)
let test_case = test_case
let check = check
let fail = fail
let check_raises = check_raises
