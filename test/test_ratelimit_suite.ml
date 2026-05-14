(** Rate limit tests *)

open Alcotest
open Test_helpers

let test_ratelimit_default_config () =
  let config = Kirin.default_rate_limit_config in
  check (float 0.001) "requests per second" 10.0 config.requests_per_second;
  check int "burst size" 20 config.burst_size

let test_ratelimit_middleware_allows () =
  let config : Kirin.rate_limit_config = {
    requests_per_second = 100.0;  (* High rate for test *)
    burst_size = 100;
  } in
  let handler _req = Kirin.text "OK" in
  let with_limit = Kirin.rate_limit ~config handler in

  let req = make_test_request
    ~headers:[("x-forwarded-for", "127.0.0.1")]
    "/" in
  let resp = with_limit req in

  check int "status 200" 200 (Kirin.Response.status_code resp);
  check bool "has ratelimit header" true
    (Option.is_some (Kirin.Response.header "x-ratelimit-limit" resp))

let test_ratelimit_middleware_limits () =
  let config : Kirin.rate_limit_config = {
    requests_per_second = 0.001;  (* Very low rate *)
    burst_size = 1;               (* Only 1 allowed *)
  } in
  let handler _req = Kirin.text "OK" in
  let with_limit = Kirin.rate_limit ~config
    ~get_key:(fun _ -> "test-limit-client")  (* Fixed key for test *)
    handler in

  let make_req () =
    make_test_request
      ~headers:[("x-forwarded-for", "192.168.1.100")]
      ~meth:`GET
      "/"
  in

  (* First request should pass *)
  let resp1 = with_limit (make_req ()) in
  check int "first request 200" 200 (Kirin.Response.status_code resp1);

  (* Second request should be rate limited *)
  let resp2 = with_limit (make_req ()) in
  check int "second request 429" 429 (Kirin.Response.status_code resp2);
  check bool "has retry-after" true
    (Option.is_some (Kirin.Response.header "retry-after" resp2))

let with_eio_rl f () = Eio_main.run @@ fun _env -> f ()

(* X-Forwarded-For trust regression tests.

   Before this PR, [get_client_id req] returned the first
   comma-segment of X-Forwarded-For, which let any HTTP client
   pick its own rate-limit bucket key by editing a request header.
   Two concrete bypasses fell out of that:

   - rotate the XFF value per request → infinite unique buckets,
     no limit ever fires
   - send a victim's IP as XFF → drain the victim's bucket
     (targeted lockout)

   New contract: XFF is ignored unless the caller explicitly opts
   in with [~trust_forwarded:true], which they should only do
   behind a reverse proxy that rewrites the header.  Tests pin
   both halves. *)

let test_get_client_id_ignores_xff_by_default () =
  let req =
    make_test_request
      ~headers:[("x-forwarded-for", "1.2.3.4")]
      "/"
  in
  check string "XFF ignored by default" "unknown"
    (Kirin.Ratelimit.get_client_id req)

let test_get_client_id_ignores_real_ip_by_default () =
  let req =
    make_test_request
      ~headers:[("x-real-ip", "1.2.3.4")]
      "/"
  in
  check string "X-Real-IP ignored by default" "unknown"
    (Kirin.Ratelimit.get_client_id req)

let test_get_client_id_uses_xff_when_trusted () =
  let req =
    make_test_request
      ~headers:[("x-forwarded-for", "203.0.113.7, 10.0.0.1")]
      "/"
  in
  check string "first XFF segment used when trusted"
    "203.0.113.7"
    (Kirin.Ratelimit.get_client_id ~trust_forwarded:true req)

let test_get_client_id_falls_back_to_real_ip_when_trusted () =
  let req =
    make_test_request
      ~headers:[("x-real-ip", "198.51.100.42")]
      "/"
  in
  check string "X-Real-IP used when XFF absent and trust enabled"
    "198.51.100.42"
    (Kirin.Ratelimit.get_client_id ~trust_forwarded:true req)

let test_get_client_id_empty_xff_falls_back_to_unknown () =
  (* Some proxies emit an empty XFF on internal calls; that must
     not bind every such request to the "" bucket. *)
  let req =
    make_test_request
      ~headers:[("x-forwarded-for", "")]
      "/"
  in
  check string "empty XFF -> unknown"
    "unknown"
    (Kirin.Ratelimit.get_client_id ~trust_forwarded:true req);
  let req2 =
    make_test_request
      ~headers:[("x-forwarded-for", ",")]
      "/"
  in
  check string "comma-only XFF -> unknown"
    "unknown"
    (Kirin.Ratelimit.get_client_id ~trust_forwarded:true req2)

let test_xff_rotation_does_not_bypass_default () =
  (* The bypass scenario: a malicious client sends a fresh XFF on
     every request hoping to be assigned a fresh bucket.  In the
     secure default mode every such request maps to "unknown" and
     therefore shares one bucket — the limit fires on the third
     hit. *)
  let config : Kirin.rate_limit_config = {
    requests_per_second = 0.001;  (* effectively no refill during test *)
    burst_size = 2;
  } in
  let handler _req = Kirin.text "OK" in
  let with_limit = Kirin.rate_limit ~config handler in
  let make_req xff =
    make_test_request
      ~headers:[("x-forwarded-for", xff)]
      ~meth:`GET
      "/"
  in
  let r1 = with_limit (make_req "10.0.0.1") in
  let r2 = with_limit (make_req "10.0.0.2") in
  let r3 = with_limit (make_req "10.0.0.3") in
  check int "first burst slot" 200 (Kirin.Response.status_code r1);
  check int "second burst slot" 200 (Kirin.Response.status_code r2);
  check int "third request limited despite XFF rotation"
    429 (Kirin.Response.status_code r3)

let tests = [
  test_case "default config" `Quick test_ratelimit_default_config;
  test_case "middleware allows" `Quick (with_eio_rl test_ratelimit_middleware_allows);
  test_case "middleware limits" `Quick (with_eio_rl test_ratelimit_middleware_limits);
  test_case "XFF ignored by default" `Quick test_get_client_id_ignores_xff_by_default;
  test_case "X-Real-IP ignored by default" `Quick test_get_client_id_ignores_real_ip_by_default;
  test_case "XFF used when trusted" `Quick test_get_client_id_uses_xff_when_trusted;
  test_case "X-Real-IP fallback when trusted" `Quick test_get_client_id_falls_back_to_real_ip_when_trusted;
  test_case "empty XFF falls back" `Quick test_get_client_id_empty_xff_falls_back_to_unknown;
  test_case "XFF rotation cannot bypass default" `Quick (with_eio_rl test_xff_rotation_does_not_bypass_default);
]
