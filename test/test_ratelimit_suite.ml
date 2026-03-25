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

let tests = [
  test_case "default config" `Quick test_ratelimit_default_config;
  test_case "middleware allows" `Quick (with_eio_rl test_ratelimit_middleware_allows);
  test_case "middleware limits" `Quick (with_eio_rl test_ratelimit_middleware_limits);
]
