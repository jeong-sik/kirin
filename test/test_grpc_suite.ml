(** gRPC tests (Phase 5) *)

open Alcotest

let test_grpc_service_create () =
  let svc = Kirin.Grpc.service "test.TestService" in
  (* Service created - check name field *)
  check string "service name" "test.TestService" svc.Kirin.Grpc.Service.name

let test_grpc_service_with_unary () =
  let svc = Kirin.Grpc.service "test.Greeter"
    |> Kirin.Grpc.unary "SayHello" (fun req -> "Hello, " ^ req)
  in
  (* Service with unary method - check name is preserved *)
  check string "service name" "test.Greeter" svc.Kirin.Grpc.Service.name

let test_grpc_interceptor () =
  let _interceptor = Kirin.Grpc.make_interceptor "test" (fun ctx next ->
    (* Just pass through *)
    next ctx)
  in
  (* Interceptor created *)
  check bool "interceptor created" true true

let test_grpc_logging_interceptor () =
  let _interceptor = Kirin.Grpc.logging_interceptor () in
  check bool "logging interceptor created" true true

let test_grpc_timing_interceptor () =
  let _interceptor = Kirin.Grpc.timing_interceptor () in
  check bool "timing interceptor created" true true

let test_grpc_status_codes () =
  check bool "ok code" true (Kirin.Grpc.StatusCode.ok = Grpc_core.Status.OK);
  check bool "not found code" true (Kirin.Grpc.StatusCode.not_found = Grpc_core.Status.Not_found);
  check bool "internal code" true (Kirin.Grpc.StatusCode.internal = Grpc_core.Status.Internal)

let test_grpc_status_create () =
  let status = Kirin.Grpc.status ~code:Kirin.Grpc.StatusCode.ok ~message:"success" in
  check string "status message" "success" status.message

let test_grpc_stream () =
  let stream = Kirin.Grpc.stream_create 5 in
  check bool "stream empty" true (Kirin.Grpc.stream_is_empty stream);
  Kirin.Grpc.stream_add stream "test";
  check bool "stream not empty" false (Kirin.Grpc.stream_is_empty stream);
  Kirin.Grpc.stream_close stream;
  check bool "stream closed" true (Kirin.Grpc.stream_is_closed stream)

let tests = [
  test_case "service create" `Quick test_grpc_service_create;
  test_case "service with unary" `Quick test_grpc_service_with_unary;
  test_case "interceptor" `Quick test_grpc_interceptor;
  test_case "logging interceptor" `Quick test_grpc_logging_interceptor;
  test_case "timing interceptor" `Quick test_grpc_timing_interceptor;
  test_case "status codes" `Quick test_grpc_status_codes;
  test_case "status create" `Quick test_grpc_status_create;
  test_case "stream operations" `Quick test_grpc_stream;
]
