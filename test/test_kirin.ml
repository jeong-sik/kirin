(** Kirin Test Suite
    Thin runner that aggregates all per-module test suites.
*)

let () =
  Alcotest.run "Kirin" [
    ("Response", Test_response_suite.tests);
    ("Router", Test_router_suite.router_tests);
    ("Handler", Test_router_suite.handler_tests);
    ("Static", Test_router_suite.static_tests);
    ("Cookie", Test_cookie_suite.tests);
    ("ETag", Test_etag_suite.tests);
    ("Multipart", Test_multipart_suite.tests);
    ("Compress", Test_compress_suite.tests);
    ("RateLimit", Test_ratelimit_suite.tests);
    ("WebSocket", Test_websocket_suite.tests);
    ("SSE", Test_sse_suite.tests);
    ("Template", Test_template_suite.tests);
    ("TLS", Test_tls_suite.tests);
    ("gRPC", Test_grpc_suite.tests);
    ("GraphQL", Test_graphql_suite.graphql_tests);
    ("Introspection", Test_graphql_suite.introspection_tests);
    (* Streaming tests live in test_streaming.ml due to Eio requirement *)
    ("Pool", Test_pool_suite.tests);
    ("Backpressure", Test_backpressure_suite.tests);
    ("Cache", Test_cache_suite.tests);
    ("Jobs", Test_jobs_suite.tests);
    ("Parallel", Test_parallel_suite.tests);
    ("Health", Test_health_suite.tests);
    ("Metrics", Test_metrics_suite.tests);
    ("Shutdown", Test_shutdown_suite.tests);
    ("WebRTC", Test_webrtc_suite.tests);
    ("Query", Test_query_suite.query_tests);
    ("Migrate", Test_query_suite.migrate_tests);
    ("Db", Test_query_suite.db_tests);
    ("Cluster", Test_cluster_suite.tests);
  ]
