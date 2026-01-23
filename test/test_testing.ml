(** Testing Module Tests (Phase 17) *)

open Kirin

let test_request_tests = [
  "get request", `Quick, (fun () ->
    let req = Testing.Test_request.get "/api/users" in
    Alcotest.(check bool) "method is GET" true (req.meth = `GET);
    Alcotest.(check string) "path" "/api/users" req.path
  );

  "post request", `Quick, (fun () ->
    let req = Testing.Test_request.post "/api/users" in
    Alcotest.(check bool) "method is POST" true (req.meth = `POST);
    Alcotest.(check string) "path" "/api/users" req.path
  );

  "put request", `Quick, (fun () ->
    let req = Testing.Test_request.put "/api/users/1" in
    Alcotest.(check bool) "method is PUT" true (req.meth = `PUT)
  );

  "delete request", `Quick, (fun () ->
    let req = Testing.Test_request.delete "/api/users/1" in
    Alcotest.(check bool) "method is DELETE" true (req.meth = `DELETE)
  );

  "patch request", `Quick, (fun () ->
    let req = Testing.Test_request.patch "/api/users/1" in
    Alcotest.(check bool) "method is PATCH" true (req.meth = `PATCH)
  );

  "with headers", `Quick, (fun () ->
    let req = Testing.Test_request.get "/api"
      |> Testing.Test_request.with_header "Authorization" "Bearer token"
      |> Testing.Test_request.with_header "Accept" "application/json" in
    Alcotest.(check int) "header count" 2 (List.length req.headers);
    Alcotest.(check (option string)) "auth header" (Some "Bearer token")
      (List.assoc_opt "Authorization" req.headers)
  );

  "with query params", `Quick, (fun () ->
    let req = Testing.Test_request.get "/api/users"
      |> Testing.Test_request.with_query "page" "1"
      |> Testing.Test_request.with_query "limit" "10" in
    Alcotest.(check int) "query count" 2 (List.length req.query);
    Alcotest.(check (option string)) "page" (Some "1")
      (List.assoc_opt "page" req.query)
  );

  "with json body", `Quick, (fun () ->
    let req = Testing.Test_request.post_json "/api/users"
      (`Assoc [("name", `String "Alice")]) in
    Alcotest.(check (option string)) "content-type"
      (Some "application/json")
      (List.assoc_opt "Content-Type" req.headers);
    Alcotest.(check bool) "has body" true (req.body <> "")
  );

  "with bearer token", `Quick, (fun () ->
    let req = Testing.Test_request.get "/api"
      |> Testing.Test_request.with_bearer_token "mytoken" in
    Alcotest.(check (option string)) "auth header"
      (Some "Bearer mytoken")
      (List.assoc_opt "Authorization" req.headers)
  );
]

let test_response_tests = [
  "status code", `Quick, (fun () ->
    let resp = { Testing.Test_response.status = `OK; headers = []; body = "OK" } in
    Alcotest.(check int) "status" 200 (Testing.Test_response.status_code resp)
  );

  "is success", `Quick, (fun () ->
    let resp = { Testing.Test_response.status = `OK; headers = []; body = "OK" } in
    Alcotest.(check bool) "is success" true (Testing.Test_response.is_success resp)
  );

  "is redirect", `Quick, (fun () ->
    let resp = { Testing.Test_response.status = `Found; headers = []; body = "" } in
    Alcotest.(check bool) "is redirect" true (Testing.Test_response.is_redirect resp)
  );

  "is client error", `Quick, (fun () ->
    let resp = { Testing.Test_response.status = `Not_found; headers = []; body = "" } in
    Alcotest.(check bool) "is client error" true (Testing.Test_response.is_client_error resp)
  );

  "is server error", `Quick, (fun () ->
    let resp = { Testing.Test_response.status = `Internal_server_error; headers = []; body = "" } in
    Alcotest.(check bool) "is server error" true (Testing.Test_response.is_server_error resp)
  );

  "get header", `Quick, (fun () ->
    let resp = { Testing.Test_response.status = `OK;
                 headers = [("Content-Type", "application/json")];
                 body = "" } in
    Alcotest.(check (option string)) "content-type"
      (Some "application/json")
      (Testing.Test_response.header "content-type" resp)
  );

  "json body", `Quick, (fun () ->
    let json = `Assoc [("message", `String "hello")] in
    let resp = { Testing.Test_response.status = `OK;
                 headers = [];
                 body = Yojson.Safe.to_string json } in
    match Testing.Test_response.json resp with
    | Some j -> Alcotest.(check bool) "json match" true (j = json)
    | None -> Alcotest.fail "Should have JSON body"
  );
]

let json_path_tests = [
  "simple key", `Quick, (fun () ->
    let json = `Assoc [("name", `String "Alice")] in
    match Testing.Json_path.get "name" json with
    | Some (`String "Alice") -> ()
    | _ -> Alcotest.fail "Should find name"
  );

  "nested key", `Quick, (fun () ->
    let json = `Assoc [
      ("user", `Assoc [("name", `String "Bob")])
    ] in
    match Testing.Json_path.get "user.name" json with
    | Some (`String "Bob") -> ()
    | _ -> Alcotest.fail "Should find nested name"
  );

  "array index", `Quick, (fun () ->
    let json = `Assoc [
      ("users", `List [`String "A"; `String "B"; `String "C"])
    ] in
    match Testing.Json_path.get "users[1]" json with
    | Some (`String "B") -> ()
    | _ -> Alcotest.fail "Should find array element"
  );

  "complex path", `Quick, (fun () ->
    let json = `Assoc [
      ("data", `Assoc [
        ("items", `List [
          `Assoc [("id", `Int 1)];
          `Assoc [("id", `Int 2)];
        ])
      ])
    ] in
    match Testing.Json_path.get "data.items[1].id" json with
    | Some (`Int 2) -> ()
    | _ -> Alcotest.fail "Should find nested array element"
  );

  "missing key", `Quick, (fun () ->
    let json = `Assoc [("name", `String "Alice")] in
    match Testing.Json_path.get "unknown" json with
    | None -> ()
    | Some _ -> Alcotest.fail "Should return None"
  );

  "get string", `Quick, (fun () ->
    let json = `Assoc [("name", `String "Alice")] in
    match Testing.Json_path.get_string "name" json with
    | Some "Alice" -> ()
    | _ -> Alcotest.fail "Should get string"
  );

  "get int", `Quick, (fun () ->
    let json = `Assoc [("count", `Int 42)] in
    match Testing.Json_path.get_int "count" json with
    | Some 42 -> ()
    | _ -> Alcotest.fail "Should get int"
  );

  "get bool", `Quick, (fun () ->
    let json = `Assoc [("active", `Bool true)] in
    match Testing.Json_path.get_bool "active" json with
    | Some true -> ()
    | _ -> Alcotest.fail "Should get bool"
  );

  "get list", `Quick, (fun () ->
    let json = `Assoc [("items", `List [`Int 1; `Int 2; `Int 3])] in
    match Testing.Json_path.get_list "items" json with
    | Some items -> Alcotest.(check int) "length" 3 (List.length items)
    | None -> Alcotest.fail "Should get list"
  );
]

let assert_tests = [
  "assert status ok", `Quick, (fun () ->
    let resp = { Testing.Test_response.status = `OK; headers = []; body = "OK" } in
    Testing.Assert.status `OK resp  (* Should not raise *)
  );

  "assert success", `Quick, (fun () ->
    let resp = { Testing.Test_response.status = `OK; headers = []; body = "OK" } in
    Testing.Assert.success resp
  );

  "assert status code", `Quick, (fun () ->
    let resp = { Testing.Test_response.status = `Created; headers = []; body = "" } in
    Testing.Assert.status_code 201 resp
  );

  "assert header exists", `Quick, (fun () ->
    let resp = { Testing.Test_response.status = `OK;
                 headers = [("X-Custom", "value")];
                 body = "" } in
    Testing.Assert.header_exists "X-Custom" resp
  );

  "assert header value", `Quick, (fun () ->
    let resp = { Testing.Test_response.status = `OK;
                 headers = [("Content-Type", "application/json")];
                 body = "" } in
    Testing.Assert.header "Content-Type" "application/json" resp
  );

  "assert body contains", `Quick, (fun () ->
    let resp = { Testing.Test_response.status = `OK;
                 headers = [];
                 body = "Hello, World!" } in
    Testing.Assert.body_contains "World" resp
  );

  "assert body equals", `Quick, (fun () ->
    let resp = { Testing.Test_response.status = `OK;
                 headers = [];
                 body = "exact" } in
    Testing.Assert.body "exact" resp
  );

  "assert json path", `Quick, (fun () ->
    let json = `Assoc [("user", `Assoc [("name", `String "Alice")])] in
    let resp = { Testing.Test_response.status = `OK;
                 headers = [];
                 body = Yojson.Safe.to_string json } in
    Testing.Assert.json_path "user.name" (`String "Alice") resp
  );

  "assert json path string", `Quick, (fun () ->
    let json = `Assoc [("message", `String "hello")] in
    let resp = { Testing.Test_response.status = `OK;
                 headers = [];
                 body = Yojson.Safe.to_string json } in
    Testing.Assert.json_path_string "message" "hello" resp
  );

  "assert json path int", `Quick, (fun () ->
    let json = `Assoc [("count", `Int 42)] in
    let resp = { Testing.Test_response.status = `OK;
                 headers = [];
                 body = Yojson.Safe.to_string json } in
    Testing.Assert.json_path_int "count" 42 resp
  );
]

let mock_tests = [
  "mock endpoint", `Quick, (fun () ->
    let mock = Testing.Mock.create ()
      |> Testing.Mock.on_json ~meth:(Some `GET) ~path:"/api/users" ~status:`OK
          (`List [`String "Alice"; `String "Bob"]) in
    let req = Testing.Test_request.get "/api/users" in
    let resp = Testing.Mock.handle mock req in
    Alcotest.(check int) "status" 200 (Testing.Test_response.status_code resp)
  );

  "mock no match returns 404", `Quick, (fun () ->
    let mock = Testing.Mock.create ()
      |> Testing.Mock.on ~meth:(Some `GET) ~path:"/api/users" ~status:`OK ~body:"OK" in
    let req = Testing.Test_request.get "/api/other" in
    let resp = Testing.Mock.handle mock req in
    Alcotest.(check int) "status" 404 (Testing.Test_response.status_code resp)
  );

  "mock call count", `Quick, (fun () ->
    let mock = Testing.Mock.create ()
      |> Testing.Mock.on ~meth:(Some `GET) ~path:"/api/test" ~status:`OK ~body:"OK" in
    let req = Testing.Test_request.get "/api/test" in
    let _ = Testing.Mock.handle mock req in
    let _ = Testing.Mock.handle mock req in
    Alcotest.(check int) "call count" 2 (Testing.Mock.call_count ~path:"/api/test" mock)
  );

  "mock reset", `Quick, (fun () ->
    let mock = Testing.Mock.create ()
      |> Testing.Mock.on ~meth:(Some `GET) ~path:"/api/test" ~status:`OK ~body:"OK" in
    let req = Testing.Test_request.get "/api/test" in
    let _ = Testing.Mock.handle mock req in
    Testing.Mock.reset mock;
    Alcotest.(check int) "call count after reset" 0 (Testing.Mock.call_count ~path:"/api/test" mock)
  );

  "mock assert called", `Quick, (fun () ->
    let mock = Testing.Mock.create ()
      |> Testing.Mock.on ~meth:(Some `GET) ~path:"/api/test" ~status:`OK ~body:"OK" in
    let req = Testing.Test_request.get "/api/test" in
    let _ = Testing.Mock.handle mock req in
    Testing.Mock.assert_called ~path:"/api/test" mock
  );
]

let utility_tests = [
  "random string", `Quick, (fun () ->
    let s = Testing.random_string ~length:10 () in
    Alcotest.(check int) "length" 10 (String.length s)
  );

  "random strings unique", `Quick, (fun () ->
    let s1 = Testing.random_string () in
    let s2 = Testing.random_string () in
    Alcotest.(check bool) "different" true (s1 <> s2)
  );

  "random email", `Quick, (fun () ->
    let email = Testing.random_email () in
    Alcotest.(check bool) "contains @" true
      (String.contains email '@')
  );

  "random int in range", `Quick, (fun () ->
    let n = Testing.random_int ~min:10 ~max:20 () in
    Alcotest.(check bool) "in range" true (n >= 10 && n <= 20)
  );
]

let () =
  Alcotest.run "Testing" [
    ("Test_request", test_request_tests);
    ("Test_response", test_response_tests);
    ("Json_path", json_path_tests);
    ("Assert", assert_tests);
    ("Mock", mock_tests);
    ("Utility", utility_tests);
  ]
