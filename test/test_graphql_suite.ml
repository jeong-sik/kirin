(** GraphQL and Introspection Control tests *)

open Alcotest
open Test_helpers

module GQL = Kirin.Graphql

(* GraphQL Tests *)

let test_graphql_schema_create () =
  let schema = GQL.schema [
    GQL.field "hello" ~typ:(GQL.non_null GQL.string)
      ~args:GQL.Arg.[]
      ~resolve:(fun _info () -> "Hello, World!");
  ] in
  (* Schema created - check it is a heap value *)
  check Alcotest.bool "schema created" true (schema |> Obj.repr |> Obj.is_block)

let test_graphql_execute () =
  let schema = GQL.schema [
    GQL.field "greeting" ~typ:(GQL.non_null GQL.string)
      ~args:GQL.Arg.[arg "name" ~typ:GQL.Arg.string]
      ~resolve:(fun _info () name_opt ->
        let name = Option.value ~default:"World" name_opt in
        "Hello, " ^ name ^ "!");
  ] in
  let result = GQL.execute schema ~ctx:() ~query:"{ greeting }" () in
  match result with
  | Ok (`Response json) ->
    let expected = `Assoc [("data", `Assoc [("greeting", `String "Hello, World!")])] in
    check Alcotest.string "graphql response" (Yojson.Basic.to_string expected) (Yojson.Basic.to_string json)
  | _ -> fail "Expected response"

let test_graphql_execute_with_args () =
  let schema = GQL.schema [
    GQL.field "greeting" ~typ:(GQL.non_null GQL.string)
      ~args:GQL.Arg.[arg "name" ~typ:GQL.Arg.string]
      ~resolve:(fun _info () name_opt ->
        let name = Option.value ~default:"World" name_opt in
        "Hello, " ^ name ^ "!");
  ] in
  let result = GQL.execute schema ~ctx:() ~query:{|{ greeting(name: "Alice") }|} () in
  match result with
  | Ok (`Response json) ->
    let expected = `Assoc [("data", `Assoc [("greeting", `String "Hello, Alice!")])] in
    check Alcotest.string "graphql response with args" (Yojson.Basic.to_string expected) (Yojson.Basic.to_string json)
  | _ -> fail "Expected response"

let test_graphql_object_type () =
  let user_type = GQL.obj "User" ~fields:[
    GQL.field "id" ~typ:(GQL.non_null GQL.string)
      ~args:GQL.Arg.[]
      ~resolve:(fun _info (id, _name) -> id);
    GQL.field "name" ~typ:(GQL.non_null GQL.string)
      ~args:GQL.Arg.[]
      ~resolve:(fun _info (_id, name) -> name);
  ] in
  let schema = GQL.schema [
    GQL.field "user" ~typ:user_type
      ~args:GQL.Arg.[]
      ~resolve:(fun _info () -> Some ("1", "Alice"));
  ] in
  let result = GQL.execute schema ~ctx:() ~query:"{ user { id name } }" () in
  match result with
  | Ok (`Response json) ->
    let expected = `Assoc [("data", `Assoc [("user", `Assoc [("id", `String "1"); ("name", `String "Alice")])])] in
    check Alcotest.string "user object" (Yojson.Basic.to_string expected) (Yojson.Basic.to_string json)
  | _ -> fail "Expected response"

let test_graphql_parse_request () =
  let body = {|{"query":"{ hello }","operationName":"GetHello","variables":{"id":"1"}}|} in
  let req = GQL.parse_request body in
  match req with
  | Some r ->
    check Alcotest.string "query" "{ hello }" r.query;
    check (option Alcotest.string) "operation name" (Some "GetHello") r.operation_name;
    check Alcotest.bool "variables present" true (Option.is_some r.variables)
  | None -> fail "Failed to parse request"

let test_graphql_parse_request_minimal () =
  let body = {|{"query":"{ hello }"}|} in
  let req = GQL.parse_request body in
  match req with
  | Some r ->
    check Alcotest.string "query" "{ hello }" r.query;
    check (option Alcotest.string) "operation name" None r.operation_name;
    check Alcotest.bool "variables none" true (Option.is_none r.variables)
  | None -> fail "Failed to parse request"

let test_graphql_error_helpers () =
  let err = GQL.error "Something went wrong" in
  check Alcotest.string "error message" "Something went wrong" err.message;
  let err_loc = GQL.error_with_location "Parse error" ~line:10 ~column:5 in
  check (option (list (pair Alcotest.int Alcotest.int))) "locations"
    (Some [(10, 5)]) err_loc.locations;
  let err_path = GQL.error_with_path "Field error" ["user"; "name"] in
  check (option (list Alcotest.string)) "path"
    (Some ["user"; "name"]) err_path.path

let test_graphql_playground () =
  let req = make_test_request "/graphql" in
  let resp = GQL.playground_handler req in
  check Alcotest.int "status" 200 (Kirin.Response.status_code resp);
  let body = response_body_to_string (Kirin.Response.body resp) in
  check Alcotest.bool "contains doctype" true (String.length body > 100 && String.sub body 0 15 = "<!DOCTYPE html>")

(* Issue #44: yojson_to_const_value must not silently map large int to 0 *)
let test_graphql_intlit_large () =
  let large_int_json = `Intlit "9999999999999999999" in
  let result = Kirin.Graphql.yojson_to_const_value large_int_json in
  (match result with
   | `Int 0 -> Alcotest.fail "large Intlit silently mapped to 0"
   | `Float _ | `String _ -> ()  (* acceptable fallbacks *)
   | `Int _ -> ()  (* if it somehow fits, fine *)
   | _ -> Alcotest.fail "unexpected const_value type")

let test_graphql_intlit_small () =
  let small_json = `Intlit "42" in
  let result = Kirin.Graphql.yojson_to_const_value small_json in
  check bool "small Intlit parsed as Int 42" true
    (result = `Int 42)

(* Issue #46: batched GraphQL handler must reject oversized batches *)
let test_graphql_batch_size_cap () =
  let schema = Kirin.Graphql.schema [
    Kirin.Graphql.field "hello"
      ~typ:(Kirin.Graphql.non_null Kirin.Graphql.string)
      ~args:Kirin.Graphql.Arg.[]
      ~resolve:(fun _info () -> "world")
  ] in
  (* Create a batch with 11 queries (default max is 10) *)
  let queries = List.init 11 (fun _ -> `Assoc [("query", `String "{ hello }")]) in
  let body_json = Yojson.Safe.to_string (`List queries) in
  let req = make_test_request ~meth:`POST ~body:body_json "/graphql" in
  let resp = Kirin.Graphql.batched_handler schema req in
  check int "oversized batch returns 400" 400 (Kirin.Response.status_code resp)

let test_graphql_batch_within_limit () =
  let schema = Kirin.Graphql.schema [
    Kirin.Graphql.field "hello"
      ~typ:(Kirin.Graphql.non_null Kirin.Graphql.string)
      ~args:Kirin.Graphql.Arg.[]
      ~resolve:(fun _info () -> "world")
  ] in
  (* Create a batch with 3 queries (within default limit) *)
  let queries = List.init 3 (fun _ -> `Assoc [("query", `String "{ hello }")]) in
  let body_json = Yojson.Safe.to_string (`List queries) in
  let req = make_test_request ~meth:`POST ~body:body_json "/graphql" in
  let resp = Kirin.Graphql.batched_handler schema req in
  check int "valid batch returns 200" 200 (Kirin.Response.status_code resp)

let graphql_tests = [
  test_case "schema create" `Quick test_graphql_schema_create;
  test_case "execute query" `Quick test_graphql_execute;
  test_case "execute with args" `Quick test_graphql_execute_with_args;
  test_case "object type" `Quick test_graphql_object_type;
  test_case "parse request" `Quick test_graphql_parse_request;
  test_case "parse request minimal" `Quick test_graphql_parse_request_minimal;
  test_case "error helpers" `Quick test_graphql_error_helpers;
  test_case "playground handler" `Quick test_graphql_playground;
  test_case "intlit large" `Quick test_graphql_intlit_large;
  test_case "intlit small" `Quick test_graphql_intlit_small;
  test_case "batch size cap" `Quick test_graphql_batch_size_cap;
  test_case "batch within limit" `Quick test_graphql_batch_within_limit;
]

(* Introspection Control Tests (#47) *)

let test_schema_for_introspection () =
  Kirin.Graphql.schema [
    Kirin.Graphql.field "hello"
      ~typ:(Kirin.Graphql.non_null Kirin.Graphql.string)
      ~args:Kirin.Graphql.Arg.[]
      ~resolve:(fun _info () -> "world")
  ]

let test_introspection_enabled_get () =
  let schema = test_schema_for_introspection () in
  let mw = Kirin.Graphql.middleware ~enable_introspection:true schema in
  let fallback _req = Kirin.Response.text "fallback" in
  let req = make_test_request ~meth:`GET "/graphql" in
  let resp = mw fallback req in
  check int "GET returns 200 when enabled" 200
    (Kirin.Response.status_code resp)

let test_introspection_disabled_get () =
  let schema = test_schema_for_introspection () in
  let mw = Kirin.Graphql.middleware ~enable_introspection:false schema in
  let fallback _req = Kirin.Response.text "fallback" in
  let req = make_test_request ~meth:`GET "/graphql" in
  let resp = mw fallback req in
  check int "GET returns 403 when disabled" 403
    (Kirin.Response.status_code resp)

let test_introspection_disabled_post_schema_query () =
  let schema = test_schema_for_introspection () in
  let mw = Kirin.Graphql.middleware ~enable_introspection:false schema in
  let fallback _req = Kirin.Response.text "fallback" in
  let body = Yojson.Safe.to_string
    (`Assoc [("query", `String "{ __schema { types { name } } }")]) in
  let req = make_test_request ~meth:`POST
    ~headers:[("content-type", "application/json")]
    ~body "/graphql" in
  let resp = mw fallback req in
  check int "POST __schema returns 403 when disabled" 403
    (Kirin.Response.status_code resp)

let test_introspection_disabled_post_type_query () =
  let schema = test_schema_for_introspection () in
  let mw = Kirin.Graphql.middleware ~enable_introspection:false schema in
  let fallback _req = Kirin.Response.text "fallback" in
  let body = Yojson.Safe.to_string
    (`Assoc [("query", `String "{ __type(name: \"Query\") { name } }")]) in
  let req = make_test_request ~meth:`POST
    ~headers:[("content-type", "application/json")]
    ~body "/graphql" in
  let resp = mw fallback req in
  check int "POST __type returns 403 when disabled" 403
    (Kirin.Response.status_code resp)

let test_introspection_disabled_post_normal_query () =
  let schema = test_schema_for_introspection () in
  let mw = Kirin.Graphql.middleware ~enable_introspection:false schema in
  let fallback _req = Kirin.Response.text "fallback" in
  let body = Yojson.Safe.to_string
    (`Assoc [("query", `String "{ hello }")]) in
  let req = make_test_request ~meth:`POST
    ~headers:[("content-type", "application/json")]
    ~body "/graphql" in
  let resp = mw fallback req in
  (* Normal queries should still work when introspection is disabled *)
  check int "POST normal query returns 200" 200
    (Kirin.Response.status_code resp)

let test_introspection_default_enabled () =
  let schema = test_schema_for_introspection () in
  (* Default: introspection should be enabled *)
  let mw = Kirin.Graphql.middleware schema in
  let fallback _req = Kirin.Response.text "fallback" in
  let req = make_test_request ~meth:`GET "/graphql" in
  let resp = mw fallback req in
  check int "GET returns 200 by default" 200
    (Kirin.Response.status_code resp)

let introspection_tests = [
  test_case "enabled GET" `Quick test_introspection_enabled_get;
  test_case "disabled GET" `Quick test_introspection_disabled_get;
  test_case "disabled POST __schema" `Quick test_introspection_disabled_post_schema_query;
  test_case "disabled POST __type" `Quick test_introspection_disabled_post_type_query;
  test_case "disabled POST normal query" `Quick test_introspection_disabled_post_normal_query;
  test_case "default enabled" `Quick test_introspection_default_enabled;
]
