(** Relay Support Tests *)

open Alcotest

(* Global ID Tests *)

let test_global_id_encode () =
  let encoded = Kirin.Graphql_relay.to_global_id "User" "123" in
  (* "User:123" base64 encoded is "VXNlcjoxMjM=" *)
  check string "encode" "VXNlcjoxMjM=" encoded

let test_global_id_decode () =
  let encoded = "VXNlcjoxMjM=" in
  match Kirin.Graphql_relay.from_global_id encoded with
  | Some (type_name, id) ->
    check string "type name" "User" type_name;
    check string "id" "123" id
  | None -> fail "decode failed"

let test_global_id_decode_invalid () =
  let encoded = "InvalidBase64" in
  match Kirin.Graphql_relay.from_global_id encoded with
  | Some _ -> fail "should fail"
  | None -> () (* decode failed as expected *)

(* Connection Tests *)

type user = { id : string; name : string }

let test_connection_from_list_simple () =
  let users = [
    { id = "1"; name = "Alice" };
    { id = "2"; name = "Bob" };
    { id = "3"; name = "Charlie" };
  ] in
  
  let args = Kirin.Graphql_relay.get_args None None None None in
  let conn = Kirin.Graphql_relay.connection_from_list users args in
  
  check int "total count" 3 (List.length conn.edges);
  check (option int) "total count field" (Some 3) conn.total_count;
  check bool "has next" false conn.page_info.has_next_page;
  check bool "has prev" false conn.page_info.has_previous_page

let test_connection_from_list_slice () =
  let users = [
    { id = "1"; name = "Alice" };
    { id = "2"; name = "Bob" };
    { id = "3"; name = "Charlie" };
    { id = "4"; name = "David" };
    { id = "5"; name = "Eve" };
  ] in
  
  (* first: 2 *)
  let args = Kirin.Graphql_relay.get_args (Some 2) None None None in
  let conn = Kirin.Graphql_relay.connection_from_list users args in
  
  check int "edges count" 2 (List.length conn.edges);
  check string "first edge" "Alice" (List.hd conn.edges).node.name;
  check string "first id" "1" (List.hd conn.edges).node.id;
  check bool "has next" true conn.page_info.has_next_page;
  check bool "has prev" false conn.page_info.has_previous_page

let test_connection_from_list_pagination () =
  let users = [
    { id = "1"; name = "Alice" };
    { id = "2"; name = "Bob" };
    { id = "3"; name = "Charlie" };
    { id = "4"; name = "David" };
    { id = "5"; name = "Eve" };
  ] in
  
  (* first: 2, after: "1" (index 1 -> Bob) *)
  (* Start index should be 1 + 1 = 2 (Charlie) *)
  let args = Kirin.Graphql_relay.get_args (Some 2) (Some "1") None None in
  let conn = Kirin.Graphql_relay.connection_from_list users args in
  
  check int "edges count" 2 (List.length conn.edges);
  check string "first edge" "Charlie" (List.hd conn.edges).node.name;
  check string "last edge" "David" (List.rev conn.edges |> List.hd).node.name;
  
  check bool "has next" true conn.page_info.has_next_page;
  check bool "has prev" true conn.page_info.has_previous_page

let global_id_tests = [
  "encode", `Quick, test_global_id_encode;
  "decode", `Quick, test_global_id_decode;
  "decode invalid", `Quick, test_global_id_decode_invalid;
]

let connection_tests = [
  "simple list", `Quick, test_connection_from_list_simple;
  "slice list", `Quick, test_connection_from_list_slice;
  "pagination", `Quick, test_connection_from_list_pagination;
]

let () =
  run "GraphQL Relay" [
    "Global ID", global_id_tests;
    "Connection", connection_tests;
  ]
