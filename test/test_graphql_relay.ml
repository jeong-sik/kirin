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
  (* Note: "1" encoded in base64 is "MQ==" *)
  let args = Kirin.Graphql_relay.get_args (Some 2) (Some "MQ==") None None in
  let conn = Kirin.Graphql_relay.connection_from_list users args in
  
  check int "edges count" 2 (List.length conn.edges);
  check string "first edge" "Charlie" (List.hd conn.edges).node.name;
  check string "last edge" "David" (List.rev conn.edges |> List.hd).node.name;
  
  check bool "has next" true conn.page_info.has_next_page;
  check bool "has prev" true conn.page_info.has_previous_page

(* Backward Pagination Tests *)

let users_5 = [
  { id = "1"; name = "Alice" };
  { id = "2"; name = "Bob" };
  { id = "3"; name = "Charlie" };
  { id = "4"; name = "David" };
  { id = "5"; name = "Eve" };
]

(* Cursor for index 3 (David) = base64("3") = "Mw==" *)
let cursor_3 = "Mw=="
(* Cursor for index 1 (Bob) = base64("1") = "MQ==" *)
let cursor_1 = "MQ=="
(* Cursor for index 4 (Eve) = base64("4") = "NA==" *)
let cursor_4 = "NA=="

let test_last_only () =
  (* last: 2 -> take the last 2 of [Alice, Bob, Charlie, David, Eve] = [David, Eve] *)
  let args = Kirin.Graphql_relay.get_args None None (Some 2) None in
  let conn = Kirin.Graphql_relay.connection_from_list users_5 args in

  check int "edges count" 2 (List.length conn.edges);
  check string "first edge" "David" (List.hd conn.edges).node.name;
  check string "last edge" "Eve" (List.rev conn.edges |> List.hd).node.name;
  check bool "has next" false conn.page_info.has_next_page;
  check bool "has prev" true conn.page_info.has_previous_page

let test_last_with_before () =
  (* last: 2, before: cursor_3 (index 3 = David)
     before removes David and Eve -> [Alice, Bob, Charlie]
     last: 2 -> [Bob, Charlie] *)
  let args = Kirin.Graphql_relay.get_args None None (Some 2) (Some cursor_3) in
  let conn = Kirin.Graphql_relay.connection_from_list users_5 args in

  check int "edges count" 2 (List.length conn.edges);
  check string "first edge" "Bob" (List.hd conn.edges).node.name;
  check string "last edge" "Charlie" (List.rev conn.edges |> List.hd).node.name;
  check bool "has next" true conn.page_info.has_next_page;
  check bool "has prev" true conn.page_info.has_previous_page

let test_before_only () =
  (* before: cursor_3 (index 3 = David)
     returns [Alice, Bob, Charlie] (indices 0,1,2)
     has_next = true because David, Eve exist after slice *)
  let args = Kirin.Graphql_relay.get_args None None None (Some cursor_3) in
  let conn = Kirin.Graphql_relay.connection_from_list users_5 args in

  check int "edges count" 3 (List.length conn.edges);
  check string "first edge" "Alice" (List.hd conn.edges).node.name;
  check string "last edge" "Charlie" (List.rev conn.edges |> List.hd).node.name;
  check bool "has next" true conn.page_info.has_next_page;
  check bool "has prev" false conn.page_info.has_previous_page

let test_after_and_before () =
  (* after: cursor_1 (index 1 = Bob), before: cursor_4 (index 4 = Eve)
     after removes Alice and Bob -> start at index 2
     before removes Eve -> end at index 4
     result: [Charlie, David] (indices 2,3)
     has_next = true (Eve excluded by before)
     has_prev = true (Alice, Bob excluded by after) *)
  let args = Kirin.Graphql_relay.get_args None (Some cursor_1) None (Some cursor_4) in
  let conn = Kirin.Graphql_relay.connection_from_list users_5 args in

  check int "edges count" 2 (List.length conn.edges);
  check string "first edge" "Charlie" (List.hd conn.edges).node.name;
  check string "last edge" "David" (List.rev conn.edges |> List.hd).node.name;
  check bool "has next" true conn.page_info.has_next_page;
  check bool "has prev" true conn.page_info.has_previous_page

let test_first_and_last () =
  (* first: 3, last: 2 on [Alice, Bob, Charlie, David, Eve]
     first: 3 -> [Alice, Bob, Charlie]
     last: 2 -> [Bob, Charlie] *)
  let args = Kirin.Graphql_relay.get_args (Some 3) None (Some 2) None in
  let conn = Kirin.Graphql_relay.connection_from_list users_5 args in

  check int "edges count" 2 (List.length conn.edges);
  check string "first edge" "Bob" (List.hd conn.edges).node.name;
  check string "last edge" "Charlie" (List.rev conn.edges |> List.hd).node.name;
  check bool "has next" true conn.page_info.has_next_page;
  check bool "has prev" true conn.page_info.has_previous_page

let test_last_larger_than_list () =
  (* last: 10 on 5 items -> returns all 5 *)
  let args = Kirin.Graphql_relay.get_args None None (Some 10) None in
  let conn = Kirin.Graphql_relay.connection_from_list users_5 args in

  check int "edges count" 5 (List.length conn.edges);
  check string "first edge" "Alice" (List.hd conn.edges).node.name;
  check string "last edge" "Eve" (List.rev conn.edges |> List.hd).node.name;
  check bool "has next" false conn.page_info.has_next_page;
  check bool "has prev" false conn.page_info.has_previous_page

let test_backward_page_walk () =
  (* Simulate backward pagination: page 1 = last 2, then last 2 before start_cursor *)
  let args1 = Kirin.Graphql_relay.get_args None None (Some 2) None in
  let conn1 = Kirin.Graphql_relay.connection_from_list users_5 args1 in

  check int "page 1 count" 2 (List.length conn1.edges);
  check string "page 1 first" "David" (List.hd conn1.edges).node.name;
  check string "page 1 last" "Eve" (List.rev conn1.edges |> List.hd).node.name;
  check bool "page 1 has prev" true conn1.page_info.has_previous_page;

  (* Use start_cursor of page 1 as before for page 2 *)
  let before_cursor = Option.get conn1.page_info.start_cursor in
  let args2 = Kirin.Graphql_relay.get_args None None (Some 2) (Some before_cursor) in
  let conn2 = Kirin.Graphql_relay.connection_from_list users_5 args2 in

  check int "page 2 count" 2 (List.length conn2.edges);
  check string "page 2 first" "Bob" (List.hd conn2.edges).node.name;
  check string "page 2 last" "Charlie" (List.rev conn2.edges |> List.hd).node.name;
  check bool "page 2 has prev" true conn2.page_info.has_previous_page;

  (* Page 3: last page *)
  let before_cursor2 = Option.get conn2.page_info.start_cursor in
  let args3 = Kirin.Graphql_relay.get_args None None (Some 2) (Some before_cursor2) in
  let conn3 = Kirin.Graphql_relay.connection_from_list users_5 args3 in

  check int "page 3 count" 1 (List.length conn3.edges);
  check string "page 3 first" "Alice" (List.hd conn3.edges).node.name;
  check bool "page 3 has prev" false conn3.page_info.has_previous_page

let test_empty_list_backward () =
  let args = Kirin.Graphql_relay.get_args None None (Some 2) None in
  let conn = Kirin.Graphql_relay.connection_from_list [] args in

  check int "edges count" 0 (List.length conn.edges);
  check bool "has next" false conn.page_info.has_next_page;
  check bool "has prev" false conn.page_info.has_previous_page;
  check (option string) "start_cursor" None conn.page_info.start_cursor;
  check (option string) "end_cursor" None conn.page_info.end_cursor

let global_id_tests = [
  "encode", `Quick, test_global_id_encode;
  "decode", `Quick, test_global_id_decode;
  "decode invalid", `Quick, test_global_id_decode_invalid;
]

let connection_tests = [
  "simple list", `Quick, test_connection_from_list_simple;
  "slice list", `Quick, test_connection_from_list_slice;
  "forward pagination", `Quick, test_connection_from_list_pagination;
]

let backward_tests = [
  "last only", `Quick, test_last_only;
  "last with before", `Quick, test_last_with_before;
  "before only", `Quick, test_before_only;
  "after and before", `Quick, test_after_and_before;
  "first and last combined", `Quick, test_first_and_last;
  "last larger than list", `Quick, test_last_larger_than_list;
  "backward page walk", `Quick, test_backward_page_walk;
  "empty list backward", `Quick, test_empty_list_backward;
]

let () =
  run "GraphQL Relay" [
    "Global ID", global_id_tests;
    "Connection", connection_tests;
    "Backward Pagination", backward_tests;
  ]
