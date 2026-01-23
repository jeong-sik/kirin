(** Validation Module Tests (Phase 16) *)

open Kirin

let string_tests = [
  "valid string", `Quick, (fun () ->
    let schema = Validation.string () in
    match Validation.validate schema (`String "hello") with
    | Ok _ -> ()
    | Error _ -> Alcotest.fail "Should be valid"
  );

  "string min length pass", `Quick, (fun () ->
    let schema = Validation.string ~min_length:3 () in
    match Validation.validate schema (`String "hello") with
    | Ok _ -> ()
    | Error _ -> Alcotest.fail "Should be valid"
  );

  "string min length fail", `Quick, (fun () ->
    let schema = Validation.string ~min_length:10 () in
    match Validation.validate schema (`String "hi") with
    | Ok _ -> Alcotest.fail "Should fail"
    | Error errs ->
      Alcotest.(check int) "one error" 1 (List.length errs);
      Alcotest.(check string) "error code" "string_too_short" (List.hd errs).code
  );

  "string max length pass", `Quick, (fun () ->
    let schema = Validation.string ~max_length:10 () in
    match Validation.validate schema (`String "hello") with
    | Ok _ -> ()
    | Error _ -> Alcotest.fail "Should be valid"
  );

  "string max length fail", `Quick, (fun () ->
    let schema = Validation.string ~max_length:3 () in
    match Validation.validate schema (`String "hello") with
    | Ok _ -> Alcotest.fail "Should fail"
    | Error errs ->
      Alcotest.(check string) "error code" "string_too_long" (List.hd errs).code
  );

  "string pattern pass", `Quick, (fun () ->
    let schema = Validation.string ~pattern:"^[a-z]+$" () in
    match Validation.validate schema (`String "hello") with
    | Ok _ -> ()
    | Error _ -> Alcotest.fail "Should be valid"
  );

  "string pattern fail", `Quick, (fun () ->
    let schema = Validation.string ~pattern:"^[a-z]+$" () in
    match Validation.validate schema (`String "Hello123") with
    | Ok _ -> Alcotest.fail "Should fail"
    | Error errs ->
      Alcotest.(check string) "error code" "pattern_mismatch" (List.hd errs).code
  );

  "string type error", `Quick, (fun () ->
    let schema = Validation.string () in
    match Validation.validate schema (`Int 42) with
    | Ok _ -> Alcotest.fail "Should fail"
    | Error errs ->
      Alcotest.(check string) "error code" "type_error" (List.hd errs).code
  );
]

let format_tests = [
  "email valid", `Quick, (fun () ->
    let schema = Validation.email () in
    match Validation.validate schema (`String "test@example.com") with
    | Ok _ -> ()
    | Error _ -> Alcotest.fail "Should be valid email"
  );

  "email invalid", `Quick, (fun () ->
    let schema = Validation.email () in
    match Validation.validate schema (`String "invalid-email") with
    | Ok _ -> Alcotest.fail "Should fail"
    | Error errs ->
      Alcotest.(check string) "error code" "format_invalid" (List.hd errs).code
  );

  "uuid valid", `Quick, (fun () ->
    let schema = Validation.uuid () in
    match Validation.validate schema (`String "550e8400-e29b-41d4-a716-446655440000") with
    | Ok _ -> ()
    | Error _ -> Alcotest.fail "Should be valid UUID"
  );

  "uri valid", `Quick, (fun () ->
    let schema = Validation.uri () in
    match Validation.validate schema (`String "https://example.com/path") with
    | Ok _ -> ()
    | Error _ -> Alcotest.fail "Should be valid URI"
  );

  "date valid", `Quick, (fun () ->
    let schema = Validation.date () in
    match Validation.validate schema (`String "2024-12-25") with
    | Ok _ -> ()
    | Error _ -> Alcotest.fail "Should be valid date"
  );

  "datetime valid", `Quick, (fun () ->
    let schema = Validation.datetime () in
    match Validation.validate schema (`String "2024-12-25T10:30:00") with
    | Ok _ -> ()
    | Error _ -> Alcotest.fail "Should be valid datetime"
  );
]

let number_tests = [
  "int valid", `Quick, (fun () ->
    let schema = Validation.int () in
    match Validation.validate schema (`Int 42) with
    | Ok _ -> ()
    | Error _ -> Alcotest.fail "Should be valid"
  );

  "int minimum pass", `Quick, (fun () ->
    let schema = Validation.int ~minimum:10 () in
    match Validation.validate schema (`Int 15) with
    | Ok _ -> ()
    | Error _ -> Alcotest.fail "Should be valid"
  );

  "int minimum fail", `Quick, (fun () ->
    let schema = Validation.int ~minimum:10 () in
    match Validation.validate schema (`Int 5) with
    | Ok _ -> Alcotest.fail "Should fail"
    | Error errs ->
      Alcotest.(check string) "error code" "number_too_small" (List.hd errs).code
  );

  "int maximum pass", `Quick, (fun () ->
    let schema = Validation.int ~maximum:100 () in
    match Validation.validate schema (`Int 50) with
    | Ok _ -> ()
    | Error _ -> Alcotest.fail "Should be valid"
  );

  "int maximum fail", `Quick, (fun () ->
    let schema = Validation.int ~maximum:10 () in
    match Validation.validate schema (`Int 20) with
    | Ok _ -> Alcotest.fail "Should fail"
    | Error errs ->
      Alcotest.(check string) "error code" "number_too_large" (List.hd errs).code
  );

  "positive int", `Quick, (fun () ->
    let schema = Validation.positive_int () in
    match Validation.validate schema (`Int 1) with
    | Ok _ -> ()
    | Error _ -> Alcotest.fail "Should be valid"
  );

  "positive int zero fails", `Quick, (fun () ->
    let schema = Validation.positive_int () in
    match Validation.validate schema (`Int 0) with
    | Ok _ -> Alcotest.fail "Should fail"
    | Error _ -> ()
  );

  "non negative int", `Quick, (fun () ->
    let schema = Validation.non_negative_int () in
    match Validation.validate schema (`Int 0) with
    | Ok _ -> ()
    | Error _ -> Alcotest.fail "Should be valid"
  );

  "float valid", `Quick, (fun () ->
    let schema = Validation.float () in
    match Validation.validate schema (`Float 3.14) with
    | Ok _ -> ()
    | Error _ -> Alcotest.fail "Should be valid"
  );

  "float from int", `Quick, (fun () ->
    let schema = Validation.float () in
    match Validation.validate schema (`Int 42) with
    | Ok (`Float f) -> Alcotest.(check (float 0.001)) "converted" 42.0 f
    | Ok _ -> Alcotest.fail "Should convert to float"
    | Error _ -> Alcotest.fail "Should be valid"
  );

  "percentage", `Quick, (fun () ->
    let schema = Validation.percentage () in
    match Validation.validate schema (`Float 50.5) with
    | Ok _ -> ()
    | Error _ -> Alcotest.fail "Should be valid"
  );

  "percentage out of range", `Quick, (fun () ->
    let schema = Validation.percentage () in
    match Validation.validate schema (`Float 150.0) with
    | Ok _ -> Alcotest.fail "Should fail"
    | Error _ -> ()
  );
]

let bool_null_tests = [
  "bool true", `Quick, (fun () ->
    let schema = Validation.bool () in
    match Validation.validate schema (`Bool true) with
    | Ok _ -> ()
    | Error _ -> Alcotest.fail "Should be valid"
  );

  "bool false", `Quick, (fun () ->
    let schema = Validation.bool () in
    match Validation.validate schema (`Bool false) with
    | Ok _ -> ()
    | Error _ -> Alcotest.fail "Should be valid"
  );

  "null", `Quick, (fun () ->
    let schema = Validation.null () in
    match Validation.validate schema `Null with
    | Ok _ -> ()
    | Error _ -> Alcotest.fail "Should be valid"
  );
]

let array_tests = [
  "array valid", `Quick, (fun () ->
    let schema = Validation.array (Validation.string ()) in
    match Validation.validate schema (`List [`String "a"; `String "b"]) with
    | Ok _ -> ()
    | Error _ -> Alcotest.fail "Should be valid"
  );

  "array min items pass", `Quick, (fun () ->
    let schema = Validation.array ~min_items:2 (Validation.string ()) in
    match Validation.validate schema (`List [`String "a"; `String "b"]) with
    | Ok _ -> ()
    | Error _ -> Alcotest.fail "Should be valid"
  );

  "array min items fail", `Quick, (fun () ->
    let schema = Validation.array ~min_items:3 (Validation.string ()) in
    match Validation.validate schema (`List [`String "a"]) with
    | Ok _ -> Alcotest.fail "Should fail"
    | Error errs ->
      Alcotest.(check string) "error code" "array_too_short" (List.hd errs).code
  );

  "array max items pass", `Quick, (fun () ->
    let schema = Validation.array ~max_items:5 (Validation.string ()) in
    match Validation.validate schema (`List [`String "a"; `String "b"]) with
    | Ok _ -> ()
    | Error _ -> Alcotest.fail "Should be valid"
  );

  "array max items fail", `Quick, (fun () ->
    let schema = Validation.array ~max_items:1 (Validation.string ()) in
    match Validation.validate schema (`List [`String "a"; `String "b"]) with
    | Ok _ -> Alcotest.fail "Should fail"
    | Error errs ->
      Alcotest.(check string) "error code" "array_too_long" (List.hd errs).code
  );

  "array unique items", `Quick, (fun () ->
    let schema = Validation.array ~unique_items:true (Validation.string ()) in
    match Validation.validate schema (`List [`String "a"; `String "a"]) with
    | Ok _ -> Alcotest.fail "Should fail"
    | Error errs ->
      Alcotest.(check string) "error code" "duplicate_item" (List.hd errs).code
  );

  "array item validation", `Quick, (fun () ->
    let schema = Validation.array (Validation.int ~minimum:0 ()) in
    match Validation.validate schema (`List [`Int 1; `Int (-1)]) with
    | Ok _ -> Alcotest.fail "Should fail"
    | Error errs ->
      Alcotest.(check bool) "has path" true (List.length (List.hd errs).path > 0)
  );
]

let object_tests = [
  "object valid", `Quick, (fun () ->
    let schema = Validation.object_ [
      Validation.field "name" (Validation.string ());
      Validation.field "age" (Validation.int ());
    ] in
    match Validation.validate schema (`Assoc [
      ("name", `String "Alice");
      ("age", `Int 30);
    ]) with
    | Ok _ -> ()
    | Error _ -> Alcotest.fail "Should be valid"
  );

  "object missing required", `Quick, (fun () ->
    let schema = Validation.object_ [
      Validation.field "name" (Validation.string ());
    ] ~required:["name"; "email"] in
    match Validation.validate schema (`Assoc [("name", `String "Alice")]) with
    | Ok _ -> Alcotest.fail "Should fail"
    | Error errs ->
      Alcotest.(check string) "error code" "required_field" (List.hd errs).code
  );

  "object additional properties allowed", `Quick, (fun () ->
    let schema = Validation.object_ [
      Validation.field "name" (Validation.string ());
    ] ~additional:true in
    match Validation.validate schema (`Assoc [
      ("name", `String "Alice");
      ("extra", `String "data");
    ]) with
    | Ok _ -> ()
    | Error _ -> Alcotest.fail "Should allow additional"
  );

  "object additional properties forbidden", `Quick, (fun () ->
    let schema = Validation.object_ [
      Validation.field "name" (Validation.string ());
    ] ~additional:false in
    match Validation.validate schema (`Assoc [
      ("name", `String "Alice");
      ("extra", `String "data");
    ]) with
    | Ok _ -> Alcotest.fail "Should fail"
    | Error errs ->
      Alcotest.(check string) "error code" "unknown_field" (List.hd errs).code
  );

  "object nested validation", `Quick, (fun () ->
    let schema = Validation.object_ [
      Validation.field "user" (Validation.object_ [
        Validation.field "name" (Validation.string ~min_length:1 ());
      ] ~required:["name"]);
    ] in
    match Validation.validate schema (`Assoc [
      ("user", `Assoc [("name", `String "")])
    ]) with
    | Ok _ -> Alcotest.fail "Should fail"
    | Error errs ->
      let path = (List.hd errs).path in
      Alcotest.(check bool) "nested path" true (List.length path >= 2)
  );
]

let enum_const_tests = [
  "enum valid", `Quick, (fun () ->
    let schema = Validation.enum [`String "active"; `String "inactive"] in
    match Validation.validate schema (`String "active") with
    | Ok _ -> ()
    | Error _ -> Alcotest.fail "Should be valid"
  );

  "enum invalid", `Quick, (fun () ->
    let schema = Validation.enum [`String "active"; `String "inactive"] in
    match Validation.validate schema (`String "unknown") with
    | Ok _ -> Alcotest.fail "Should fail"
    | Error errs ->
      Alcotest.(check string) "error code" "invalid_enum" (List.hd errs).code
  );

  "const valid", `Quick, (fun () ->
    let schema = Validation.const (`String "fixed") in
    match Validation.validate schema (`String "fixed") with
    | Ok _ -> ()
    | Error _ -> Alcotest.fail "Should be valid"
  );

  "const invalid", `Quick, (fun () ->
    let schema = Validation.const (`String "fixed") in
    match Validation.validate schema (`String "different") with
    | Ok _ -> Alcotest.fail "Should fail"
    | Error errs ->
      Alcotest.(check string) "error code" "const_mismatch" (List.hd errs).code
  );
]

let composition_tests = [
  "one of valid", `Quick, (fun () ->
    let schema = Validation.one_of [
      Validation.string ();
      Validation.int ();
    ] in
    match Validation.validate schema (`String "hello") with
    | Ok _ -> ()
    | Error _ -> Alcotest.fail "Should be valid"
  );

  "one of none match", `Quick, (fun () ->
    let schema = Validation.one_of [
      Validation.string ();
      Validation.int ();
    ] in
    match Validation.validate schema (`Bool true) with
    | Ok _ -> Alcotest.fail "Should fail"
    | Error errs ->
      Alcotest.(check string) "error code" "one_of_none" (List.hd errs).code
  );

  "any of valid", `Quick, (fun () ->
    let schema = Validation.any_of [
      Validation.string ~min_length:10 ();
      Validation.string ~max_length:5 ();
    ] in
    match Validation.validate schema (`String "hi") with
    | Ok _ -> ()
    | Error _ -> Alcotest.fail "Should be valid"
  );

  "all of valid", `Quick, (fun () ->
    let schema = Validation.all_of [
      Validation.string ~min_length:3 ();
      Validation.string ~max_length:10 ();
    ] in
    match Validation.validate schema (`String "hello") with
    | Ok _ -> ()
    | Error _ -> Alcotest.fail "Should be valid"
  );

  "all of partial fail", `Quick, (fun () ->
    let schema = Validation.all_of [
      Validation.string ~min_length:3 ();
      Validation.string ~max_length:4 ();
    ] in
    match Validation.validate schema (`String "hello") with
    | Ok _ -> Alcotest.fail "Should fail"
    | Error _ -> ()
  );

  "optional (one of with null)", `Quick, (fun () ->
    let schema = Validation.optional (Validation.string ()) in
    match Validation.validate schema `Null with
    | Ok _ -> ()
    | Error _ -> Alcotest.fail "Should accept null"
  );
]

let custom_tests = [
  "custom validator pass", `Quick, (fun () ->
    let schema = Validation.custom (fun json ->
      match json with
      | `String s when String.length s mod 2 = 0 -> Ok json
      | _ -> Error "String length must be even"
    ) in
    match Validation.validate schema (`String "hi") with
    | Ok _ -> ()
    | Error _ -> Alcotest.fail "Should be valid"
  );

  "custom validator fail", `Quick, (fun () ->
    let schema = Validation.custom (fun json ->
      match json with
      | `String s when String.length s mod 2 = 0 -> Ok json
      | _ -> Error "String length must be even"
    ) in
    match Validation.validate schema (`String "odd") with
    | Ok _ -> Alcotest.fail "Should fail"
    | Error errs ->
      Alcotest.(check string) "error code" "custom_validation" (List.hd errs).code
  );
]

let error_formatting_tests = [
  "error to string", `Quick, (fun () ->
    let err : Validation.error = {
      path = ["user"; "email"];
      message = "Invalid email";
      code = "format_invalid";
    } in
    let s = Validation.error_to_string err in
    Alcotest.(check bool) "contains path" true
      (try let _ = Str.search_forward (Str.regexp_string "user.email") s 0 in true
       with Not_found -> false)
  );

  "errors to json", `Quick, (fun () ->
    let errs : Validation.error list = [
      { path = ["name"]; message = "Required"; code = "required_field" };
    ] in
    let json = Validation.errors_to_json errs in
    match json with
    | `Assoc fields ->
      Alcotest.(check bool) "has errors" true (List.mem_assoc "errors" fields)
    | _ -> Alcotest.fail "Expected JSON object"
  );

  "format path root", `Quick, (fun () ->
    let path = Validation.format_path [] in
    Alcotest.(check string) "root" "root" path
  );

  "format path nested", `Quick, (fun () ->
    let path = Validation.format_path ["user"; "address"; "city"] in
    Alcotest.(check string) "nested" "user.address.city" path
  );
]

let body_validation_tests = [
  "validate body valid", `Quick, (fun () ->
    let schema = Validation.object_ [
      Validation.field "name" (Validation.string ());
    ] in
    match Validation.validate_body schema "{\"name\":\"Alice\"}" with
    | Ok _ -> ()
    | Error _ -> Alcotest.fail "Should be valid"
  );

  "validate body invalid json", `Quick, (fun () ->
    let schema = Validation.string () in
    match Validation.validate_body schema "not json" with
    | Ok _ -> Alcotest.fail "Should fail"
    | Error errs ->
      Alcotest.(check string) "error code" "parse_error" (List.hd errs).code
  );

  "validate query params", `Quick, (fun () ->
    let params = [
      ("limit", Validation.int ~minimum:1 ~maximum:100 ());
    ] in
    let query_getter name =
      if name = "limit" then Some "50" else None
    in
    match Validation.validate_query_params params query_getter with
    | Ok () -> ()
    | Error _ -> Alcotest.fail "Should be valid"
  );
]

let () =
  Alcotest.run "Validation" [
    ("String", string_tests);
    ("Format", format_tests);
    ("Number", number_tests);
    ("Bool/Null", bool_null_tests);
    ("Array", array_tests);
    ("Object", object_tests);
    ("Enum/Const", enum_const_tests);
    ("Composition", composition_tests);
    ("Custom", custom_tests);
    ("Error Formatting", error_formatting_tests);
    ("Body Validation", body_validation_tests);
  ]
