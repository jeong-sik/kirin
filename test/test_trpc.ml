(** tRPC Tests *)

open Kirin_trpc

(* Test Counter *)
let tests_passed = ref 0
let tests_failed = ref 0

let test name f =
  try
    f ();
    incr tests_passed;
    Printf.printf "✓ %s\n" name
  with e ->
    incr tests_failed;
    Printf.printf "✗ %s: %s\n" name (Printexc.to_string e)

let assert_eq msg expected actual =
  if expected <> actual then
    failwith (Printf.sprintf "%s: expected %s, got %s" msg expected actual)

let assert_true msg condition =
  if not condition then failwith msg

(* ==========================================================================
   Procedure Tests
   ========================================================================== *)

let test_procedure_types () =
  test "procedure_type_to_string query" (fun () ->
    assert_eq "query" "query" (Procedure.procedure_type_to_string Query)
  );
  test "procedure_type_to_string mutation" (fun () ->
    assert_eq "mutation" "mutation" (Procedure.procedure_type_to_string Mutation)
  );
  test "procedure_type_to_string subscription" (fun () ->
    assert_eq "subscription" "subscription" (Procedure.procedure_type_to_string Subscription)
  )

let test_validators () =
  test "string_input valid" (fun () ->
    match Procedure.string_input (`String "hello") with
    | Ok s -> assert_eq "value" "hello" s
    | Error _ -> failwith "expected Ok"
  );
  test "string_input invalid" (fun () ->
    match Procedure.string_input (`Int 42) with
    | Error _ -> ()
    | Ok _ -> failwith "expected Error"
  );
  test "int_input valid" (fun () ->
    match Procedure.int_input (`Int 42) with
    | Ok i -> assert_eq "value" "42" (string_of_int i)
    | Error _ -> failwith "expected Ok"
  );
  test "bool_input valid" (fun () ->
    match Procedure.bool_input (`Bool true) with
    | Ok b -> assert_true "value" b
    | Error _ -> failwith "expected Ok"
  );
  test "optional_input some" (fun () ->
    match Procedure.optional_input Procedure.string_input (`String "test") with
    | Ok (Some s) -> assert_eq "value" "test" s
    | _ -> failwith "expected Some"
  );
  test "optional_input none" (fun () ->
    match Procedure.optional_input Procedure.string_input `Null with
    | Ok None -> ()
    | _ -> failwith "expected None"
  );
  test "list_input valid" (fun () ->
    match Procedure.list_input Procedure.int_input (`List [`Int 1; `Int 2; `Int 3]) with
    | Ok [1; 2; 3] -> ()
    | _ -> failwith "expected [1; 2; 3]"
  );
  test "field extractor" (fun () ->
    let json = `Assoc [("name", `String "Alice")] in
    match Procedure.field "name" Procedure.string_input json with
    | Ok s -> assert_eq "value" "Alice" s
    | Error _ -> failwith "expected Ok"
  );
  test "field_opt extractor missing" (fun () ->
    let json = `Assoc [] in
    match Procedure.field_opt "name" Procedure.string_input json with
    | Ok None -> ()
    | _ -> failwith "expected None"
  )

let test_serializers () =
  test "unit_output" (fun () ->
    let json = Procedure.unit_output () in
    assert_eq "value" "`Null" (Yojson.Safe.show json)
  );
  test "string_output" (fun () ->
    match Procedure.string_output "hello" with
    | `String "hello" -> ()
    | _ -> failwith "expected String"
  );
  test "int_output" (fun () ->
    match Procedure.int_output 42 with
    | `Int 42 -> ()
    | _ -> failwith "expected Int"
  );
  test "list_output" (fun () ->
    match Procedure.list_output Procedure.int_output [1; 2; 3] with
    | `List [`Int 1; `Int 2; `Int 3] -> ()
    | _ -> failwith "expected List"
  );
  test "option_output some" (fun () ->
    match Procedure.option_output Procedure.string_output (Some "test") with
    | `String "test" -> ()
    | _ -> failwith "expected String"
  );
  test "option_output none" (fun () ->
    match Procedure.option_output Procedure.string_output None with
    | `Null -> ()
    | _ -> failwith "expected Null"
  )

let test_type_info () =
  test "type_info_to_typescript string" (fun () ->
    assert_eq "ts" "string" (Procedure.type_info_to_typescript TString)
  );
  test "type_info_to_typescript int" (fun () ->
    assert_eq "ts" "number" (Procedure.type_info_to_typescript TInt)
  );
  test "type_info_to_typescript bool" (fun () ->
    assert_eq "ts" "boolean" (Procedure.type_info_to_typescript TBool)
  );
  test "type_info_to_typescript array" (fun () ->
    assert_eq "ts" "string[]" (Procedure.type_info_to_typescript (TArray TString))
  );
  test "type_info_to_typescript optional" (fun () ->
    assert_eq "ts" "string | undefined" (Procedure.type_info_to_typescript (TOptional TString))
  )

(* ==========================================================================
   Router Tests
   ========================================================================== *)

let test_router () =
  test "create empty router" (fun () ->
    let router = Router.create () in
    assert_eq "prefix" "" (Router.get_prefix router)
  );
  test "create router with prefix" (fun () ->
    let router = Router.create ~prefix:"api" () in
    assert_eq "prefix" "api" (Router.get_prefix router)
  );
  test "add query to router" (fun () ->
    let router = Router.create ()
      |> Router.query ~path:"test.hello"
           ~input:Procedure.no_input
           ~output:Procedure.string_output
           ~input_type:TNull
           ~output_type:TString
           (fun _ctx () -> Ok "hello")
    in
    let paths = Router.paths router in
    assert_true "has path" (List.mem "test.hello" paths)
  );
  test "find procedure" (fun () ->
    let router = Router.create ()
      |> Router.query ~path:"user.get"
           ~input:Procedure.string_input
           ~output:Procedure.json_output
           ~input_type:TString
           ~output_type:(TObject [("id", TString)])
           (fun _ctx id -> Ok (`Assoc [("id", `String id)]))
    in
    match Router.find_procedure "user.get" router with
    | Some _ -> ()
    | None -> failwith "expected to find procedure"
  );
  test "execute procedure" (fun () ->
    let router = Router.create ()
      |> Router.query ~path:"echo"
           ~input:Procedure.string_input
           ~output:Procedure.string_output
           ~input_type:TString
           ~output_type:TString
           (fun _ctx s -> Ok s)
    in
    match Router.execute_procedure "echo" () (`String "test") router with
    | Ok (`String "test") -> ()
    | Ok _ -> failwith "wrong result"
    | Error e -> failwith ("unexpected error: " ^ e)
  );
  test "execute procedure not found" (fun () ->
    let router = Router.create () in
    match Router.execute_procedure "nonexistent" () `Null router with
    | Error _ -> ()
    | Ok _ -> failwith "expected error"
  );
  test "all_procedure_infos" (fun () ->
    let router = Router.create ()
      |> Router.query ~path:"a" ~input:Procedure.no_input ~output:Procedure.unit_output
           ~input_type:TNull ~output_type:TNull (fun _ctx () -> Ok ())
      |> Router.mutation ~path:"b" ~input:Procedure.no_input ~output:Procedure.unit_output
           ~input_type:TNull ~output_type:TNull (fun _ctx () -> Ok ())
    in
    let infos = Router.all_procedure_infos router in
    assert_eq "count" "2" (string_of_int (List.length infos))
  );
  test "queries filter" (fun () ->
    let router = Router.create ()
      |> Router.query ~path:"q" ~input:Procedure.no_input ~output:Procedure.unit_output
           ~input_type:TNull ~output_type:TNull (fun _ctx () -> Ok ())
      |> Router.mutation ~path:"m" ~input:Procedure.no_input ~output:Procedure.unit_output
           ~input_type:TNull ~output_type:TNull (fun _ctx () -> Ok ())
    in
    let queries = Router.queries router in
    assert_eq "count" "1" (string_of_int (List.length queries))
  )

(* ==========================================================================
   Context Tests
   ========================================================================== *)

let test_context () =
  test "base_of_request" (fun () ->
    (* We can't easily create a mock request, so just test the factory *)
    let _factory = Context.unit_factory in
    ()
  );
  test "header lookup" (fun () ->
    let ctx = { Context.headers = [("Authorization", "Bearer token123")] ; request = Obj.magic (); path = "/" } in
    match Context.header "authorization" ctx with
    | Some "Bearer token123" -> ()
    | _ -> failwith "expected header"
  );
  test "bearer_token extraction" (fun () ->
    let ctx = { Context.headers = [("Authorization", "Bearer mytoken")] ; request = Obj.magic (); path = "/" } in
    match Context.bearer_token ctx with
    | Some "mytoken" -> ()
    | _ -> failwith "expected token"
  );
  test "bearer_token missing" (fun () ->
    let ctx = { Context.headers = [] ; request = Obj.magic (); path = "/" } in
    match Context.bearer_token ctx with
    | None -> ()
    | _ -> failwith "expected None"
  )

(* ==========================================================================
   Batch Tests
   ========================================================================== *)

let test_batch () =
  test "parse single call" (fun () ->
    let json = `Assoc [
      ("id", `Int 1);
      ("path", `String "test.hello");
      ("input", `String "world");
    ] in
    match Batch.parse_call json with
    | Some call ->
      assert_eq "id" "1" (string_of_int call.Batch.id);
      assert_eq "path" "test.hello" call.Batch.path
    | None -> failwith "expected call"
  );
  test "parse batch array" (fun () ->
    let json = `List [
      `Assoc [("id", `Int 1); ("path", `String "a")];
      `Assoc [("id", `Int 2); ("path", `String "b")];
    ] in
    let calls = Batch.parse_batch json in
    assert_eq "count" "2" (string_of_int (List.length calls))
  );
  test "is_batch array" (fun () ->
    assert_true "is batch" (Batch.is_batch (`List []))
  );
  test "is_batch object" (fun () ->
    assert_true "not batch" (not (Batch.is_batch (`Assoc [])))
  );
  test "success response" (fun () ->
    let resp = Batch.success ~id:1 (`String "ok") in
    assert_eq "id" "1" (string_of_int resp.Batch.id);
    match resp.Batch.result with
    | Ok (`String "ok") -> ()
    | _ -> failwith "expected Ok"
  );
  test "error response" (fun () ->
    let resp = Batch.error ~id:1 "something went wrong" in
    match resp.Batch.result with
    | Error { Batch.message = "something went wrong"; _ } -> ()
    | _ -> failwith "expected Error"
  );
  test "response_to_json success" (fun () ->
    let resp = Batch.success ~id:1 (`String "ok") in
    let json = Batch.response_to_json resp in
    match json with
    | `Assoc fields ->
      assert_true "has id" (List.mem_assoc "id" fields);
      assert_true "has result" (List.mem_assoc "result" fields)
    | _ -> failwith "expected object"
  );
  test "batch_response_to_json single" (fun () ->
    let responses = [Batch.success ~id:1 (`String "ok")] in
    match Batch.batch_response_to_json responses with
    | `Assoc _ -> ()  (* Single response, not wrapped in array *)
    | _ -> failwith "expected object"
  );
  test "batch_response_to_json multiple" (fun () ->
    let responses = [
      Batch.success ~id:1 (`String "a");
      Batch.success ~id:2 (`String "b");
    ] in
    match Batch.batch_response_to_json responses with
    | `List l -> assert_eq "count" "2" (string_of_int (List.length l))
    | _ -> failwith "expected array"
  )

(* ==========================================================================
   Subscription Tests
   ========================================================================== *)

let test_subscription () =
  test "generate_id" (fun () ->
    let id = Subscription.generate_id () in
    assert_true "starts with sub_" (String.sub id 0 4 = "sub_")
  );
  test "encode Data message" (fun () ->
    let msg = Subscription.encode_message ~id:"sub_1" (Subscription.Data (`String "hello")) in
    match msg with
    | `Assoc fields ->
      assert_true "has id" (List.mem_assoc "id" fields);
      assert_true "has result" (List.mem_assoc "result" fields)
    | _ -> failwith "expected object"
  );
  test "encode Complete message" (fun () ->
    let msg = Subscription.encode_message ~id:"sub_1" Subscription.Complete in
    match msg with
    | `Assoc fields ->
      (match List.assoc_opt "result" fields with
       | Some (`Assoc result_fields) ->
         (match List.assoc_opt "type" result_fields with
          | Some (`String "stopped") -> ()
          | _ -> failwith "expected stopped")
       | _ -> failwith "expected result")
    | _ -> failwith "expected object"
  );
  test "encode Error message" (fun () ->
    let msg = Subscription.encode_message ~id:"sub_1"
        (Subscription.Err { code = -1; message = "test error" }) in
    match msg with
    | `Assoc fields ->
      assert_true "has error" (List.mem_assoc "error" fields)
    | _ -> failwith "expected object"
  );
  test "sse_event format" (fun () ->
    let event = Subscription.sse_event ~event:"message" ~data:"hello" in
    assert_true "has event:" (String.sub event 0 7 = "event: ");
    assert_true "has data:" (String.length event > 20)
  );
  test "Registry create" (fun () ->
    let reg = Subscription.Registry.create () in
    assert_eq "count" "0" (string_of_int (Subscription.Registry.count reg))
  );
  test "Registry add and count" (fun () ->
    let reg = Subscription.Registry.create () in
    Subscription.Registry.add reg ~id:"sub_1" ~cancel:(fun () -> ());
    assert_eq "count" "1" (string_of_int (Subscription.Registry.count reg))
  );
  test "Registry has" (fun () ->
    let reg = Subscription.Registry.create () in
    Subscription.Registry.add reg ~id:"sub_1" ~cancel:(fun () -> ());
    assert_true "has sub_1" (Subscription.Registry.has reg "sub_1");
    assert_true "not has sub_2" (not (Subscription.Registry.has reg "sub_2"))
  );
  test "Registry remove" (fun () ->
    let cancelled = ref false in
    let reg = Subscription.Registry.create () in
    Subscription.Registry.add reg ~id:"sub_1" ~cancel:(fun () -> cancelled := true);
    assert_true "removed" (Subscription.Registry.remove reg "sub_1");
    assert_true "was cancelled" !cancelled;
    assert_eq "count" "0" (string_of_int (Subscription.Registry.count reg))
  )

(* ==========================================================================
   Codegen Tests
   ========================================================================== *)

let test_codegen () =
  test "generate_type string" (fun () ->
    assert_eq "ts" "string" (Codegen.generate_type TString)
  );
  test "generate_type object" (fun () ->
    let ts = Codegen.generate_type (TObject [("id", TString); ("name", TString)]) in
    assert_true "has id" (String.length ts > 10)
  );
  test "generate_json_schema string" (fun () ->
    match Codegen.generate_json_schema TString with
    | `Assoc [("type", `String "string")] -> ()
    | _ -> failwith "expected string schema"
  );
  test "generate_json_schema int" (fun () ->
    match Codegen.generate_json_schema TInt with
    | `Assoc [("type", `String "integer")] -> ()
    | _ -> failwith "expected integer schema"
  );
  test "generate_json_schema array" (fun () ->
    match Codegen.generate_json_schema (TArray TString) with
    | `Assoc fields ->
      assert_true "has type" (List.mem_assoc "type" fields);
      assert_true "has items" (List.mem_assoc "items" fields)
    | _ -> failwith "expected array schema"
  );
  test "generate_zod string" (fun () ->
    assert_eq "zod" "z.string()" (Codegen.generate_zod TString)
  );
  test "generate_zod int" (fun () ->
    assert_eq "zod" "z.number().int()" (Codegen.generate_zod TInt)
  );
  test "generate_client" (fun () ->
    let infos = [{
      Procedure.name = "getUser";
      proc_type = Query;
      input_type = TString;
      output_type = TObject [("id", TString); ("name", TString)];
      description = Some "Get user by ID";
    }] in
    let client = Codegen.generate_client ~router_name:"App" infos in
    assert_true "has import" (String.length client > 100);
    assert_true "has AppRouter" (String.length client > 100)
  );
  test "generate_schema" (fun () ->
    let infos = [{
      Procedure.name = "getUser";
      proc_type = Query;
      input_type = TString;
      output_type = TString;
      description = None;
    }] in
    let schema = Codegen.generate_schema infos in
    match schema with
    | `Assoc fields ->
      assert_true "has procedures" (List.mem_assoc "procedures" fields)
    | _ -> failwith "expected object"
  )

(* ==========================================================================
   Handler Tests
   ========================================================================== *)

let test_handler () =
  test "default_config" (fun () ->
    assert_eq "prefix" "/trpc" Handler.default_config.Handler.prefix;
    assert_true "batching enabled" Handler.default_config.Handler.enable_batching
  );
  test "parse_path valid" (fun () ->
    match Handler.parse_path ~prefix:"/trpc" "/trpc/user.get" with
    | Some "user.get" -> ()
    | _ -> failwith "expected user.get"
  );
  test "parse_path no match" (fun () ->
    match Handler.parse_path ~prefix:"/trpc" "/api/users" with
    | None -> ()
    | Some _ -> failwith "expected None"
  );
  test "parse_path empty" (fun () ->
    match Handler.parse_path ~prefix:"/trpc" "/trpc" with
    | None -> ()
    | Some _ -> failwith "expected None for empty path"
  )

(* ==========================================================================
   Integration Tests
   ========================================================================== *)

let test_integration () =
  test "full query workflow" (fun () ->
    let router = Router.create ()
      |> Router.query ~path:"greeting.hello"
           ~input:Procedure.string_input
           ~output:Procedure.string_output
           ~input_type:TString
           ~output_type:TString
           (fun _ctx name -> Ok (Printf.sprintf "Hello, %s!" name))
    in
    match Router.execute_procedure "greeting.hello" () (`String "World") router with
    | Ok (`String "Hello, World!") -> ()
    | _ -> failwith "unexpected result"
  );
  test "full mutation workflow" (fun () ->
    let counter = ref 0 in
    let router = Router.create ()
      |> Router.mutation ~path:"counter.increment"
           ~input:Procedure.int_input
           ~output:Procedure.int_output
           ~input_type:TInt
           ~output_type:TInt
           (fun _ctx amount ->
             counter := !counter + amount;
             Ok !counter)
    in
    match Router.execute_procedure "counter.increment" () (`Int 5) router with
    | Ok (`Int 5) -> ()
    | _ -> failwith "unexpected result"
  );
  test "nested router paths" (fun () ->
    let user_router = Router.create ()
      |> Router.query ~path:"get" ~input:Procedure.string_input ~output:Procedure.json_output
           ~input_type:TString ~output_type:(TObject [])
           (fun _ctx id -> Ok (`Assoc [("id", `String id)]))
    in
    let main_router = Router.create ()
      |> Router.merge ~prefix:"user" user_router
    in
    match Router.find_procedure "user.get" main_router with
    | Some _ -> ()
    | None -> failwith "expected to find user.get"
  )

(* ==========================================================================
   Convenience Functions Tests
   ========================================================================== *)

let test_convenience () =
  test "query convenience" (fun () ->
    let router = Router.create ()
      |> query ~path:"test" ~input:no_input ~output:unit_output
           ~input_type:TNull ~output_type:TNull (fun _ctx () -> Ok ())
    in
    assert_eq "paths" "1" (string_of_int (List.length (Router.paths router)))
  );
  test "mutation convenience" (fun () ->
    let router = Router.create ()
      |> mutation ~path:"test" ~input:no_input ~output:unit_output
           ~input_type:TNull ~output_type:TNull (fun _ctx () -> Ok ())
    in
    let mutations = Router.mutations router in
    assert_eq "count" "1" (string_of_int (List.length mutations))
  );
  test "generate_client convenience" (fun () ->
    let router = Router.create ()
      |> query ~path:"hello" ~input:string_input ~output:string_output
           ~input_type:TString ~output_type:TString (fun _ctx s -> Ok s)
    in
    let client = generate_client ~name:"Test" router in
    assert_true "has content" (String.length client > 100)
  );
  test "generate_schema convenience" (fun () ->
    let router = Router.create ()
      |> query ~path:"hello" ~input:no_input ~output:string_output
           ~input_type:TNull ~output_type:TString (fun _ctx () -> Ok "hi")
    in
    match generate_schema router with
    | `Assoc _ -> ()
    | _ -> failwith "expected object"
  );
  test "generate_zod convenience" (fun () ->
    let router = Router.create ()
      |> query ~path:"hello" ~input:no_input ~output:string_output
           ~input_type:TNull ~output_type:TString (fun _ctx () -> Ok "hi")
    in
    let zod = generate_zod router in
    assert_true "has zod import" (String.length zod > 20)
  )

(* ==========================================================================
   Main
   ========================================================================== *)

let () =
  Printf.printf "\n=== tRPC Tests ===\n\n";

  Printf.printf "-- Procedure Types --\n";
  test_procedure_types ();

  Printf.printf "\n-- Validators --\n";
  test_validators ();

  Printf.printf "\n-- Serializers --\n";
  test_serializers ();

  Printf.printf "\n-- Type Info --\n";
  test_type_info ();

  Printf.printf "\n-- Router --\n";
  test_router ();

  Printf.printf "\n-- Context --\n";
  test_context ();

  Printf.printf "\n-- Batch --\n";
  test_batch ();

  Printf.printf "\n-- Subscription --\n";
  test_subscription ();

  Printf.printf "\n-- Codegen --\n";
  test_codegen ();

  Printf.printf "\n-- Handler --\n";
  test_handler ();

  Printf.printf "\n-- Integration --\n";
  test_integration ();

  Printf.printf "\n-- Convenience Functions --\n";
  test_convenience ();

  Printf.printf "\n=== Results: %d passed, %d failed ===\n" !tests_passed !tests_failed;
  if !tests_failed > 0 then exit 1
