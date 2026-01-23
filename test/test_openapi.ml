(** OpenAPI Module Tests (Phase 14) *)

open Kirin

let spec_tests = [
  "create spec", `Quick, (fun () ->
    let spec = Openapi.create
      ~title:"Test API"
      ~version:"1.0.0"
      () in
    Alcotest.(check string) "title" "Test API" spec.info.title;
    Alcotest.(check string) "openapi" "3.0.3" spec.openapi
  );

  "create spec with description", `Quick, (fun () ->
    let spec = Openapi.create
      ~title:"Test API"
      ~version:"1.0.0"
      ~description:"A test API"
      () in
    Alcotest.(check (option string)) "description" (Some "A test API") spec.info.description
  );

  "create spec with servers", `Quick, (fun () ->
    let servers = [
      { Openapi.url = "https://api.example.com";
        server_description = Some "Production";
        variables = None }
    ] in
    let spec = Openapi.create
      ~title:"Test API"
      ~version:"1.0.0"
      ~servers
      () in
    Alcotest.(check int) "servers count" 1 (List.length spec.servers)
  );
]

let schema_tests = [
  "string schema", `Quick, (fun () ->
    let schema = Openapi.string () in
    Alcotest.(check bool) "is string" true (schema.schema_type = Some Openapi.String)
  );

  "string with min length", `Quick, (fun () ->
    let schema = Openapi.string ~min_length:5 () in
    Alcotest.(check (option int)) "min_length" (Some 5) schema.min_length
  );

  "string with max length", `Quick, (fun () ->
    let schema = Openapi.string ~max_length:100 () in
    Alcotest.(check (option int)) "max_length" (Some 100) schema.max_length
  );

  "string with format", `Quick, (fun () ->
    let schema = Openapi.string ~format:"email" () in
    Alcotest.(check (option string)) "format" (Some "email") schema.format
  );

  "integer schema", `Quick, (fun () ->
    let schema = Openapi.integer () in
    Alcotest.(check bool) "is integer" true (schema.schema_type = Some Openapi.Integer)
  );

  "integer with range", `Quick, (fun () ->
    let schema = Openapi.integer ~minimum:0. ~maximum:100. () in
    Alcotest.(check (option (float 0.01))) "minimum" (Some 0.) schema.minimum;
    Alcotest.(check (option (float 0.01))) "maximum" (Some 100.) schema.maximum
  );

  "number schema", `Quick, (fun () ->
    let schema = Openapi.number () in
    Alcotest.(check bool) "is number" true (schema.schema_type = Some Openapi.Number)
  );

  "boolean schema", `Quick, (fun () ->
    let schema = Openapi.boolean () in
    Alcotest.(check bool) "is boolean" true (schema.schema_type = Some Openapi.Boolean)
  );

  "array schema", `Quick, (fun () ->
    let items = Openapi.string () in
    let schema = Openapi.array items in
    Alcotest.(check bool) "is array" true (schema.schema_type = Some Openapi.Array);
    Alcotest.(check bool) "has items" true (schema.items <> None)
  );

  "object schema", `Quick, (fun () ->
    let schema = Openapi.object_
      [
        ("name", Openapi.string ());
        ("age", Openapi.integer ());
      ] in
    Alcotest.(check bool) "is object" true (schema.schema_type = Some Openapi.Object);
    Alcotest.(check bool) "has properties" true (schema.properties <> None)
  );

  "object with required", `Quick, (fun () ->
    let schema = Openapi.object_
      ~required:["name"]
      [("name", Openapi.string ())] in
    Alcotest.(check (option (list string))) "required" (Some ["name"]) schema.required
  );

  "ref schema", `Quick, (fun () ->
    let schema = Openapi.ref_ "#/components/schemas/User" in
    Alcotest.(check (option string)) "ref" (Some "#/components/schemas/User") schema.ref_path
  );

  "nullable schema", `Quick, (fun () ->
    let schema = Openapi.nullable (Openapi.string ()) in
    Alcotest.(check bool) "nullable" true schema.nullable
  );
]

let parameter_tests = [
  "query parameter", `Quick, (fun () ->
    let param = Openapi.query_param
      ~name:"limit"
      ~schema:(Openapi.integer ())
      () in
    Alcotest.(check string) "name" "limit" param.name;
    Alcotest.(check string) "in" "query" (Openapi.param_in_to_string param.param_in);
    Alcotest.(check bool) "required" false param.required
  );

  "path parameter", `Quick, (fun () ->
    let param = Openapi.path_param
      ~name:"id"
      ~schema:(Openapi.string ())
      () in
    Alcotest.(check string) "name" "id" param.name;
    Alcotest.(check string) "in" "path" (Openapi.param_in_to_string param.param_in);
    Alcotest.(check bool) "required" true param.required
  );

  "header parameter", `Quick, (fun () ->
    let param = Openapi.header_param
      ~name:"X-Request-ID"
      ~schema:(Openapi.string ())
      () in
    Alcotest.(check string) "name" "X-Request-ID" param.name;
    Alcotest.(check string) "in" "header" (Openapi.param_in_to_string param.param_in)
  );
]

let response_tests = [
  "json response", `Quick, (fun () ->
    let resp = Openapi.response
      ~description:"Success"
      ~content:(Openapi.object_ [])
      () in
    Alcotest.(check string) "description" "Success" resp.response_description;
    Alcotest.(check bool) "has content" true (resp.response_content <> None)
  );

  "empty response", `Quick, (fun () ->
    let resp = Openapi.empty_response "No Content" in
    Alcotest.(check string) "description" "No Content" resp.response_description;
    Alcotest.(check bool) "no content" true (resp.response_content = None)
  );
]

let operation_tests = [
  "get operation", `Quick, (fun () ->
    let op = Openapi.operation
      ~summary:"Get users"
      ~responses:[(200, Openapi.empty_response "OK")]
      () in
    Alcotest.(check (option string)) "summary" (Some "Get users") op.summary
  );

  "post operation with body", `Quick, (fun () ->
    let op = Openapi.operation
      ~summary:"Create user"
      ~request_body:(Openapi.json_body (Openapi.object_ []))
      ~responses:[(201, Openapi.empty_response "Created")]
      () in
    Alcotest.(check bool) "has request body" true (op.request_body <> None)
  );

  "operation with tags", `Quick, (fun () ->
    let op = Openapi.operation
      ~summary:"Get users"
      ~tags:["users"; "admin"]
      ~responses:[(200, Openapi.empty_response "OK")]
      () in
    Alcotest.(check int) "tags count" 2 (List.length op.tags)
  );

  "operation with parameters", `Quick, (fun () ->
    let op = Openapi.operation
      ~summary:"Get user"
      ~parameters:[
        Openapi.path_param ~name:"id" ~schema:(Openapi.string ()) ();
      ]
      ~responses:[(200, Openapi.empty_response "OK")]
      () in
    Alcotest.(check int) "parameters count" 1 (List.length op.parameters)
  );
]

let path_tests = [
  "add path", `Quick, (fun () ->
    let path_item = {
      Openapi.path_summary = None;
      path_description = None;
      get = Some (Openapi.operation
        ~summary:"Get users"
        ~responses:[(200, Openapi.empty_response "OK")]
        ());
      post = None; put = None; delete = None; patch = None;
      options = None; head = None; path_parameters = []
    } in
    let spec = Openapi.create ~title:"API" ~version:"1.0.0" () in
    let spec = Openapi.add_path spec ~path:"/users" path_item in
    Alcotest.(check int) "paths count" 1 (List.length spec.paths)
  );

  "add multiple paths", `Quick, (fun () ->
    let path_item = {
      Openapi.path_summary = None;
      path_description = None;
      get = Some (Openapi.operation ~responses:[] ());
      post = None; put = None; delete = None; patch = None;
      options = None; head = None; path_parameters = []
    } in
    let spec = Openapi.create ~title:"API" ~version:"1.0.0" () in
    let spec = Openapi.add_path spec ~path:"/users" path_item in
    let spec = Openapi.add_path spec ~path:"/posts" path_item in
    Alcotest.(check int) "paths count" 2 (List.length spec.paths)
  );
]

let component_tests = [
  "add schema", `Quick, (fun () ->
    let spec = Openapi.create ~title:"API" ~version:"1.0.0" () in
    let spec = Openapi.add_schema spec ~name:"User" (Openapi.object_ []) in
    Alcotest.(check bool) "has components" true (spec.components <> None);
    match spec.components with
    | Some c -> Alcotest.(check int) "schemas count" 1 (List.length c.schemas)
    | None -> Alcotest.fail "Expected components"
  );
]

let json_tests = [
  "to json", `Quick, (fun () ->
    let spec = Openapi.create ~title:"Test API" ~version:"1.0.0" () in
    let json = Openapi.to_json spec in
    match json with
    | `Assoc fields ->
      Alcotest.(check bool) "has openapi" true (List.mem_assoc "openapi" fields);
      Alcotest.(check bool) "has info" true (List.mem_assoc "info" fields)
    | _ -> Alcotest.fail "Expected JSON object"
  );

  "spec json helper", `Quick, (fun () ->
    let spec = Openapi.create ~title:"Test API" ~version:"1.0.0" () in
    let json = Openapi.spec_json spec in
    match json with
    | `Assoc _ -> ()
    | _ -> Alcotest.fail "Expected JSON object"
  );
]

let ui_tests = [
  "swagger ui", `Quick, (fun () ->
    let spec = Openapi.create ~title:"Test API" ~version:"1.0.0" () in
    let html = Openapi.swagger_ui spec in
    Alcotest.(check bool) "contains html" true (String.length html > 0);
    Alcotest.(check bool) "contains swagger" true
      (try let _ = Str.search_forward (Str.regexp_string "swagger-ui") html 0 in true
       with Not_found -> false)
  );

  "redoc", `Quick, (fun () ->
    let spec = Openapi.create ~title:"Test API" ~version:"1.0.0" () in
    let html = Openapi.redoc spec in
    Alcotest.(check bool) "contains html" true (String.length html > 0);
    Alcotest.(check bool) "contains redoc" true
      (try let _ = Str.search_forward (Str.regexp_string "redoc") html 0 in true
       with Not_found -> false)
  );
]

let () =
  Alcotest.run "Openapi" [
    ("Spec", spec_tests);
    ("Schema", schema_tests);
    ("Parameter", parameter_tests);
    ("Response", response_tests);
    ("Operation", operation_tests);
    ("Path", path_tests);
    ("Component", component_tests);
    ("JSON", json_tests);
    ("UI", ui_tests);
  ]
