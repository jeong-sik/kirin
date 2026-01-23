(** Kirin GraphQL Integration

    This module provides GraphQL API support for Kirin applications.
    It uses the ocaml-graphql-server library with synchronous execution,
    perfect for Eio's direct-style async.

    {b Features:}
    - Schema-first development
    - HTTP endpoint (POST /graphql, GET for introspection)
    - Built-in error handling
    - Context passing for auth/request data

    {b Quick Example:}
    {[
      open Kirin.Graphql

      let user_type = obj "User"
        ~fields:[
          field "id" ~typ:(non_null string)
            ~args:Arg.[] ~resolve:(fun _info user -> user.id);
          field "name" ~typ:(non_null string)
            ~args:Arg.[] ~resolve:(fun _info user -> user.name);
        ]

      let schema = schema [
        field "user" ~typ:user_type
          ~args:Arg.[arg "id" ~typ:(non_null Arg.string)]
          ~resolve:(fun _info () id -> find_user id)
      ]

      let routes = Kirin.router [
        Kirin.post "/graphql" (Kirin.Graphql.handler schema);
        Kirin.get "/graphql" (Kirin.Graphql.playground_handler);
      ]
    ]}
*)

(** {1 Re-exports from graphql library} *)

(** Schema module for building GraphQL schemas *)
module Schema = Graphql.Schema

(** Argument definitions *)
module Arg = Schema.Arg

(** {1 Kirin-style Schema Builders} *)

(** Create a non-null type *)
let non_null = Schema.non_null

(** Create a list type *)
let list = Schema.list

(** String scalar type *)
let string = Schema.string

(** Int scalar type *)
let int = Schema.int

(** Float scalar type *)
let float = Schema.float

(** Boolean scalar type *)
let bool = Schema.bool

(** ID/GUID scalar type (serialized as string) *)
let id = Schema.guid

(** Create an object type

    {[
      let user = obj "User" ~fields:[
        field "id" ~typ:(non_null string) ~args:Arg.[] ~resolve:(fun _ u -> u.id);
        field "name" ~typ:(non_null string) ~args:Arg.[] ~resolve:(fun _ u -> u.name);
      ]
    ]}
*)
let obj = Schema.obj

(** Create a field in an object type *)
let field = Schema.field

(** Create an async field (for IO operations) *)
let io_field = Schema.io_field

(** Create an interface type *)
let interface = Schema.interface

(** Create a union type *)
let union = Schema.union

(** Create an enum type

    {[
      let status_enum = enum "Status" ~values:[
        enum_value "ACTIVE" ~value:`Active;
        enum_value "INACTIVE" ~value:`Inactive;
      ]
    ]}
*)
let enum = Schema.enum

(** Create an enum value *)
let enum_value = Schema.enum_value

(** Create a scalar type *)
let scalar = Schema.scalar

(** {1 Schema Creation} *)

(** Create a GraphQL schema

    {[
      let schema = schema
        ~mutations:[
          field "createUser" ~typ:user_type
            ~args:Arg.[arg "name" ~typ:(non_null Arg.string)]
            ~resolve:(fun _ () name -> create_user name);
        ]
        [
          field "users" ~typ:(non_null (list (non_null user_type)))
            ~args:Arg.[] ~resolve:(fun _ () -> get_all_users ());
        ]
    ]}
*)
let schema ?mutations ?subscriptions query =
  Schema.schema
    ?mutations
    ?subscriptions
    query

(** Fix combinator for recursive types *)
let fix = Schema.fix

(** {1 HTTP Handlers} *)

(** GraphQL request body *)
type graphql_request = {
  query : string;
  operation_name : string option;
  variables : Yojson.Safe.t option;
}

(** Parse GraphQL request from JSON body *)
let parse_request body : graphql_request option =
  try
    let json = Yojson.Safe.from_string body in
    let query =
      match json with
      | `Assoc assoc ->
        (match List.assoc_opt "query" assoc with
         | Some (`String q) -> Some q
         | _ -> None)
      | _ -> None
    in
    let operation_name =
      match json with
      | `Assoc assoc ->
        (match List.assoc_opt "operationName" assoc with
         | Some (`String name) -> Some name
         | Some `Null -> None
         | _ -> None)
      | _ -> None
    in
    let variables =
      match json with
      | `Assoc assoc ->
        (match List.assoc_opt "variables" assoc with
         | Some `Null -> None
         | Some vars -> Some vars
         | None -> None)
      | _ -> None
    in
    match query with
    | Some q -> Some { query = q; operation_name; variables }
    | None -> None
  with _ -> None

(** Convert Yojson.Safe.t to Graphql_parser.const_value *)
let rec yojson_to_const_value : Yojson.Safe.t -> Graphql_parser.const_value = function
  | `Null -> `Null
  | `Bool b -> `Bool b
  | `Int i -> `Int i
  | `Float f -> `Float f
  | `String s -> `String s
  | `List items -> `List (List.map yojson_to_const_value items)
  | `Assoc fields -> `Assoc (List.map (fun (k, v) -> (k, yojson_to_const_value v)) fields)
  | `Intlit s -> `Int (int_of_string_opt s |> Option.value ~default:0)

(** Convert variables to Graphql format *)
let convert_variables (vars : Yojson.Safe.t option) : (string * Graphql_parser.const_value) list =
  match vars with
  | None -> []
  | Some (`Assoc pairs) ->
    List.map (fun (key, value) -> (key, yojson_to_const_value value)) pairs
  | Some _ -> []

(** Convert Yojson.Basic.t to Yojson.Safe.t for response *)
let rec basic_to_safe : Yojson.Basic.t -> Yojson.Safe.t = function
  | `Null -> `Null
  | `Bool b -> `Bool b
  | `Int i -> `Int i
  | `Float f -> `Float f
  | `String s -> `String s
  | `List items -> `List (List.map basic_to_safe items)
  | `Assoc fields -> `Assoc (List.map (fun (k, v) -> (k, basic_to_safe v)) fields)

(** Execute a GraphQL query

    @param schema The GraphQL schema
    @param ctx Context value passed to resolvers
    @param query GraphQL query string
    @param variables Optional query variables
    @param operation_name Optional operation name for multi-operation documents
*)
let execute schema ~ctx ~query ?variables ?operation_name () =
  match Graphql_parser.parse query with
  | Error err -> Error (`String ("Parse error: " ^ err))
  | Ok document ->
    let variables' = convert_variables variables in
    Schema.execute schema ctx ?operation_name ~variables:variables' document

(** GraphQL HTTP handler

    Creates a POST handler for /graphql endpoint that:
    - Parses JSON request body
    - Executes the query against the schema
    - Returns JSON response

    {[
      let routes = Kirin.router [
        Kirin.post "/graphql" (Kirin.Graphql.handler schema);
      ]
    ]}
*)
let handler ?(make_ctx = fun _req -> ()) schema req =
  let body = Request.body req in
  match parse_request body with
  | None ->
    Response.json ~status:`Bad_request
      (`Assoc [("errors", `List [
        `Assoc [("message", `String "Invalid GraphQL request body")]
      ])])
  | Some gql_req ->
    let ctx = make_ctx req in
    let result = execute schema ~ctx ~query:gql_req.query
      ?variables:gql_req.variables
      ?operation_name:gql_req.operation_name
      ()
    in
    match result with
    | Ok (`Response data) ->
      Response.json (basic_to_safe data)
    | Ok (`Stream _) ->
      (* Streaming responses need WebSocket/SSE - return error for HTTP *)
      Response.json ~status:`Bad_request
        (`Assoc [("errors", `List [
          `Assoc [("message", `String "Subscriptions require WebSocket connection")]
        ])])
    | Error err ->
      let error_msg = match err with
        | `String s -> s
        | json -> Yojson.Basic.to_string json
      in
      Response.json ~status:`Bad_request
        (`Assoc [("errors", `List [
          `Assoc [("message", `String error_msg)]
        ])])

(** GraphQL Playground HTML handler

    Serves the GraphQL Playground IDE for interactive query exploration.

    {[
      let routes = Kirin.router [
        Kirin.get "/graphql" (Kirin.Graphql.playground_handler);
      ]
    ]}
*)
let playground_handler ?(endpoint = "/graphql") _req =
  let html = Printf.sprintf {|<!DOCTYPE html>
<html>
<head>
  <meta charset=utf-8/>
  <meta name="viewport" content="user-scalable=no, initial-scale=1.0, minimum-scale=1.0, maximum-scale=1.0, minimal-ui">
  <title>GraphQL Playground</title>
  <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/graphql-playground-react/build/static/css/index.css" />
  <script src="https://cdn.jsdelivr.net/npm/graphql-playground-react/build/static/js/middleware.js"></script>
</head>
<body>
  <div id="root">
    <style>
      body { margin: 0; overflow: hidden; }
      #root { height: 100vh; }
    </style>
  </div>
  <script>
    window.addEventListener('load', function (event) {
      GraphQLPlayground.init(document.getElementById('root'), {
        endpoint: '%s',
        settings: {
          'request.credentials': 'include',
        }
      })
    })
  </script>
</body>
</html>|} endpoint
  in
  Response.html html

(** Introspection handler (GET /graphql)

    Handles introspection queries for schema discovery tools.
*)
let introspection_handler schema req =
  (* Check for introspection query in URL params *)
  match Request.query "query" req with
  | None -> playground_handler req
  | Some query ->
    let ctx = () in
    let result = execute schema ~ctx ~query () in
    match result with
    | Ok (`Response data) -> Response.json (basic_to_safe data)
    | Ok (`Stream _) ->
      Response.json ~status:`Bad_request
        (`Assoc [("errors", `List [
          `Assoc [("message", `String "Streams not supported via GET")]
        ])])
    | Error err ->
      let error_msg = match err with
        | `String s -> s
        | json -> Yojson.Basic.to_string json
      in
      Response.json ~status:`Bad_request
        (`Assoc [("errors", `List [
          `Assoc [("message", `String error_msg)]
        ])])

(** {1 Middleware} *)

(** GraphQL middleware for adding GraphQL endpoints

    Adds both POST and GET handlers at the specified path.

    {[
      let app =
        Kirin.graphql_middleware ~path:"/graphql" schema
        @@ Kirin.logger
        @@ routes
    ]}
*)
let middleware ?(path = "/graphql") ?(make_ctx = fun _req -> ()) schema =
  let post_handler = handler ~make_ctx schema in
  let get_handler = introspection_handler schema in
  fun next req ->
    let req_path = Request.path req in
    let method_ = Request.meth req in
    if req_path = path then
      match method_ with
      | `POST -> post_handler req
      | `GET -> get_handler req
      | _ ->
        Response.empty `Method_not_allowed
        |> Response.with_header "allow" "GET, POST"
    else
      next req

(** {1 Subscription Support (via WebSocket/SSE)} *)

(** Subscription field constructor *)
let subscription_field = Schema.subscription_field

(** {1 Error Helpers} *)

(** GraphQL error type *)
type graphql_error = {
  message : string;
  locations : (int * int) list option;
  path : string list option;
}

(** Create a GraphQL error response *)
let error_response ?(status = `Bad_request) errors =
  let error_json = `List (List.map (fun err ->
    let fields = [("message", `String err.message)] in
    let fields = match err.locations with
      | Some locs ->
        let locs_json = `List (List.map (fun (line, col) ->
          `Assoc [("line", `Int line); ("column", `Int col)]
        ) locs) in
        ("locations", locs_json) :: fields
      | None -> fields
    in
    let fields = match err.path with
      | Some path -> ("path", `List (List.map (fun s -> `String s) path)) :: fields
      | None -> fields
    in
    `Assoc fields
  ) errors) in
  Response.json ~status (`Assoc [("errors", error_json)])

(** Create a simple error *)
let error message = { message; locations = None; path = None }

(** Create error with location *)
let error_with_location message ~line ~column =
  { message; locations = Some [(line, column)]; path = None }

(** Create error with path *)
let error_with_path message path =
  { message; locations = None; path = Some path }

(** {1 Batched Queries} *)

(** Handle batched GraphQL queries (array of requests)

    Some clients (like Apollo) send multiple queries in a single request.
*)
let batched_handler ?(make_ctx = fun _req -> ()) schema req =
  let body = Request.body req in
  try
    let json = Yojson.Safe.from_string body in
    match json with
    | `List requests ->
      (* Batched request *)
      let results = List.map (fun request_json ->
        let query_opt = match request_json with
          | `Assoc assoc -> List.assoc_opt "query" assoc
          | _ -> None
        in
        match query_opt with
        | Some (`String query) ->
          let variables = match request_json with
            | `Assoc assoc ->
              (match List.assoc_opt "variables" assoc with
               | Some `Null -> None
               | Some v -> Some v
               | None -> None)
            | _ -> None
          in
          let operation_name = match request_json with
            | `Assoc assoc ->
              (match List.assoc_opt "operationName" assoc with
               | Some (`String n) -> Some n
               | _ -> None)
            | _ -> None
          in
          let ctx = make_ctx req in
          let result = execute schema ~ctx ~query ?variables ?operation_name () in
          (match result with
           | Ok (`Response data) -> basic_to_safe data
           | Ok (`Stream _) -> `Assoc [("errors", `List [`Assoc [("message", `String "Subscriptions not supported in batch")]])]
           | Error err ->
             let error_msg = match err with
               | `String s -> s
               | json -> Yojson.Basic.to_string json
             in
             `Assoc [("errors", `List [`Assoc [("message", `String error_msg)]])])
        | _ ->
          `Assoc [("errors", `List [`Assoc [("message", `String "Missing query field")]])]
      ) requests in
      Response.json (`List results)
    | `Assoc _ ->
      (* Single request - delegate to normal handler *)
      handler ~make_ctx schema req
    | _ ->
      Response.json ~status:`Bad_request
        (`Assoc [("errors", `List [`Assoc [("message", `String "Invalid request format")]])])
  with _ ->
    Response.json ~status:`Bad_request
      (`Assoc [("errors", `List [`Assoc [("message", `String "Invalid JSON")]])])
