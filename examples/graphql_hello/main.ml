(** Kirin GraphQL Hello World Example

    This example shows how to create a simple GraphQL API with Kirin.

    Run: dune exec examples/graphql_hello/main.exe
    Open: http://localhost:8000/graphql (GraphQL Playground)

    Example queries:
    {[
      # Get greeting
      query {
        hello(name: "World")
      }

      # Get user
      query {
        user(id: "1") {
          id
          name
          email
        }
      }

      # List users
      query {
        users {
          id
          name
        }
      }

      # Create user
      mutation {
        createUser(name: "Alice", email: "alice@example.com") {
          id
          name
          email
        }
      }
    ]}
*)

open Kirin.Graphql

(* In-memory data store *)
type user = {
  id : string;
  name : string;
  email : string option;
}

let users : user list ref = ref [
  { id = "1"; name = "Bob"; email = Some "bob@example.com" };
  { id = "2"; name = "Carol"; email = Some "carol@example.com" };
]

let next_id = ref 3

(* GraphQL Types *)
let user_type = obj "User" ~fields:[
  field "id" ~typ:(non_null string)
    ~args:Arg.[]
    ~resolve:(fun _info user -> user.id);
  field "name" ~typ:(non_null string)
    ~args:Arg.[]
    ~resolve:(fun _info user -> user.name);
  field "email" ~typ:string
    ~args:Arg.[]
    ~resolve:(fun _info user -> user.email);
]

(* GraphQL Schema *)
let schema = schema
  ~mutations:[
    field "createUser" ~typ:user_type
      ~args:Arg.[
        arg "name" ~typ:(non_null Arg.string);
        arg "email" ~typ:Arg.string;
      ]
      ~resolve:(fun _info () name email ->
        let id = string_of_int !next_id in
        incr next_id;
        let user = { id; name; email } in
        users := user :: !users;
        Some user);
  ]
  [
    (* Query: hello *)
    field "hello" ~typ:(non_null string)
      ~args:Arg.[arg "name" ~typ:Arg.string]
      ~resolve:(fun _info () name_opt ->
        let name = Option.value ~default:"World" name_opt in
        "Hello, " ^ name ^ "!");

    (* Query: user *)
    field "user" ~typ:user_type
      ~args:Arg.[arg "id" ~typ:(non_null Arg.string)]
      ~resolve:(fun _info () id ->
        List.find_opt (fun u -> u.id = id) !users);

    (* Query: users *)
    field "users" ~typ:(non_null (list (non_null user_type)))
      ~args:Arg.[]
      ~resolve:(fun _info () ->
        !users);
  ]

(* HTTP routes *)
let routes = Kirin.router [
  (* Root page *)
  Kirin.get "/" (fun _ ->
    Kirin.html {|
      <h1>Kirin GraphQL Server</h1>
      <p>Go to <a href="/graphql">/graphql</a> for GraphQL Playground</p>
    |});

  (* GraphQL endpoint (POST) *)
  Kirin.post "/graphql" (Kirin.Graphql.handler schema);

  (* GraphQL Playground (GET) *)
  Kirin.get "/graphql" (Kirin.Graphql.playground_handler);
]

let () =
  Printf.printf "=== Kirin GraphQL Example ===\n";
  Printf.printf "Server: http://localhost:8000\n";
  Printf.printf "Playground: http://localhost:8000/graphql\n";
  Printf.printf "\n";
  Printf.printf "Example queries:\n";
  Printf.printf "  { hello(name: \"World\") }\n";
  Printf.printf "  { user(id: \"1\") { id name email } }\n";
  Printf.printf "  { users { id name } }\n";
  Printf.printf "\n";
  Kirin.start ~port:8000
  @@ Kirin.logger
  @@ Kirin.timing
  @@ routes
