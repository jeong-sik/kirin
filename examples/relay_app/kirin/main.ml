(** Kirin + Relay Example (Backend) *)

open Kirin.Graphql
open Kirin.Graphql_relay

type user = { id : string; name : string }

(* The type of objects that implement the Node interface *)
type node = User of user

let users_db = [
  { id = "1"; name = "Alice" };
  { id = "2"; name = "Bob" };
  { id = "3"; name = "Charlie" };
  { id = "4"; name = "David" };
  { id = "5"; name = "Eve" };
]

(* 1. Recursive definition handling for User type *)
let user_type_ref = ref None

(* 3. Define User type *)
let user_type = obj "User"
  ~fields:[
    field "id" ~typ:(non_null id)
      ~args:Arg.[]
      ~resolve:(fun _ (User u) -> to_global_id "User" u.id);
    field "name" ~typ:(non_null string)
      ~args:Arg.[]
      ~resolve:(fun _ (User u) -> u.name);
  ]

(* Fix the recursive reference *)
let () = user_type_ref := Some user_type

let (user_connection, _) = connection_definitions "User" user_type

let schema = schema [
  (* Root field: users(first: Int, after: String) *)
  field "users"
    ~typ:(non_null user_connection)
    ~args:(Kirin.Graphql_relay.args ())
    ~resolve:(fun _ () first after last before ->
      let args = get_args first after last before in
      let nodes = List.map (fun u -> User u) users_db in
      connection_from_list nodes args
    )
]

let routes = Kirin.router [
  Kirin.get "/" (fun _ -> Kirin.html "<h1>Kirin + Relay Server</h1><p><a href='/graphql'>Playground</a></p>");
  Kirin.post "/graphql" (Kirin.Graphql.handler schema);
  Kirin.get "/graphql" (Kirin.Graphql.playground_handler);
]

let () = Kirin.start ~port:9000
  @@ Kirin.logger
  @@ Kirin.cors ()
  @@ Kirin.Graphql.middleware schema
  @@ routes