(** Kirin Validation Example (Pydantic style) *)

open Kirin.Validation

(* 1. Define OCaml types for your data *)
type user = {
  name : string;
  age : int;
  active : bool;
}

(* Mocking yojson generators since we don't have PPX in this example *)
let user_of_yojson json =
  let open Yojson.Safe.Util in
  try
    let name = json |> member "name" |> to_string in
    let age = json |> member "age" |> to_int in
    let active = json |> member "active" |> to_bool in
    Ok { name; age; active }
  with e -> Error (Printexc.to_string e)

let user_to_yojson u =
  `Assoc [
    ("name", `String u.name);
    ("age", `Int u.age);
    ("active", `Bool u.active);
  ]

(* 2. Define your validation rules using the Type DSL *)
let user_v = Type.(obj user_of_yojson [
  "name"   %> string ~min:2 ~max:20 ();
  "age"    %> int ~min:0 ~max:150 ();
  "active" %> bool ();
])

(* 3. Use Kirin.validated to wrap your handler *)
let create_user_handler = Kirin.validated user_v (fun user _req ->
  (* 'user' is already parsed and validated as OCaml record! *)
  Printf.printf "Creating user: %s (age %d)\n" user.name user.age;
  Kirin.json (user_to_yojson user)
)

let routes = Kirin.router [
  Kirin.get "/" (fun _ -> Kirin.html "<h1>Kirin Validation Example</h1>");
  Kirin.post "/users" create_user_handler;
]

let () = Kirin.start ~port:9001
  @@ Kirin.logger
  @@ routes
