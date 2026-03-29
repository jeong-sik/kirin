type 'a action_result =
  | Success of 'a
  | Redirect of string * int
  | ValidationError of (string * string) list
  | ServerError of string

val success : 'a -> 'a action_result
val redirect : ?status:int -> string -> 'a action_result
val validation_error : (string * string) list -> 'a action_result
val server_error : string -> 'a action_result
val param : string -> 'a Route_def.action_context -> string option
val json_body : 'a Route_def.action_context -> Yojson.Safe.t option
val field : string -> 'a Route_def.action_context -> Yojson.Safe.t option
val string_field : string -> 'a Route_def.action_context -> string option
val int_field : string -> 'a Route_def.action_context -> int option
val bool_field : string -> 'a Route_def.action_context -> bool option

type validation_rule =
  | Required
  | MinLength of int
  | MaxLength of int
  | Pattern of string
  | Email
  | Custom of (Yojson.Safe.t option -> bool) * string

val validate_field
  :  string
  -> validation_rule list
  -> 'a Route_def.action_context
  -> (string * string) list

val validate
  :  (string * validation_rule list) list
  -> 'a Route_def.action_context
  -> (unit, (string * string) list) result

val chain : ('a -> 'b action_result) -> ('a -> 'b action_result) -> 'a -> 'b action_result

val with_validation
  :  (string * validation_rule list) list
  -> ('a Route_def.action_context -> 'b action_result)
  -> 'a Route_def.action_context
  -> 'b action_result

val for_method
  :  Route_def.http_method
  -> ('a Route_def.action_context -> 'b action_result)
  -> 'a Route_def.action_context
  -> 'b action_result

val post_only
  :  ('a Route_def.action_context -> 'b action_result)
  -> 'a Route_def.action_context
  -> 'b action_result

val put_only
  :  ('a Route_def.action_context -> 'b action_result)
  -> 'a Route_def.action_context
  -> 'b action_result

val delete_only
  :  ('a Route_def.action_context -> 'b action_result)
  -> 'a Route_def.action_context
  -> 'b action_result

val to_json
  :  ('a
      -> ([> `Assoc of (string * [> `String of string ]) list
          | `Bool of bool
          | `Int of int
          | `String of string
          ]
          as
          'b))
  -> 'a action_result
  -> [> `Assoc of (string * 'b) list ]

val status_of_result : 'a action_result -> int
