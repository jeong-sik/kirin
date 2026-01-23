(** Route Actions

    Form handling and mutations (Remix-style). *)

open Route_def

(** {1 Action Types} *)

(** Action result with redirect support *)
type 'a action_result =
  | Success of 'a
  | Redirect of string * int  (* url, status *)
  | ValidationError of (string * string) list  (* field, message *)
  | ServerError of string

(** Create success result *)
let success data = Success data

(** Create redirect result *)
let redirect ?(status = 302) url = Redirect (url, status)

(** Create validation error *)
let validation_error errors = ValidationError errors

(** Create server error *)
let server_error msg = ServerError msg

(** {1 Action Utilities} *)

(** Get param from context *)
let param name (ctx : _ action_context) =
  List.find_map (fun (n, v) -> if n = name then Some v else None) ctx.params

(** Get body as JSON *)
let json_body ctx = ctx.body

(** Get field from JSON body *)
let field name ctx =
  match ctx.body with
  | Some (`Assoc fields) -> List.assoc_opt name fields
  | _ -> None

(** Get string field *)
let string_field name ctx =
  match field name ctx with
  | Some (`String s) -> Some s
  | _ -> None

(** Get int field *)
let int_field name ctx =
  match field name ctx with
  | Some (`Int i) -> Some i
  | _ -> None

(** Get bool field *)
let bool_field name ctx =
  match field name ctx with
  | Some (`Bool b) -> Some b
  | _ -> None

(** {1 Form Validation} *)

(** Validation rule *)
type validation_rule =
  | Required
  | MinLength of int
  | MaxLength of int
  | Pattern of string
  | Email
  | Custom of (Yojson.Safe.t option -> bool) * string

(** Validate a field *)
let validate_field name rules ctx =
  let value = field name ctx in
  let errors = List.filter_map (fun rule ->
    match rule with
    | Required ->
      (match value with
       | None | Some `Null -> Some (name, "is required")
       | _ -> None)
    | MinLength min ->
      (match value with
       | Some (`String s) when String.length s < min ->
         Some (name, Printf.sprintf "must be at least %d characters" min)
       | _ -> None)
    | MaxLength max ->
      (match value with
       | Some (`String s) when String.length s > max ->
         Some (name, Printf.sprintf "must be at most %d characters" max)
       | _ -> None)
    | Pattern pattern ->
      (match value with
       | Some (`String s) ->
         let re = Str.regexp pattern in
         if Str.string_match re s 0 then None
         else Some (name, "has invalid format")
       | _ -> None)
    | Email ->
      (match value with
       | Some (`String s) ->
         if String.contains s '@' && String.contains s '.' then None
         else Some (name, "must be a valid email")
       | _ -> None)
    | Custom (check, msg) ->
      if check value then None else Some (name, msg)
  ) rules in
  errors

(** Validate multiple fields *)
let validate rules ctx =
  let all_errors = List.concat_map (fun (name, field_rules) ->
    validate_field name field_rules ctx
  ) rules in
  if all_errors = [] then Ok ()
  else Error all_errors

(** {1 Action Composition} *)

(** Chain actions (run second only if first succeeds) *)
let chain action1 action2 ctx =
  match action1 ctx with
  | Success _ -> action2 ctx
  | other -> other

(** Run action with validation *)
let with_validation rules action ctx =
  match validate rules ctx with
  | Ok () -> action ctx
  | Error errors -> ValidationError errors

(** {1 Action Middleware} *)

(** Add method check *)
let for_method expected_method action ctx =
  if ctx.method_ = expected_method then
    action ctx
  else
    ServerError (Printf.sprintf "Method %s not allowed" (method_to_string ctx.method_))

(** POST only action *)
let post_only action = for_method POST action

(** PUT only action *)
let put_only action = for_method PUT action

(** DELETE only action *)
let delete_only action = for_method DELETE action

(** {1 Response Conversion} *)

(** Convert action result to JSON response *)
let to_json serialize = function
  | Success data ->
    `Assoc [
      ("success", `Bool true);
      ("data", serialize data);
    ]
  | Redirect (url, status) ->
    `Assoc [
      ("redirect", `String url);
      ("status", `Int status);
    ]
  | ValidationError errors ->
    `Assoc [
      ("success", `Bool false);
      ("errors", `Assoc (List.map (fun (k, v) -> (k, `String v)) errors));
    ]
  | ServerError msg ->
    `Assoc [
      ("success", `Bool false);
      ("error", `String msg);
    ]

(** Get HTTP status from result *)
let status_of_result = function
  | Success _ -> 200
  | Redirect (_, status) -> status
  | ValidationError _ -> 400
  | ServerError _ -> 500
