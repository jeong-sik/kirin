(** Request/Response Validation (Phase 16)

    Schema-based validation with clear error messages.

    {b Quick Start:}
    {[
      open Kirin

      let user_schema = Validation.(
        object_ [
          field "name" (string ~min_length:1 ~max_length:100 ());
          field "email" (string ~pattern:"^[^@]+@[^@]+$" ());
          field "age" (int ~minimum:0 ~maximum:150 ());
        ] ~required:["name"; "email"]
      )

      let create_user req =
        match Validation.validate_json user_schema (Request.json req) with
        | Ok data -> Response.json data
        | Error errors -> Response.json ~status:`Bad_request
            (Validation.errors_to_json errors)
    ]}
*)

include Validation_schema
include Validation_format

(** {1 Validation Logic} *)

(** Create error *)
let make_error ?(path = []) ~code message =
  { path; message; code }

(** Validate string *)
let validate_string constraints path value =
  let errors = ref [] in

  (match constraints.min_length with
   | Some min when String.length value < min ->
     errors := make_error ~path ~code:"string_too_short"
       (Printf.sprintf "String must be at least %d characters" min) :: !errors
   | _ -> ());

  (match constraints.max_length with
   | Some max when String.length value > max ->
     errors := make_error ~path ~code:"string_too_long"
       (Printf.sprintf "String must be at most %d characters" max) :: !errors
   | _ -> ());

  (match constraints.pattern with
   | Some pattern ->
     let re = Str.regexp pattern in
     if not (Str.string_match re value 0) then
       errors := make_error ~path ~code:"pattern_mismatch"
         (Printf.sprintf "String does not match pattern: %s" pattern) :: !errors
   | None -> ());

  (match constraints.format with
   | Some fmt ->
     (match validate_format fmt value with
      | Ok () -> ()
      | Error msg -> errors := make_error ~path ~code:"format_invalid" msg :: !errors)
   | None -> ());

  if !errors = [] then Ok (`String value)
  else Error !errors

(** Validate number *)
let validate_number constraints path value =
  let errors = ref [] in

  (match constraints.minimum with
   | Some min ->
     let valid = if constraints.exclusive_min then value > min else value >= min in
     if not valid then
       errors := make_error ~path ~code:"number_too_small"
         (Printf.sprintf "Number must be %s %g"
           (if constraints.exclusive_min then "greater than" else "at least") min) :: !errors
   | None -> ());

  (match constraints.maximum with
   | Some max ->
     let valid = if constraints.exclusive_max then value < max else value <= max in
     if not valid then
       errors := make_error ~path ~code:"number_too_large"
         (Printf.sprintf "Number must be %s %g"
           (if constraints.exclusive_max then "less than" else "at most") max) :: !errors
   | None -> ());

  (match constraints.multiple_of with
   | Some m when m <> 0. ->
     if mod_float value m <> 0. then
       errors := make_error ~path ~code:"not_multiple_of"
         (Printf.sprintf "Number must be a multiple of %g" m) :: !errors
   | _ -> ());

  !errors

(** Recursion depth cap. JSON Schema validation walks the
    schema/value tree via mutually-recursive [validate_array],
    [validate_object], [validate_schema]. Without a cap, an
    attacker-supplied JSON body validated against a permissive
    schema (e.g. one with a self-referential or recursive shape)
    overflows the stack and crashes the request fiber.

    64 is well above any practical hand-written schema depth and
    keeps the call stack bounded to a few KB of frames. *)
let max_validation_depth = 64

exception Schema_too_deep

(** Validate array. [depth] is the current schema-recursion depth; passed
    through to nested [validate_schema] calls. *)
let rec validate_array depth item_schema constraints path items =
  let errors = ref [] in

  (match constraints.min_items with
   | Some min when List.length items < min ->
     errors := make_error ~path ~code:"array_too_short"
       (Printf.sprintf "Array must have at least %d items" min) :: !errors
   | _ -> ());

  (match constraints.max_items with
   | Some max when List.length items > max ->
     errors := make_error ~path ~code:"array_too_long"
       (Printf.sprintf "Array must have at most %d items" max) :: !errors
   | _ -> ());

  if constraints.unique_items then begin
    let seen = Hashtbl.create (List.length items) in
    List.iteri (fun i item ->
      let key = Yojson.Safe.to_string item in
      if Hashtbl.mem seen key then
        errors := make_error ~path:(path @ [string_of_int i]) ~code:"duplicate_item"
          "Array items must be unique" :: !errors
      else
        Hashtbl.add seen key ()
    ) items
  end;

  (* Validate each item *)
  let validated_items = List.mapi (fun i item ->
    let item_path = path @ [string_of_int i] in
    match validate_schema (depth + 1) item_schema item_path item with
    | Ok v -> v
    | Error errs ->
      errors := !errors @ errs;
      item
  ) items in

  if !errors = [] then Ok (`List validated_items)
  else Error !errors

(** Validate object *)
and validate_object depth obj_schema path json =
  match json with
  | `Assoc fields ->
    let errors = ref [] in
    let validated = ref [] in

    (* Check required fields *)
    List.iter (fun req ->
      if not (List.mem_assoc req fields) then
        errors := make_error ~path:(path @ [req]) ~code:"required_field"
          (Printf.sprintf "Required field '%s' is missing" req) :: !errors
    ) obj_schema.required;

    (* Validate each field *)
    List.iter (fun (name, value) ->
      let field_path = path @ [name] in
      match List.assoc_opt name obj_schema.properties with
      | Some field_schema ->
        (match validate_schema (depth + 1) field_schema field_path value with
         | Ok v -> validated := (name, v) :: !validated
         | Error errs -> errors := !errors @ errs)
      | None ->
        if obj_schema.additional_properties then
          validated := (name, value) :: !validated
        else
          errors := make_error ~path:field_path ~code:"unknown_field"
            (Printf.sprintf "Unknown field '%s'" name) :: !errors
    ) fields;

    if !errors = [] then Ok (`Assoc (List.rev !validated))
    else Error !errors
  | _ ->
    Error [make_error ~path ~code:"type_error" "Expected object"]

(** Main validation function. Raises [Schema_too_deep] if a recursive
    schema/value descent exceeds [max_validation_depth]. *)
and validate_schema depth schema path json =
  if depth > max_validation_depth then raise Schema_too_deep;
  match schema, json with
  | Any, v -> Ok v

  | String constraints, `String s -> validate_string constraints path s
  | String _, _ -> Error [make_error ~path ~code:"type_error" "Expected string"]

  | Int constraints, `Int i ->
    let errors = validate_number constraints path (float_of_int i) in
    if errors = [] then Ok json else Error errors
  | Int _, _ -> Error [make_error ~path ~code:"type_error" "Expected integer"]

  | Float constraints, `Float f ->
    let errors = validate_number constraints path f in
    if errors = [] then Ok json else Error errors
  | Float constraints, `Int i ->
    let f = float_of_int i in
    let errors = validate_number constraints path f in
    if errors = [] then Ok (`Float f) else Error errors
  | Float _, _ -> Error [make_error ~path ~code:"type_error" "Expected number"]

  | Bool, `Bool _ -> Ok json
  | Bool, _ -> Error [make_error ~path ~code:"type_error" "Expected boolean"]

  | Null, `Null -> Ok json
  | Null, _ -> Error [make_error ~path ~code:"type_error" "Expected null"]

  | Array (item_schema, constraints), `List items ->
    validate_array depth item_schema constraints path items
  | Array _, _ -> Error [make_error ~path ~code:"type_error" "Expected array"]

  | Object obj_schema, json -> validate_object depth obj_schema path json

  | Enum values, json ->
    if List.exists (fun v -> v = json) values then Ok json
    else Error [make_error ~path ~code:"invalid_enum" "Value is not one of the allowed values"]

  | Const expected, json ->
    if expected = json then Ok json
    else Error [make_error ~path ~code:"const_mismatch" "Value does not match expected constant"]

  | OneOf schemas, json ->
    let results = List.map (fun s -> validate_schema (depth + 1) s path json) schemas in
    let valid = List.filter Result.is_ok results in
    if List.length valid = 1 then List.hd valid
    else if List.length valid = 0 then
      Error [make_error ~path ~code:"one_of_none" "Value does not match any of the allowed schemas"]
    else
      Error [make_error ~path ~code:"one_of_multiple" "Value matches multiple schemas (should match exactly one)"]

  | AnyOf schemas, json ->
    let results = List.map (fun s -> validate_schema (depth + 1) s path json) schemas in
    (match List.find_opt Result.is_ok results with
     | Some result -> result
     | None -> Error [make_error ~path ~code:"any_of_none" "Value does not match any of the allowed schemas"])

  | AllOf schemas, json ->
    let results = List.map (fun s -> validate_schema (depth + 1) s path json) schemas in
    let errors = List.concat_map (function Ok _ -> [] | Error e -> e) results in
    if errors = [] then Ok json
    else Error errors

  | Custom f, json ->
    (match f json with
     | Ok v -> Ok v
     | Error msg -> Error [make_error ~path ~code:"custom_validation" msg])

(** {1 Public API} *)

(** Validate JSON against schema. Returns [Error] with a
    [schema_too_deep] code if recursion exceeds [max_validation_depth]
    rather than letting [Schema_too_deep] escape the module. *)
let validate schema json =
  try validate_schema 0 schema [] json
  with Schema_too_deep ->
    Error [make_error ~code:"schema_too_deep"
             (Printf.sprintf
                "Validation aborted: schema/value nested deeper than %d levels"
                max_validation_depth)]

(** Validate and return typed result *)
let validate_json schema json =
  validate schema json

(** Validate string representation *)
let validate_string_json schema str =
  try
    let json = Yojson.Safe.from_string str in
    validate schema json
  with Yojson.Json_error _ ->
    Error [make_error ~code:"parse_error" "Invalid JSON"]

(** {1 Common Schemas} *)

(** Email string *)
let email () = string ~format:"email" ()

(** UUID string *)
let uuid () = string ~format:"uuid" ()

(** URI string *)
let uri () = string ~format:"uri" ()

(** Date string (YYYY-MM-DD) *)
let date () = string ~format:"date" ()

(** DateTime string (ISO 8601) *)
let datetime () = string ~format:"date-time" ()

(** Positive integer *)
let positive_int () = int ~minimum:1 ()

(** Non-negative integer *)
let non_negative_int () = int ~minimum:0 ()

(** Percentage (0-100) *)
let percentage () = float ~minimum:0. ~maximum:100. ()

(** Non-empty string *)
let non_empty_string () = string ~min_length:1 ()

(** {1 Type-safe Validation (Pydantic style)} *)

(** Type-safe validator that combines JSON parsing and schema validation

    @param of_yojson Function generated by ppx_deriving_yojson
    @param schema Kirin validation schema
*)
let validate_type of_yojson schema json =
  match validate schema json with
  | Error e -> Error e
  | Ok validated_json ->
    (match of_yojson validated_json with
     | Ok v -> Ok v
     | Error msg ->
       Error [make_error ~code:"mapping_error" (Printf.sprintf "Failed to map JSON to type: %s" msg)])

(** Handler wrapper for automatic body validation (FastAPI style)

    {[
      let create_user_handler = Validation.validated_body user_of_yojson user_schema (fun user req ->
        (* 'user' is already parsed and validated! *)
        Response.json (user_to_yojson user)
      )
    ]}
*)
let validated_body of_yojson schema handler next req =
  match Request.json_body req with
  | Error _ -> Response.json ~status:`Bad_request (`Assoc [("error", `String "Invalid JSON")])
  | Ok json ->
    match validate_type of_yojson schema json with
    | Ok validated_data -> handler validated_data next req
    | Error errors ->
      Response.json ~status:`Bad_request (errors_to_json errors)

(** {1 Kirin Type-safe DSL} *)

type 'a typed_schema = {
  schema : schema;
  of_json : Yojson.Safe.t -> ('a, string) Stdlib.result;
}

module Type = struct
  let make schema of_json = { schema; of_json }

  let string ?min ?max ?pattern ?format () =
    make (String {
      min_length = min;
      max_length = max;
      pattern;
      format
    }) (fun j -> match j with `String s -> Ok s | _ -> Error "Expected string")

  let int ?min ?max () =
    make (Int {
      minimum = Option.map float_of_int min;
      maximum = Option.map float_of_int max;
      exclusive_min = false;
      exclusive_max = false;
      multiple_of = None
    }) (fun j -> match j with `Int i -> Ok i | _ -> Error "Expected integer")

  let bool () = make Bool (fun j -> match j with `Bool b -> Ok b | _ -> Error "Expected boolean")

  let obj of_json props =
    let required = List.map fst props in
    make (Object { properties = props; required; additional_properties = false }) of_json

  (** Infix operator for field definition: "name" %> Type.string ()
      Only extracts the schema to allow heterogeneous field types in the list.
  *)
  let ( %> ) name t = (name, t.schema)
end

(** New validated handler using the Type DSL

    {[
      let v = Type.(obj user_of_yojson [
        "name" %> string ~min:1 ();
        "age"  %> int ~min:0 ();
      ])

      let create_user = validated v (fun user req -> ...)
    ]}
*)
let validated t handler req =
  match Request.json_body req with
  | Error _ -> Response.json ~status:`Bad_request (`Assoc [("error", `String "Invalid JSON")])
  | Ok json ->
    match validate t.schema json with
    | Error errors -> Response.json ~status:`Bad_request (errors_to_json errors)
    | Ok validated_json ->
      (match t.of_json validated_json with
       | Ok v -> handler v req
       | Error msg ->
         Response.json ~status:`Bad_request (`Assoc [("error", `String msg)]))

(** {1 Request Helpers} *)

(** Validate body string with schema *)
let validate_body schema body_string =
  validate_string_json schema body_string

(** Coerce string to JSON based on schema type (for query params) *)
let coerce_string_to_json schema value =
  match schema with
  | Int _ | Float _ ->
    (* Try to parse as number *)
    (try
      if String.contains value '.' then
        `Float (float_of_string value)
      else
        `Int (int_of_string value)
     with Failure _ -> `String value)
  | Bool ->
    (* Try to parse as boolean *)
    (match String.lowercase_ascii value with
     | "true" | "1" | "yes" -> `Bool true
     | "false" | "0" | "no" -> `Bool false
     | _ -> `String value)
  | _ -> `String value

(** Validate query parameters list *)
let validate_query_params params query_getter =
  let errors = ref [] in
  List.iter (fun (name, schema) ->
    match query_getter name with
    | None -> ()  (* Optional by default *)
    | Some value ->
      let json = coerce_string_to_json schema value in
      match validate schema json with
      | Ok _ -> ()
      | Error errs ->
        errors := !errors @ List.map (fun e -> { e with path = ["query"; name] @ e.path }) errs
  ) params;
  if !errors = [] then Ok ()
  else Error !errors
