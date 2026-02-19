(** Kirin MCP - JSON Schema Builder

    Helper functions to construct JSON Schema objects for tool input definitions.
    Default dialect: JSON Schema 2020-12 (per MCP 2025-11-25 spec).
*)

(** {1 Primitive Types} *)

(** String type *)
let string ?description ?enum ?pattern ?min_length ?max_length () =
  let base = ["type", `String "string"] in
  let with_desc = match description with
    | Some d -> ("description", `String d) :: base
    | None -> base
  in
  let with_enum = match enum with
    | Some values -> ("enum", `List (List.map (fun s -> `String s) values)) :: with_desc
    | None -> with_desc
  in
  let with_pattern = match pattern with
    | Some p -> ("pattern", `String p) :: with_enum
    | None -> with_enum
  in
  let with_min = match min_length with
    | Some m -> ("minLength", `Int m) :: with_pattern
    | None -> with_pattern
  in
  let with_max = match max_length with
    | Some m -> ("maxLength", `Int m) :: with_min
    | None -> with_min
  in
  `Assoc with_max

(** Integer type *)
let int ?description ?minimum ?maximum ?exclusive_minimum ?exclusive_maximum () =
  let base = ["type", `String "integer"] in
  let with_desc = match description with
    | Some d -> ("description", `String d) :: base
    | None -> base
  in
  let with_min = match minimum with
    | Some m -> ("minimum", `Int m) :: with_desc
    | None -> with_desc
  in
  let with_max = match maximum with
    | Some m -> ("maximum", `Int m) :: with_min
    | None -> with_min
  in
  let with_exc_min = match exclusive_minimum with
    | Some m -> ("exclusiveMinimum", `Int m) :: with_max
    | None -> with_max
  in
  let with_exc_max = match exclusive_maximum with
    | Some m -> ("exclusiveMaximum", `Int m) :: with_exc_min
    | None -> with_exc_min
  in
  `Assoc with_exc_max

(** Number (float) type *)
let number ?description ?minimum ?maximum () =
  let base = ["type", `String "number"] in
  let with_desc = match description with
    | Some d -> ("description", `String d) :: base
    | None -> base
  in
  let with_min = match minimum with
    | Some m -> ("minimum", `Float m) :: with_desc
    | None -> with_desc
  in
  let with_max = match maximum with
    | Some m -> ("maximum", `Float m) :: with_min
    | None -> with_min
  in
  `Assoc with_max

(** Boolean type *)
let bool ?description () =
  let base = ["type", `String "boolean"] in
  let with_desc = match description with
    | Some d -> ("description", `String d) :: base
    | None -> base
  in
  `Assoc with_desc

(** Null type *)
let null () =
  `Assoc ["type", `String "null"]

(** {1 Composite Types} *)

(** Array type *)
let array ?description ?min_items ?max_items ?unique_items items =
  let base = [
    "type", `String "array";
    "items", items;
  ] in
  let with_desc = match description with
    | Some d -> ("description", `String d) :: base
    | None -> base
  in
  let with_min = match min_items with
    | Some m -> ("minItems", `Int m) :: with_desc
    | None -> with_desc
  in
  let with_max = match max_items with
    | Some m -> ("maxItems", `Int m) :: with_min
    | None -> with_min
  in
  let with_unique = match unique_items with
    | Some true -> ("uniqueItems", `Bool true) :: with_max
    | _ -> with_max
  in
  `Assoc with_unique

(** Object type *)
let object_ ?description ?additional_properties properties ~required =
  let props = `Assoc (List.map (fun (name, schema) -> (name, schema)) properties) in
  let base = [
    "type", `String "object";
    "properties", props;
  ] in
  let with_required = if required = [] then base
    else ("required", `List (List.map (fun s -> `String s) required)) :: base
  in
  let with_desc = match description with
    | Some d -> ("description", `String d) :: with_required
    | None -> with_required
  in
  let with_additional = match additional_properties with
    | Some false -> ("additionalProperties", `Bool false) :: with_desc
    | _ -> with_desc
  in
  `Assoc with_additional

(** {1 Combinators} *)

(** OneOf (union type) *)
let one_of ?description schemas =
  let base = ["oneOf", `List schemas] in
  let with_desc = match description with
    | Some d -> ("description", `String d) :: base
    | None -> base
  in
  `Assoc with_desc

(** AnyOf (any of multiple types) *)
let any_of ?description schemas =
  let base = ["anyOf", `List schemas] in
  let with_desc = match description with
    | Some d -> ("description", `String d) :: base
    | None -> base
  in
  `Assoc with_desc

(** AllOf (must match all schemas) *)
let all_of ?description schemas =
  let base = ["allOf", `List schemas] in
  let with_desc = match description with
    | Some d -> ("description", `String d) :: base
    | None -> base
  in
  `Assoc with_desc

(** {1 Special Types} *)

(** Const (exact value) *)
let const value =
  `Assoc ["const", value]

(** Enum (one of specific values) *)
let enum values =
  `Assoc ["enum", `List values]

(** Reference to another schema *)
let ref_ uri =
  `Assoc ["$ref", `String uri]

(** {1 Convenience Builders} *)

(** Empty object (no properties required) *)
let empty_object () =
  object_ [] ~required:[]

(** Optional field wrapper *)
let optional schema =
  one_of [schema; null ()]

(** String with default value *)
let string_with_default ~default ?description () =
  let base = string ?description () in
  match base with
  | `Assoc fields -> `Assoc (("default", `String default) :: fields)
  | _ -> base

(** Integer with default value *)
let int_with_default ~default ?description () =
  let base = int ?description () in
  match base with
  | `Assoc fields -> `Assoc (("default", `Int default) :: fields)
  | _ -> base

(** Boolean with default value *)
let bool_with_default ~default ?description () =
  let base = bool ?description () in
  match base with
  | `Assoc fields -> `Assoc (("default", `Bool default) :: fields)
  | _ -> base

(** {1 Dialect} *)

(** Default JSON Schema dialect (2020-12 per MCP 2025-11-25 spec) *)
let default_dialect = "https://json-schema.org/draft/2020-12/schema"

(** Add $schema annotation to a schema object *)
let with_dialect schema =
  match schema with
  | `Assoc fields -> `Assoc (("$schema", `String default_dialect) :: fields)
  | _ -> schema
