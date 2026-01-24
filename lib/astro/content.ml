(** Astro Content Collections

    Type-safe content management for Markdown/MDX. *)

(** {1 Content Types} *)

(** Content schema field *)
type schema_field =
  | StringField of { required: bool }
  | NumberField of { required: bool; min: float option; max: float option }
  | BoolField of { required: bool }
  | DateField of { required: bool }
  | EnumField of { required: bool; values: string list }
  | ArrayField of { required: bool; items: schema_field }
  | ObjectField of { required: bool; fields: (string * schema_field) list }
  | ReferenceField of { required: bool; collection: string }
  | ImageField of { required: bool }

(** Content schema *)
type schema = {
  name: string;
  fields: (string * schema_field) list;
}

(** Content entry *)
type entry = {
  id: string;
  slug: string;
  collection: string;
  data: (string * Yojson.Safe.t) list;
  body: string;
  render_result: string option;
}

(** Content collection *)
type collection = {
  name: string;
  schema: schema;
  entries: entry list;
}

(** {1 Schema Builders} *)

(** String field *)
let string_ ?(required=true) () = StringField { required }

(** Number field *)
let number ?(required=true) ?min ?max () = NumberField { required; min; max }

(** Boolean field *)
let bool_ ?(required=true) () = BoolField { required }

(** Date field *)
let date ?(required=true) () = DateField { required }

(** Enum field *)
let enum ?(required=true) values = EnumField { required; values }

(** Array field *)
let array ?(required=true) items = ArrayField { required; items }

(** Object field *)
let object_ ?(required=true) fields = ObjectField { required; fields }

(** Reference field *)
let reference ?(required=true) collection = ReferenceField { required; collection }

(** Image field *)
let image ?(required=true) () = ImageField { required }

(** {1 Schema Creation} *)

(** Create schema *)
let define_schema name fields = { name; fields }

(** {1 Collection Creation} *)

(** Create collection *)
let define_collection ~name ~schema () = {
  name;
  schema;
  entries = [];
}

(** Add entry to collection *)
let add_entry collection entry =
  { collection with entries = entry :: collection.entries }

(** {1 Entry Creation} *)

(** Create entry *)
let create_entry ~id ~collection ~data ~body () = {
  id;
  slug = id;  (* Default slug is id *)
  collection;
  data;
  body;
  render_result = None;
}

(** Set entry slug *)
let with_slug slug entry = { entry with slug }

(** {1 Content Querying} *)

(** Get all entries *)
let get_entries collection = collection.entries

(** Get entry by slug *)
let get_entry_by_slug collection slug =
  List.find_opt (fun e -> e.slug = slug) collection.entries

(** Get entry by id *)
let get_entry_by_id collection id =
  List.find_opt (fun e -> e.id = id) collection.entries

(** Filter entries *)
let filter_entries pred collection =
  List.filter pred collection.entries

(** Sort entries *)
let sort_entries compare collection =
  { collection with entries = List.sort compare collection.entries }

(** {1 Frontmatter Parsing} *)

(** Parse frontmatter from content *)
let parse_frontmatter content =
  let lines = String.split_on_char '\n' content in
  match lines with
  | "---" :: rest ->
    let rec parse_until_end acc = function
      | [] -> (List.rev acc, "")
      | "---" :: body -> (List.rev acc, String.concat "\n" body)
      | line :: rest -> parse_until_end (line :: acc) rest
    in
    let (frontmatter_lines, body) = parse_until_end [] rest in
    let frontmatter = String.concat "\n" frontmatter_lines in
    (Some frontmatter, body)
  | _ -> (None, content)

(** {1 Validation} *)

(** Validation error *)
type validation_error = {
  field: string;
  message: string;
}

(** Validate entry against schema *)
let validate_entry schema entry =
  let errors = ref [] in
  List.iter (fun (name, field) ->
    let value = List.assoc_opt name entry.data in
    match field, value with
    | StringField { required = true }, None ->
      errors := { field = name; message = "required field missing" } :: !errors
    | NumberField { required = true; _ }, None ->
      errors := { field = name; message = "required field missing" } :: !errors
    | BoolField { required = true }, None ->
      errors := { field = name; message = "required field missing" } :: !errors
    | DateField { required = true }, None ->
      errors := { field = name; message = "required field missing" } :: !errors
    | EnumField { required = true; _ }, None ->
      errors := { field = name; message = "required field missing" } :: !errors
    | ReferenceField { required = true; _ }, None ->
      errors := { field = name; message = "required field missing" } :: !errors
    | ImageField { required = true }, None ->
      errors := { field = name; message = "required field missing" } :: !errors
    | NumberField { min = Some m; _ }, Some (`Float f) when f < m ->
      errors := { field = name; message = Printf.sprintf "value must be >= %f" m } :: !errors
    | NumberField { max = Some m; _ }, Some (`Float f) when f > m ->
      errors := { field = name; message = Printf.sprintf "value must be <= %f" m } :: !errors
    | EnumField { values; _ }, Some (`String v) when not (List.mem v values) ->
      errors := { field = name; message = Printf.sprintf "must be one of: %s" (String.concat ", " values) } :: !errors
    | _ -> ()
  ) schema.fields;
  if !errors = [] then Ok ()
  else Error (List.rev !errors)

(** {1 Serialization} *)

(** Entry to JSON *)
let entry_to_json entry =
  `Assoc [
    ("id", `String entry.id);
    ("slug", `String entry.slug);
    ("collection", `String entry.collection);
    ("data", `Assoc entry.data);
    ("body", `String entry.body);
  ]

(** Collection to JSON *)
let collection_to_json collection =
  `Assoc [
    ("name", `String collection.name);
    ("entries", `List (List.map entry_to_json collection.entries));
  ]
