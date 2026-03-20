type schema_field =
    StringField of { required : bool; }
  | NumberField of { required : bool; min : float option; max : float option;
    }
  | BoolField of { required : bool; }
  | DateField of { required : bool; }
  | EnumField of { required : bool; values : string list; }
  | ArrayField of { required : bool; items : schema_field; }
  | ObjectField of { required : bool; fields : (string * schema_field) list;
    }
  | ReferenceField of { required : bool; collection : string; }
  | ImageField of { required : bool; }
type schema = { name : string; fields : (string * schema_field) list; }
type entry = {
  id : string;
  slug : string;
  collection : string;
  data : (string * Yojson.Safe.t) list;
  body : string;
  render_result : string option;
}
type collection = { name : string; schema : schema; entries : entry list; }
val string_ : ?required:bool -> unit -> schema_field
val number :
  ?required:bool -> ?min:float -> ?max:float -> unit -> schema_field
val bool_ : ?required:bool -> unit -> schema_field
val date : ?required:bool -> unit -> schema_field
val enum : ?required:bool -> string list -> schema_field
val array : ?required:bool -> schema_field -> schema_field
val object_ : ?required:bool -> (string * schema_field) list -> schema_field
val reference : ?required:bool -> string -> schema_field
val image : ?required:bool -> unit -> schema_field
val define_schema : string -> (string * schema_field) list -> schema
val define_collection : name:string -> schema:schema -> unit -> collection
val add_entry : collection -> entry -> collection
val create_entry :
  id:string ->
  collection:string ->
  data:(string * Yojson.Safe.t) list -> body:string -> unit -> entry
val with_slug : string -> entry -> entry
val get_entries : collection -> entry list
val get_entry_by_slug : collection -> string -> entry option
val get_entry_by_id : collection -> string -> entry option
val filter_entries : (entry -> bool) -> collection -> entry list
val sort_entries : (entry -> entry -> int) -> collection -> collection
val parse_frontmatter : string -> string option * string
type validation_error = { field : string; message : string; }
val validate_entry : schema -> entry -> (unit, validation_error list) result
val entry_to_json :
  entry ->
  [> `Assoc of
       (string *
        [> `Assoc of (string * Yojson.Safe.t) list | `String of string ])
       list ]
val collection_to_json :
  collection ->
  [> `Assoc of
       (string *
        [> `List of
             [> `Assoc of
                  (string *
                   [> `Assoc of (string * Yojson.Safe.t) list
                    | `String of string ])
                  list ]
             list
         | `String of string ])
       list ]
