type entry = {
  file : string;
  src : string;
  is_entry : bool;
  css : string list;
  assets : string list;
  dynamic_imports : string list;
  imports : string list;
}
type t = { entries : (string * entry) list; base_url : string; }
val empty_entry : entry
val to_list_safe : [> `List of 'a list ] -> 'a list
val parse_entry : Yojson__Safe.t -> entry
val parse :
  ?base_url:string -> [> `Assoc of (string * Yojson__Safe.t) list ] -> t
val load : ?base_url:string -> string -> (t, string) result
val resolve : t -> string -> entry option
val resolve_file : t -> string -> string option
val css_for : t -> string -> string list
val all_css : t -> string list
val entry_points : t -> string list
val preload_hint : t -> string -> string
val preload_hints : t -> string -> string
val script_tag : t -> string -> string
val css_link_tag : string -> string
val css_tags : t -> string -> string
val entry_to_json :
  entry ->
  [> `Assoc of
       (string *
        [> `Bool of bool
         | `List of [> `String of string ] list
         | `String of string ])
       list ]
val to_json :
  t ->
  [> `Assoc of
       (string *
        [> `Assoc of
             (string *
              [> `Assoc of
                   (string *
                    [> `Bool of bool
                     | `List of [> `String of string ] list
                     | `String of string ])
                   list ])
             list
         | `String of string ])
       list ]
