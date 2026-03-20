type entry = {
  file : string;
  src : string;
  is_entry : bool;
  css : string list;
  assets : string list;
  dynamic_imports : string list;
  imports : string list;
}
type t = (string * entry) list
val empty_entry : string -> entry
val parse_entry :
  src:string ->
  [> `Assoc of
       (string *
        [> `Bool of bool
         | `List of [> `String of string ] list
         | `String of string ])
       list ] ->
  entry
val parse :
  [> `Assoc of
       (string *
        [> `Assoc of
             (string *
              [> `Bool of bool
               | `List of [> `String of string ] list
               | `String of string ])
             list ])
       list ] ->
  t
val parse_string : string -> (t, string) result
val load : string -> (t, string) result
val find : t -> string -> entry option
val resolve : t -> string -> string option
val css_for : t -> string -> string list
val assets_for : t -> string -> string list
val imports_for : t -> string -> string list
val dynamic_imports_for : t -> string -> string list
val entries : ('a * entry) list -> ('a * entry) list
val all_css : ('a * entry) list -> String.t list
val all_assets : ('a * entry) list -> String.t list
val preload_hints : ?base_url:string -> t -> string -> string
val hashed_filename : t -> string -> string option
val mem : ('a * 'b) list -> 'a -> bool
val sources : ('a * 'b) list -> 'a list
val pp : Format.formatter -> (string * entry) list -> unit
val to_string : (string * entry) list -> string
