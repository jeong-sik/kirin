type mode = Native | Compat
type options = {
  title : string;
  meta : (string * string) list;
  scripts : string list;
  styles : string list;
  initial_data : Yojson.Safe.t option;
  root_id : string;
  body_class : string option;
  lang : string;
  mode : mode;
  signals_data : (string * Yojson.Safe.t) list;
}
val default_options : options
val escape_json : string -> string
val serialize_data : Yojson.Safe.t -> string
val data_script : Yojson.Safe.t option -> string
val signals_script : (string * Yojson.Safe.t) list -> string
val meta_tag : string * string -> string
val meta_tags : (string * string) list -> string
val render : options -> string
val shell :
  title:string ->
  ?meta:(string * string) list ->
  ?initial_data:Yojson.Safe.t option ->
  ?signals_data:(string * Yojson.Safe.t) list ->
  manifest:Manifest.t ->
  entry:string -> ?mode:mode -> unit -> string
val client_entry_native : component:string -> root_id:string -> string
val client_entry_compat : component:string -> root_id:string -> string
val client_entry : component:string -> root_id:string -> mode -> string
val mode_to_string : mode -> string
val options_to_json : options -> [> `Assoc of (string * Yojson.Safe.t) list ]
