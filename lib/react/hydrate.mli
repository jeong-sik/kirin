type options = {
  title : string;
  lang : string;
  meta : (string * string) list;
  head_extra : string list;
  scripts : string list;
  styles : string list;
  initial_data : Yojson.Safe.t option;
  dehydrated_state : Yojson.Safe.t option;
  root_id : string;
  root_class : string option;
  body_class : string option;
  body_attrs : (string * string) list;
  nonce : string option;
}
val default_options : options
val render : options -> string
val shell :
  title:string ->
  ?lang:string ->
  ?meta:(string * string) list ->
  ?initial_data:Yojson.Safe.t option ->
  manifest:Manifest.t -> entry:string -> unit -> string
val response : options -> Kirin.Response.t
val simple_response :
  title:string ->
  ?meta:(string * string) list ->
  ?initial_data:Yojson.Safe.t option ->
  manifest:Manifest.t ->
  entry:string -> unit -> Kirin.Response.t
module Builder :
  sig
    type t = options
    val create : unit -> options
    val title : string -> options -> options
    val lang : string -> options -> options
    val meta : (string * string) list -> options -> options
    val add_meta : name:string -> content:string -> options -> options
    val head_extra : string list -> options -> options
    val add_head : string -> options -> options
    val scripts : string list -> options -> options
    val add_script : string -> options -> options
    val styles : string list -> options -> options
    val add_style : string -> options -> options
    val initial_data : Yojson.Safe.t -> options -> options
    val dehydrated_state : Yojson.Safe.t -> options -> options
    val root_id : string -> options -> options
    val root_class : string -> options -> options
    val body_class : string -> options -> options
    val body_attr : name:string -> value:string -> options -> options
    val nonce : string -> options -> options
    val build : 'a -> 'a
    val render : options -> string
    val response : options -> Kirin.Response.t
  end
val with_manifest :
  manifest:Manifest.t -> entry:string -> options -> options
val with_preloads :
  manifest:Manifest.t -> entry:string -> options -> options
val streaming_placeholder : ?id:string -> unit -> string
val suspense_placeholder : id:string -> fallback:string -> unit -> string
