type property = {
  store_prop_name : string;
  store_prop_value : Yojson.Safe.t;
}
type method_ = { store_method_name : string; store_method_body : string; }
type t = {
  name : string;
  properties : property list;
  methods : method_ list;
}
val create : string -> t
val with_property : string -> Yojson.Safe.t -> t -> t
val with_method : string -> string -> t -> t
val from_object : string -> (string * Yojson.Safe.t) list -> t
val to_object : t -> string
val to_alpine_store : t -> string
val accessor : t -> string -> string
val registration_script : t list -> string
val hydration_script :
  store_name:string -> data:(string * Yojson.Safe.t) list -> string
val with_server_data : (string * Yojson.Safe.t) list -> t -> t
val magic_store : string -> string
val x_text_store : string -> string -> string
val x_bind_store : string -> string -> string -> string
type persist_config = {
  persist_name : string;
  persist_storage : [ `Local | `Session ];
}
val to_persisted_store :
  ?storage:[< `Local | `Session > `Local ] -> t -> string
val property_to_json :
  property -> [> `Assoc of (string * Yojson.Safe.t) list ]
val to_json :
  t ->
  [> `Assoc of
       (string *
        [> `List of [> `Assoc of (string * Yojson.Safe.t) list ] list
         | `String of string ])
       list ]
