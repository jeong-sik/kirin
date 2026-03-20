type property = {
  prop_name : string;
  prop_default : Yojson.Safe.t;
  prop_type : [ `Array | `Boolean | `Number | `Object | `String ];
}
type method_ = {
  method_name : string;
  method_params : string list;
  method_body : string;
}
type getter = { getter_name : string; getter_body : string; }
type watcher = { watch_property : string; watch_handler : string; }
type t = {
  name : string;
  properties : property list;
  methods : method_ list;
  getters : getter list;
  watchers : watcher list;
  init : string option;
  destroy : string option;
}
val create : string -> t
val with_property :
  ?default:Yojson.Safe.t ->
  [ `Array | `Boolean | `Number | `Object | `String ] -> string -> t -> t
val with_method : string -> string list -> string -> t -> t
val with_getter : string -> string -> t -> t
val with_watcher : string -> string -> t -> t
val with_init : string -> t -> t
val with_destroy : string -> t -> t
val string_prop : ?default:string -> string -> property
val number_prop : ?default:float -> string -> property
val boolean_prop : ?default:bool -> string -> property
val array_prop : ?default:Yojson.Safe.t list -> string -> property
val object_prop :
  ?default:(string * Yojson.Safe.t) list -> string -> property
val to_x_data : t -> string
val to_alpine_data : t -> string
val to_x_data_ref : t -> string
val to_x_data_with_props : t -> (string * Yojson.Safe.t) list -> string
val registration_script : t list -> string
val inline_data : data:(string * Yojson.Safe.t) list -> t -> string
val property_to_json :
  property -> [> `Assoc of (string * Yojson.Safe.t) list ]
val method_to_json :
  method_ ->
  [> `Assoc of
       (string *
        [> `List of [> `String of string ] list | `String of string ])
       list ]
val to_json :
  t ->
  [> `Assoc of
       (string *
        [> `List of [> `Assoc of (string * Yojson.Safe.t) list ] list
         | `Null
         | `String of string ])
       list ]
