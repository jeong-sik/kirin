type property_type =
  | String
  | Number
  | Boolean
  | Object
  | Array

type property =
  { prop_name : string
  ; prop_type : property_type
  ; prop_attribute : string option
  ; prop_reflect : bool
  ; prop_state : bool
  ; prop_default : Yojson.Safe.t option
  }

type t =
  { tag_name : string
  ; class_name : string
  ; properties : property list
  ; styles : string list
  ; template : string
  ; shadow_mode : [ `Closed | `Open ]
  }

val string_prop
  :  ?attribute:bool
  -> ?reflect:bool
  -> ?default:string option
  -> string
  -> property

val number_prop
  :  ?attribute:bool
  -> ?reflect:bool
  -> ?default:float option
  -> string
  -> property

val boolean_prop
  :  ?attribute:bool
  -> ?reflect:bool
  -> ?default:bool option
  -> string
  -> property

val object_prop : ?default:Yojson.Safe.t option -> string -> property
val array_prop : ?default:Yojson.Safe.t option -> string -> property
val state_prop : property_type -> string -> property

val create
  :  tag_name:string
  -> class_name:string
  -> ?properties:property list
  -> ?styles:string list
  -> ?shadow_mode:[ `Closed | `Open ]
  -> string
  -> t

val is_valid_tag_name : string -> bool
val property_type_to_ts : property_type -> string
val property_decorator : property -> string
val to_typescript : t -> string
val to_javascript : t -> string
val property_type_to_json : property_type -> [> `String of string ]
val property_to_json : property -> [> `Assoc of (string * Yojson.Safe.t) list ]

val to_json
  :  t
  -> [> `Assoc of
          (string
          * [> `List of
                 [> `Assoc of (string * Yojson.Safe.t) list | `String of string ] list
            | `String of string
            ])
            list
     ]
