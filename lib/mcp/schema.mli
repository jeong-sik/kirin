val string
  :  ?description:string
  -> ?enum:'a list
  -> ?pattern:string
  -> ?min_length:'b
  -> ?max_length:'b
  -> unit
  -> [> `Assoc of
          (string
          * [> `Int of 'b | `List of [> `String of 'a ] list | `String of string ])
            list
     ]

val int
  :  ?description:string
  -> ?minimum:'a
  -> ?maximum:'a
  -> ?exclusive_minimum:'a
  -> ?exclusive_maximum:'a
  -> unit
  -> [> `Assoc of (string * [> `Int of 'a | `String of string ]) list ]

val number
  :  ?description:string
  -> ?minimum:'a
  -> ?maximum:'a
  -> unit
  -> [> `Assoc of (string * [> `Float of 'a | `String of string ]) list ]

val bool
  :  ?description:string
  -> unit
  -> [> `Assoc of (string * [> `String of string ]) list ]

val null : unit -> [> `Assoc of (string * [> `String of string ]) list ]

val array
  :  ?description:string
  -> ?min_items:'a
  -> ?max_items:'a
  -> ?unique_items:bool
  -> ([> `Bool of bool | `Int of 'a | `String of string ] as 'b)
  -> [> `Assoc of (string * 'b) list ]

val object_
  :  ?description:string
  -> ?additional_properties:bool
  -> ('a * 'b) list
  -> required:'c list
  -> [> `Assoc of
          (string
          * [> `Assoc of ('a * 'b) list
            | `Bool of bool
            | `List of [> `String of 'c ] list
            | `String of string
            ])
            list
     ]

val one_of
  :  ?description:'a
  -> 'b
  -> [> `Assoc of (string * [> `List of 'b | `String of 'a ]) list ]

val any_of
  :  ?description:'a
  -> 'b
  -> [> `Assoc of (string * [> `List of 'b | `String of 'a ]) list ]

val all_of
  :  ?description:'a
  -> 'b
  -> [> `Assoc of (string * [> `List of 'b | `String of 'a ]) list ]

val const : 'a -> [> `Assoc of (string * 'a) list ]
val enum : 'a -> [> `Assoc of (string * [> `List of 'a ]) list ]
val ref_ : 'a -> [> `Assoc of (string * [> `String of 'a ]) list ]

val empty_object
  :  unit
  -> [> `Assoc of
          (string
          * [> `Assoc of ('a * 'b) list
            | `Bool of bool
            | `List of [> `String of 'c ] list
            | `String of string
            ])
            list
     ]

val optional
  :  ([> `Assoc of (string * [> `String of string ]) list ] as 'a)
  -> [> `Assoc of (string * [> `List of 'a list | `String of 'b ]) list ]

val string_with_default
  :  default:string
  -> ?description:string
  -> unit
  -> [> `Assoc of
          (string
          * [> `Int of 'a | `List of [> `String of 'b ] list | `String of string ])
            list
     ]

val int_with_default
  :  default:'a
  -> ?description:string
  -> unit
  -> [> `Assoc of (string * [> `Int of 'a | `String of string ]) list ]

val bool_with_default
  :  default:'a
  -> ?description:string
  -> unit
  -> [> `Assoc of (string * [> `Bool of 'a | `String of string ]) list ]

val default_dialect : string
val with_dialect : ([> `Assoc of (string * [> `String of string ]) list ] as 'a) -> 'a
