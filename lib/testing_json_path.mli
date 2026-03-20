val get :
  string ->
  ([> `Assoc of (string * 'a) list | `List of 'a list ] as 'a) -> 'a option
val exists :
  string ->
  ([> `Assoc of (string * 'a) list | `List of 'a list ] as 'a) -> bool
val get_string :
  string ->
  ([> `Assoc of (string * 'a) list | `List of 'a list | `String of 'b ] as 'a) ->
  'b option
val get_int :
  string ->
  ([> `Assoc of (string * 'a) list | `Int of 'b | `List of 'a list ] as 'a) ->
  'b option
val get_bool :
  string ->
  ([> `Assoc of (string * 'a) list | `Bool of 'b | `List of 'a list ] as 'a) ->
  'b option
val get_list :
  string ->
  ([> `Assoc of (string * 'a) list | `List of 'a list ] as 'a) ->
  'a list option
