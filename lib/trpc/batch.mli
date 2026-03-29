type call =
  { id : int
  ; path : string
  ; input : Yojson.Safe.t
  }

val parse_call : [> `Assoc of (string * Yojson.Safe.t) list ] -> call option

type response =
  { id : int
  ; result : (Yojson.Safe.t, error) result
  }

and error =
  { code : int
  ; message : string
  ; data : Yojson.Safe.t option
  }

module ErrorCode : sig
  val parse_error : int
  val invalid_request : int
  val method_not_found : int
  val invalid_params : int
  val internal_error : int
end

val success : id:int -> Yojson.Safe.t -> response
val error : id:int -> ?code:int -> ?data:Yojson.Safe.t -> string -> response
val response_to_json : response -> [> `Assoc of (string * Yojson.Safe.t) list ]

val parse_batch
  :  [> `Assoc of (string * Yojson.Safe.t) list
     | `List of [> `Assoc of (string * Yojson.Safe.t) list ] list
     ]
  -> call list

val is_batch : [> `List of 'a ] -> bool

val process_batch
  :  execute:(string -> 'a -> Yojson.Safe.t -> (Yojson.Safe.t, string) result)
  -> 'a
  -> call list
  -> response list

val batch_response_to_json
  :  response list
  -> [> `Assoc of (string * Yojson.Safe.t) list
     | `List of [> `Assoc of (string * Yojson.Safe.t) list ] list
     ]

val create_batch_request
  :  ('a * 'b * ([> `Int of 'a | `String of 'b ] as 'c)) list
  -> [> `List of [> `Assoc of (string * 'c) list ] list ]

val create_request
  :  id:'a
  -> path:'b
  -> input:([> `Int of 'a | `String of 'b ] as 'c)
  -> [> `Assoc of (string * 'c) list ]
