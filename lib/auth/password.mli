val default_iterations : int
val salt_length : int
val hash_length : int
val generate_salt : unit -> string
val bytes_to_hex : string -> string
val hex_to_bytes : string -> string
val pbkdf2 : password:string -> salt:String.t -> iterations:int -> dk_len:int -> string
val constant_time_compare : string -> string -> bool

type hash_result = string

val hash : ?iterations:int -> string -> string
val verify : string -> string -> bool
val needs_rehash : ?iterations:int -> string -> bool

type strength =
  | Weak
  | Fair
  | Good
  | Strong

val check_strength : string -> strength
val strength_to_string : strength -> string
