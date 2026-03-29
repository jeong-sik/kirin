val use : string list -> string * string
val use_one : 'a -> string * 'a

module Preload : sig
  val enable : string * string
  val preload_on_hover : string * string
  val preload_init : string * string
  val preload_images : string * string
end

module Ws : sig
  val enable : string * string
  val connect : 'a -> string * 'a
  val send : string * string
  val setup : string -> (string * string) list
end

module Sse : sig
  val enable : string * string
  val connect : 'a -> string * 'a
  val swap : 'a -> string * 'a
  val setup : string -> string -> (string * string) list
end

module ResponseTargets : sig
  val enable : string * string
  val target_status : int -> 'a -> string * 'a
  val target_4xx : 'a -> string * 'a
  val target_5xx : 'a -> string * 'a
  val target_error : 'a -> string * 'a
  val error_handling : error_container:string -> (string * string) list
end

module LoadingStates : sig
  val enable : string * string
  val add_class : 'a -> string * 'a
  val remove_class : 'a -> string * 'a
  val disable : string * string
  val show : string * string
  val aria_busy : string * string
  val delay : int -> string * string
  val with_spinner : class_:string -> delay_ms:int -> (string * string) list
end

module ClassTools : sig
  val enable : string * string
  val classes_add : string -> string -> string * string
  val classes_remove : string -> string -> string * string
  val classes_toggle : string -> string -> string * string
  val classes : (string * string * string) list -> string * string
end

module JsonEnc : sig
  val enable : string * string
end

module PathDeps : sig
  val enable : string * string
  val path_deps : string list -> string * string
end

module Restored : sig
  val enable : string * string
end

module MultiSwap : sig
  val enable : string * string
end

module Morphdom : sig
  val enable : string * string
end

module AlpineMorph : sig
  val enable : string * string
end

module Debug : sig
  val enable : string * string
end

val combine : 'a list list -> 'a list
val attrs_to_string : (string * string) list -> string
