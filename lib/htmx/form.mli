val post : 'a -> string * 'a
val put : 'a -> string * 'a
val patch : 'a -> string * 'a
val delete : 'a -> string * 'a
val target : 'a -> string * 'a
val swap : 'a -> string * 'a
val include_ : 'a -> string * 'a
val vals : 'a -> string * 'a
val disable_during : string * string
val indicator : 'a -> string * 'a
val trigger : 'a -> string * 'a
val trigger_submit : string * string
val trigger_with_mods : string -> string list -> string * string
val trigger_debounce : string -> int -> string * string
val trigger_throttle : string -> int -> string * string
val trigger_changed : string -> string * string
val required : string * string
val pattern : 'a -> string * 'a
val minlength : int -> string * string
val maxlength : int -> string * string
val min : int -> string * string
val max : int -> string * string
val step : int -> string * string
type input_type =
    Text
  | Password
  | Email
  | Number
  | Tel
  | Url
  | Date
  | Time
  | Datetime_local
  | Month
  | Week
  | Color
  | Range
  | File
  | Hidden
  | Search
  | Checkbox
  | Radio
val input_type_to_string : input_type -> string
val input :
  type_:input_type ->
  name:string ->
  ?id:string ->
  ?value:string ->
  ?placeholder:string -> ?class_:string -> (string * string) list -> string
val text_input :
  name:string ->
  ?id:string ->
  ?value:string ->
  ?placeholder:string -> ?class_:string -> (string * string) list -> string
val password_input :
  name:string ->
  ?id:string ->
  ?placeholder:string -> ?class_:string -> (string * string) list -> string
val email_input :
  name:string ->
  ?id:string ->
  ?value:string ->
  ?placeholder:string -> ?class_:string -> (string * string) list -> string
val number_input :
  name:string ->
  ?id:string ->
  ?value:string ->
  ?placeholder:string -> ?class_:string -> (string * string) list -> string
val hidden_input : name:string -> value:string -> string
val textarea :
  name:string ->
  ?id:string ->
  ?rows:int ->
  ?cols:int ->
  ?placeholder:string ->
  ?class_:string -> ?content:string -> (string * string) list -> string
val option : value:string -> ?selected:bool -> string -> string
val select :
  name:string ->
  ?id:string ->
  ?class_:string -> string list -> (string * string) list -> string
val submit : ?class_:string -> ?disabled:bool -> string -> string
val submit_with_loading :
  label:string -> loading_label:string -> ?class_:string -> unit -> string
val form :
  action:string ->
  ?method_:[< `Delete | `Patch | `Post | `Put > `Post ] ->
  ?id:string ->
  ?class_:string ->
  ?target:string -> ?swap:string -> ?indicator:string -> string -> string
val field : label:string -> name:string -> ?error:string -> string -> string
val validate_on_blur : url:string -> string * string
