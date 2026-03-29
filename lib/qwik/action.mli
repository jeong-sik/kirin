type 'a result =
  | ActionOk of 'a
  | ActionFail of
      { status : int
      ; errors : (string * string) list
      }
  | ActionRedirect of string

type file_info =
  { name : string
  ; content_type : string
  ; size : int
  }

type field_value =
  | Text of string
  | Multiple of string list
  | File of file_info

type form_data = { fields : (string * field_value) list }

type context =
  { url : string
  ; params : (string * string) list
  ; headers : (string * string) list
  ; cookie : (string * string) list
  ; form_data : form_data
  }

type scope =
  | RouteAction
  | GlobalAction

type 'a t =
  { name : string
  ; scope : scope
  ; action : context -> 'a result
  }

val route_action : name:string -> (context -> 'a result) -> 'a t
val global_action : name:string -> (context -> 'a result) -> 'a t
val execute : 'a t -> context -> 'a result
val get_field : form_data -> string -> string option
val get_multiple : form_data -> string -> string list
val get_file : form_data -> string -> file_info option
val has_field : form_data -> string -> bool
val empty_form : form_data

type validation_result =
  | Valid
  | Invalid of (string * string) list

val required : string -> form_data -> validation_result
val email : string -> form_data -> validation_result
val min_length : int -> string -> form_data -> validation_result
val validate_all : ('a -> validation_result) list -> 'a -> validation_result

val context_of_request
  :  url:string
  -> params:(string * string) list
  -> headers:(string * string) list
  -> cookies:(string * string) list
  -> form:form_data
  -> unit
  -> context

val get_header : context -> string -> string option
val get_cookie : context -> string -> string option
val ok : 'a -> 'a result
val fail : ?status:int -> (string * string) list -> 'a result
val redirect : string -> 'a result

val field_value_to_json
  :  field_value
  -> [> `Assoc of (string * [> `Int of int | `String of string ]) list
     | `List of [> `String of string ] list
     | `String of string
     ]

val form_data_to_json
  :  form_data
  -> [> `Assoc of
          (string
          * [> `Assoc of (string * [> `Int of int | `String of string ]) list
            | `List of [> `String of string ] list
            | `String of string
            ])
            list
     ]

val result_to_json
  :  ('a
      -> ([> `Assoc of (string * [> `String of string ]) list
          | `Int of int
          | `String of string
          ]
          as
          'b))
  -> 'a result
  -> [> `Assoc of (string * 'b) list ]

val to_qrl : chunk:string -> symbol:string -> 'a t -> Qrl.t

type 'a action_qrl =
  { action : 'a t
  ; qrl : Qrl.t
  }

val with_qrl : chunk:string -> symbol:string -> 'a t -> 'a action_qrl
