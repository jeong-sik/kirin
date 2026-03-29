type action_context =
  { url : string
  ; request_headers : (string * string) list
  ; cookies : (string * string) list
  ; locals : Yojson.Safe.t
  ; route_id : string
  }

type file_data =
  { filename : string
  ; content_type : string
  ; data : string
  }

type field_value =
  | Text of string
  | File of file_data
  | Multiple of field_value list

type form_data = (string * field_value) list

val get_text : ('a * field_value) list -> 'a -> string option
val get_text_exn : (string * field_value) list -> string -> string
val get_all : ('a * field_value) list -> 'a -> string list
val get_file : ('a * field_value) list -> 'a -> file_data option

type action_output =
  | ActionSuccess of Yojson.Safe.t
  | ActionFail of int * Yojson.Safe.t
  | ActionRedirect of int * string
  | ActionError of int * string

val success : ?data:Yojson.Safe.t -> unit -> action_output
val fail : ?status:int -> Yojson.Safe.t -> action_output
val redirect : ?status:int -> string -> action_output
val error : int -> string -> action_output

type validation_error =
  { field : string
  ; message : string
  }

type 'a validation_result =
  | Valid of 'a
  | Invalid of validation_error list

val validation_error : string -> string -> validation_error
val required : (string * field_value) list -> string -> string validation_result
val email : (string * field_value) list -> string -> string validation_result
val min_length : int -> (string * field_value) list -> string -> string validation_result
val combine_validations : 'a validation_result list -> 'a list validation_result

val errors_to_json
  :  validation_error list
  -> [> `Assoc of (string * [> `String of string ]) list ]

val fail_validation : validation_error list -> action_output

type action_handler = action_context -> form_data -> action_output

type named_action =
  { name : string option
  ; handler : action_handler
  }

val default : action_handler -> named_action
val named : string -> action_handler -> named_action
val find_action : named_action list -> string option -> named_action option

val create_context
  :  url:string
  -> ?headers:(string * string) list
  -> ?cookies:(string * string) list
  -> ?locals:Yojson.Safe.t
  -> route_id:string
  -> unit
  -> action_context

val cookie : action_context -> string -> string option
val header : action_context -> string -> string option
val output_to_json : action_output -> [> `Assoc of (string * Yojson.Safe.t) list ]

val form_to_json
  :  ('a * field_value) list
  -> [> `Assoc of
          ('a
          * ([> `Assoc of (string * [> `String of string ]) list
             | `List of 'b list
             | `String of string
             ]
             as
             'b))
            list
     ]
