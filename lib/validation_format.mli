val email_pattern : string
val uuid_pattern : string
val uri_pattern : string
val date_pattern : string
val datetime_pattern : string
val validate_format : string -> string -> (unit, string) result
val format_path : string list -> string

val error_to_json
  :  Validation_schema.error
  -> [> `Assoc of (string * [> `String of string ]) list ]

val errors_to_json
  :  Validation_schema.error list
  -> [> `Assoc of
          (string
          * [> `List of [> `Assoc of (string * [> `String of string ]) list ] list ])
            list
     ]

val error_to_string : Validation_schema.error -> string
val errors_to_string : Validation_schema.error list -> string
