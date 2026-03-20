val generate_type : Procedure.type_info -> string
val generate_procedure_type : Procedure.procedure_info -> string
val generate_router_type :
  Procedure.procedure_info list -> string
val generate_input_types :
  Procedure.procedure_info list -> string
val generate_output_types :
  Procedure.procedure_info list -> string
val generate_client :
  router_name:string -> Procedure.procedure_info list -> string
val generate_json_schema :
  Procedure.type_info ->
  ([> `Assoc of (string * 'a) list | `List of 'a list | `String of string ]
   as 'a)
val generate_schema :
  Procedure.procedure_info list ->
  [> `Assoc of
       (string *
        [> `Assoc of
             (string *
              [> `Assoc of
                   (string *
                    ([> `Assoc of (string * 'a) list
                      | `List of 'a list
                      | `Null
                      | `String of string ]
                     as 'a))
                   list ])
             list
         | `String of string ])
       list ]
val generate_zod : Procedure.type_info -> string
val generate_zod_schemas :
  Procedure.procedure_info list -> string
