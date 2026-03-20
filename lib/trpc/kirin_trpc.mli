module Procedure = Procedure
module Router = Trpc_router
module Context = Context
module Batch = Batch
module Handler = Handler
module Subscription = Subscription
module Codegen = Codegen
type procedure_type =
  Procedure.procedure_type =
    Query
  | Mutation
  | Subscription
type type_info =
  Procedure.type_info =
    TString
  | TInt
  | TBool
  | TNull
  | TArray of type_info
  | TObject of (string * type_info) list
  | TOptional of type_info
  | TUnion of type_info list
  | TCustom of string
val query :
  path:string ->
  ?meta:Procedure.meta ->
  input:'a Procedure.validator ->
  output:'b Procedure.serializer ->
  input_type:Procedure.type_info ->
  output_type:Procedure.type_info ->
  ('c -> 'a -> ('b, string) result) ->
  'c Trpc_router.t -> 'c Trpc_router.t
val mutation :
  path:string ->
  ?meta:Procedure.meta ->
  input:'a Procedure.validator ->
  output:'b Procedure.serializer ->
  input_type:Procedure.type_info ->
  output_type:Procedure.type_info ->
  ('c -> 'a -> ('b, string) result) ->
  'c Trpc_router.t -> 'c Trpc_router.t
val routes :
  ?config:Handler.config ->
  ctx_factory:(Kirin.Request.t -> 'a) ->
  'a Trpc_router.t -> Kirin.Router.route list
val generate_client : name:string -> 'a Trpc_router.t -> string
val generate_schema :
  'a Trpc_router.t ->
  [> `Assoc of
       (string *
        [> `Assoc of
             (string *
              [> `Assoc of
                   (string *
                    ([> `Assoc of (string * 'b) list
                      | `List of 'b list
                      | `Null
                      | `String of string ]
                     as 'b))
                   list ])
             list
         | `String of string ])
       list ]
val generate_zod : 'a Trpc_router.t -> string
val no_input : unit Procedure.validator
val string_input : string Procedure.validator
val int_input : int Procedure.validator
val bool_input : bool Procedure.validator
val json_input : Yojson.Safe.t Procedure.validator
val optional_input : 'a Procedure.validator -> 'a option Procedure.validator
val list_input : 'a Procedure.validator -> 'a list Procedure.validator
val field : string -> 'a Procedure.validator -> 'a Procedure.validator
val field_opt :
  string -> 'a Procedure.validator -> 'a option Procedure.validator
val unit_output : unit Procedure.serializer
val string_output : string Procedure.serializer
val int_output : int Procedure.serializer
val bool_output : bool Procedure.serializer
val json_output : Yojson.Safe.t Procedure.serializer
val list_output : 'a Procedure.serializer -> 'a list Procedure.serializer
val option_output : 'a Procedure.serializer -> 'a option Procedure.serializer
