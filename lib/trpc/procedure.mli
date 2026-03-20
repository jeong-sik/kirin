type procedure_type = Query | Mutation | Subscription
val procedure_type_to_string : procedure_type -> string
type 'a validator = Yojson.Safe.t -> ('a, string) result
val no_input : unit validator
val json_input : Yojson.Safe.t validator
val string_input : string validator
val int_input : int validator
val bool_input : bool validator
val optional_input : 'a validator -> 'a option validator
val list_input : 'a validator -> 'a list validator
val field : string -> 'a validator -> 'a validator
val field_opt : string -> 'a validator -> 'a option validator
type 'a serializer = 'a -> Yojson.Safe.t
val unit_output : unit serializer
val json_output : Yojson.Safe.t serializer
val string_output : string serializer
val int_output : int serializer
val bool_output : bool serializer
val list_output : 'a serializer -> 'a list serializer
val option_output : 'a serializer -> 'a option serializer
type meta = {
  description : string option;
  deprecated : bool;
  tags : string list;
}
val default_meta : meta
type ('ctx, 'input, 'output) t = {
  proc_type : procedure_type;
  path : string;
  meta : meta;
  validate : 'input validator;
  serialize : 'output serializer;
  handler : 'ctx -> 'input -> ('output, string) result;
}
val query :
  path:string ->
  ?meta:meta ->
  input:'a validator ->
  output:'b serializer -> ('c -> 'a -> ('b, string) result) -> ('c, 'a, 'b) t
val mutation :
  path:string ->
  ?meta:meta ->
  input:'a validator ->
  output:'b serializer -> ('c -> 'a -> ('b, string) result) -> ('c, 'a, 'b) t
val subscription :
  path:string ->
  ?meta:meta ->
  input:'a validator ->
  output:'b serializer -> ('c -> 'a -> ('b, string) result) -> ('c, 'a, 'b) t
val execute :
  ('ctx, 'input, 'output) t ->
  'ctx -> Yojson.Safe.t -> (Yojson.Safe.t, string) result
type type_info =
    TString
  | TInt
  | TBool
  | TNull
  | TArray of type_info
  | TObject of (string * type_info) list
  | TOptional of type_info
  | TUnion of type_info list
  | TCustom of string
val type_info_to_typescript : type_info -> string
type procedure_info = {
  name : string;
  proc_type : procedure_type;
  input_type : type_info;
  output_type : type_info;
  description : string option;
}
