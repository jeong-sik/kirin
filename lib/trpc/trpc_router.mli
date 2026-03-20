type 'ctx boxed_procedure = {
  path : string;
  proc_type : Procedure.procedure_type;
  meta : Procedure.meta;
  execute : 'ctx -> Yojson.Safe.t -> (Yojson.Safe.t, string) result;
  info : Procedure.procedure_info;
}
val box_procedure :
  ('ctx, 'input, 'output) Procedure.t ->
  input_type:Procedure.type_info ->
  output_type:Procedure.type_info -> 'ctx boxed_procedure
type 'ctx t = {
  prefix : string;
  procedures : 'ctx boxed_procedure list;
  children : (string * 'ctx t) list;
}
val create : ?prefix:string -> unit -> 'ctx t
val get_prefix : 'a t -> string
val add_procedure : 'a boxed_procedure -> 'a t -> 'a t
val query :
  path:string ->
  ?meta:Procedure.meta ->
  input:'a Procedure.validator ->
  output:'b Procedure.serializer ->
  input_type:Procedure.type_info ->
  output_type:Procedure.type_info ->
  ('c -> 'a -> ('b, string) result) -> 'c t -> 'c t
val mutation :
  path:string ->
  ?meta:Procedure.meta ->
  input:'a Procedure.validator ->
  output:'b Procedure.serializer ->
  input_type:Procedure.type_info ->
  output_type:Procedure.type_info ->
  ('c -> 'a -> ('b, string) result) -> 'c t -> 'c t
val subscription :
  path:string ->
  ?meta:Procedure.meta ->
  input:'a Procedure.validator ->
  output:'b Procedure.serializer ->
  input_type:Procedure.type_info ->
  output_type:Procedure.type_info ->
  ('c -> 'a -> ('b, string) result) -> 'c t -> 'c t
val merge : prefix:string -> 'a t -> 'a t -> 'a t
val find_procedure : string -> 'a t -> 'a boxed_procedure option
val all_procedures : ?prefix:string -> 'a t -> 'a boxed_procedure list
val all_procedure_infos : 'a t -> Procedure.procedure_info list
val execute_procedure :
  string -> 'a -> Yojson.Safe.t -> 'a t -> (Yojson.Safe.t, string) result
val paths : 'a t -> string list
val queries : 'a t -> 'a boxed_procedure list
val mutations : 'a t -> 'a boxed_procedure list
val subscriptions : 'a t -> 'a boxed_procedure list
