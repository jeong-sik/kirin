type 'a loader_result = {
  data : 'a;
  headers : (string * string) list;
  status : int;
}
val ok :
  ?headers:(string * string) list -> ?status:int -> 'a -> 'a loader_result
val param :
  string -> 'a Route_def.loader_context -> string option
val param_exn :
  string -> 'a Route_def.loader_context -> string
val param_int :
  string -> 'a Route_def.loader_context -> int option
val search_param :
  string -> 'a Route_def.loader_context -> string option
val search_params_all :
  string -> 'a Route_def.loader_context -> string list
val parallel : ('a -> 'b) list -> 'a -> 'b list
val sequential : ('a -> ('b, 'c) result) list -> 'a -> ('b list, 'c) result
type cache_key = string
val cache_key : 'a Route_def.loader_context -> string
val redirect : ?status:int -> string -> ('a, string) result
val parse_redirect : string -> (int * string) option
val not_found : unit -> ('a, string) result
val is_not_found : string -> bool
val unauthorized : unit -> ('a, string) result
val is_unauthorized : string -> bool
type ('ctx, 'a, 'b) middleware =
    ('ctx, 'a) Route_def.loader ->
    ('ctx, 'b) Route_def.loader
val with_auth :
  ('ctx -> bool) ->
  ('ctx, 'a) Route_def.loader ->
  ('ctx, 'a) Route_def.loader
val with_logging :
  (string -> unit) ->
  ('ctx, 'a) Route_def.loader ->
  ('ctx, 'a) Route_def.loader
val with_timing :
  (float -> unit) ->
  ('ctx, 'a) Route_def.loader ->
  ('ctx, 'a) Route_def.loader
