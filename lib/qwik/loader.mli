type 'a result =
    LoaderOk of 'a
  | LoaderRedirect of string
  | LoaderError of { status : int; message : string; }
  | LoaderNotFound
type context = {
  url : string;
  params : (string * string) list;
  query : (string * string) list;
  headers : (string * string) list;
  cookie : (string * string) list;
  platform : platform_context;
}
and platform_context = { env : (string * string) list; }
type 'a t = { name : string; loader : context -> 'a result; }
val create : name:string -> (context -> 'a result) -> 'a t
val data : name:string -> (context -> 'a) -> 'a t
val validated :
  name:string ->
  validate:(context -> string option) -> (context -> 'a) -> 'a t
val execute : 'a t -> context -> 'a result
val execute_parallel : 'a t list -> context -> (string * 'a result) list
val context_of_request :
  url:string ->
  params:(string * string) list ->
  query:(string * string) list ->
  headers:(string * string) list ->
  cookies:(string * string) list -> unit -> context
val get_param : context -> string -> string option
val get_query : context -> string -> string option
val get_header : context -> string -> string option
val get_cookie : context -> string -> string option
val map : ('a -> 'b) -> 'a result -> 'b result
val bind : ('a -> 'b result) -> 'a result -> 'b result
val redirect : string -> 'a result
val error : status:int -> message:string -> 'a result
val not_found : 'a result
val result_to_json :
  ('a -> ([> `Int of int | `String of string ] as 'b)) ->
  'a result -> [> `Assoc of (string * 'b) list ]
val context_to_json :
  context ->
  [> `Assoc of
       (string *
        [> `Assoc of (string * [> `String of string ]) list
         | `String of string ])
       list ]
val to_qrl : chunk:string -> symbol:string -> 'a t -> Qrl.t
type 'a loader_qrl = { loader : 'a t; qrl : Qrl.t; }
val with_qrl : chunk:string -> symbol:string -> 'a t -> 'a loader_qrl
