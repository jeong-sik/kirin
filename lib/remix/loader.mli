type loader_context =
  { request_url : string
  ; request_method : string
  ; params : (string * string) list
  ; headers : (string * string) list
  ; cookies : (string * string) list
  }

type 'a loader_result =
  | Data of 'a
  | Redirect of string * int
  | NotFound
  | ServerError of string

type json_result =
  { data : Yojson.Safe.t option
  ; redirect : (string * int) option
  ; error : string option
  ; status : int
  }

type 'a loader = loader_context -> 'a loader_result

val context_of_request : Kirin.Request.t -> loader_context
val with_params : (string * string) list -> loader_context -> loader_context
val param : loader_context -> string -> string option
val header : loader_context -> string -> string option
val map : ('a -> 'b) -> 'a loader_result -> 'b loader_result
val bind : ('a -> 'b loader_result) -> 'a loader_result -> 'b loader_result

val parallel
  :  ('a -> 'b loader_result)
  -> ('a -> 'c loader_result)
  -> 'a
  -> ('b * 'c) loader_result

val optional : ('a -> 'b loader_result) -> 'a -> 'b option loader_result
val pure : 'a -> 'b -> 'a loader_result
val redirect : ?status:int -> string -> 'a -> 'b loader_result
val not_found : 'a -> 'b loader_result
val error : string -> 'a -> 'b loader_result

val require_auth
  :  check_auth:('a -> bool)
  -> login_url:string
  -> ('a -> 'b loader_result)
  -> 'a
  -> 'b loader_result

val to_json_result : Yojson.Safe.t loader_result -> json_result
val json_result_to_yojson : json_result -> [> `Assoc of (string * Yojson.Safe.t) list ]
val run_loader : ('a -> Yojson.Safe.t loader_result) -> 'a -> Kirin.Response.t

val endpoint
  :  (loader_context -> Yojson.Safe.t loader_result)
  -> Kirin.Request.t
  -> Kirin.Response.t

type 'a deferred =
  | Resolved of 'a
  | Pending of (unit -> 'a loader_result)

val defer : (unit -> 'a loader_result) -> 'a deferred
val resolve : 'a deferred -> 'a loader_result

val context_to_json
  :  loader_context
  -> [> `Assoc of
          (string
          * [> `Assoc of (string * [> `String of string ]) list | `String of string ])
            list
     ]
