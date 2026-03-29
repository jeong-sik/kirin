type endpoint =
  { meth : Http.Method.t option
  ; path : string
  ; response : Testing_response.t
  ; mutable call_count : int
  }

type t =
  { mutable endpoints : endpoint list
  ; mutable unmatched_requests : Testing_request.t list
  }

val create : unit -> t

val on
  :  ?meth:Http.Method.t option
  -> path:string
  -> status:Http.Status.t
  -> ?headers:(string * string) list
  -> ?body:string
  -> t
  -> t

val on_json
  :  ?meth:Http.Method.t option
  -> path:string
  -> status:Http.Status.t
  -> ?headers:(string * string) list
  -> Yojson.Safe.t
  -> t
  -> t

val handle : t -> Testing_request.t -> Testing_response.t
val call_count : path:string -> t -> int
val unmatched : t -> Testing_request.t list
val reset : t -> unit
val assert_called : path:string -> t -> unit
val assert_called_times : path:string -> times:int -> t -> unit
val assert_no_unmatched : t -> unit
