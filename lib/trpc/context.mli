type base = {
  request : Kirin.Request.t;
  headers : (string * string) list;
  path : string;
}
val base_of_request : Kirin.Request.t -> base
val header : string -> base -> string option
val authorization : base -> string option
val bearer_token : base -> string option
val content_type : base -> string option
val base_for_test :
  ?headers:(string * string) list -> ?path:string -> unit -> base
type 'a t = { base : base; data : 'a; }
val create : data:'a -> Kirin.Request.t -> 'a t
val map : ('a -> 'b) -> 'a t -> 'b t
val base : 'a t -> base
val data : 'a t -> 'a
val request : 'a t -> Kirin.Request.t
type 'a factory = Kirin.Request.t -> 'a t
val factory : (Kirin.Request.t -> 'a) -> 'a factory
val unit_factory : unit factory
module type CONTEXT =
  sig type data val get : unit -> data t option val set : data t -> unit end
val make_store : unit -> (module CONTEXT with type data = 'a)
