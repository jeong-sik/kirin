(** tRPC Context

    Request context for procedures. *)

open Kirin

(** {1 Context Types} *)

(** Base context with common fields *)
type base = {
  request: Request.t;
  headers: (string * string) list;
  path: string;
}

(** Create base context from request *)
let base_of_request req = {
  request = req;
  headers = Request.headers req |> Http.Header.to_list;
  path = Request.path req;
}

(** {1 Context Helpers} *)

(** Get header from context *)
let header name ctx =
  List.assoc_opt (String.lowercase_ascii name)
    (List.map (fun (k, v) -> (String.lowercase_ascii k, v)) ctx.headers)

(** Get authorization header *)
let authorization ctx = header "authorization" ctx

(** Get bearer token *)
let bearer_token ctx =
  match authorization ctx with
  | Some auth when String.length auth > 7 &&
                   String.lowercase_ascii (String.sub auth 0 7) = "bearer " ->
    Some (String.sub auth 7 (String.length auth - 7))
  | _ -> None

(** Get content type *)
let content_type ctx = header "content-type" ctx

(** {1 Custom Context} *)

(** Extend base context with custom data *)
type 'a t = {
  base: base;
  data: 'a;
}

(** Create context with custom data *)
let create ~data req = {
  base = base_of_request req;
  data;
}

(** Map context data *)
let map f ctx = {
  base = ctx.base;
  data = f ctx.data;
}

(** Access base context *)
let base ctx = ctx.base

(** Access custom data *)
let data ctx = ctx.data

(** Access request *)
let request ctx = ctx.base.request

(** {1 Context Factory} *)

(** Context factory type *)
type 'a factory = Request.t -> 'a t

(** Create a simple context factory *)
let factory (create_data : Request.t -> 'a) : 'a factory =
  fun req -> create ~data:(create_data req) req

(** No custom data context *)
let unit_factory : unit factory =
  fun req -> create ~data:() req

(** {1 Middleware Integration} *)

(** Context storage using first-class modules *)
module type CONTEXT = sig
  type data
  val get : unit -> data t option
  val set : data t -> unit
end

(** Create a context store for a specific data type *)
let make_store (type a) () : (module CONTEXT with type data = a) =
  let module Store = struct
    type data = a
    let storage : a t option ref = ref None
    let get () = !storage
    let set ctx = storage := Some ctx
  end in
  (module Store)
