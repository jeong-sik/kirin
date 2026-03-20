(** Simple HTML Template Engine (Mustache-like syntax)

    Supports variable interpolation, conditionals, iteration,
    partials, and HTML escaping.

    @since 1.0.0
    @status stable *)

(** {1 Types} *)

(** Template context (Yojson for flexibility) *)
type context = Yojson.Safe.t

(** Template AST node *)
type node =
    Text of string
  | Var of string * bool
  | If of string * node list * node list option
  | Unless of string * node list
  | Each of string * node list
  | Partial of string
  | Comment

(** Partial resolver function *)
type partials = string -> string option

(** {1 Context Builders} *)

val empty_context : [> `Assoc of 'a list ]
val context : ('a * 'b) list -> [> `Assoc of ('a * [> `String of 'b ]) list ]
val context_of : 'a -> [> `Assoc of 'a ]

(** {1 Context Operations} *)

val lookup : ([> `Assoc of (string * 'a) list ] as 'a) -> string -> 'a option
val value_to_string :
  [< `Assoc of 'a
   | `Bool of bool
   | `Float of float
   | `Int of int
   | `List of 'b
   | `Null
   | `String of string ] ->
  string
val is_truthy :
  [> `Bool of bool
   | `Int of int
   | `List of 'a list
   | `Null
   | `String of string ] ->
  bool

(** {1 HTML} *)

val html_escape : string -> string

(** {1 Parsing} *)

val parse : string -> node list

(** {1 Rendering} *)

val no_partials : 'a -> 'b option

val render :
  ?partials:(string -> string option) ->
  ([< `Assoc of (string * 'a) list
    | `Bool of bool
    | `Float of float
    | `Int of int
    | `List of 'a list
    | `Null
    | `String of string
    > `Assoc `Bool `Int `List `Null `String ]
   as 'a) ->
  string -> string

val render_nodes :
  ?partials:(string -> string option) ->
  ([< `Assoc of (string * 'a) list
    | `Bool of bool
    | `Float of float
    | `Int of int
    | `List of 'a list
    | `Null
    | `String of string
    > `Assoc `Bool `Int `List `Null `String ]
   as 'a) ->
  node list -> string

val render_node :
  ?partials:(string -> string option) ->
  ([< `Assoc of (string * 'a) list
    | `Bool of bool
    | `Float of float
    | `Int of int
    | `List of 'a list
    | `Null
    | `String of string
    > `Assoc `Bool `Int `List `Null `String ]
   as 'a) ->
  node -> string

(** {1 Response Helpers} *)

val html :
  ?partials:(string -> string option) ->
  ([< `Assoc of (string * 'a) list
    | `Bool of bool
    | `Float of float
    | `Int of int
    | `List of 'a list
    | `Null
    | `String of string
    > `Assoc `Bool `Int `List `Null `String ]
   as 'a) ->
  string -> Response.t

(** {1 Utilities} *)

val from_string : 'a -> 'a
val interpolate : string -> (string * string) list -> string
