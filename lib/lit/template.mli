type expression =
  | Text of string
  | Property of string
  | Computed of string
  | Conditional of
      { condition : string
      ; then_ : t
      ; else_ : t option
      }
  | Loop of
      { items : string
      ; item : string
      ; index : string option
      ; body : t
      }
  | Slot of
      { name : string option
      ; fallback : string option
      }
  | Event of
      { event_name : string
      ; handler : string
      ; options : event_options
      }
  | Directive of
      { directive_name : string
      ; args : string list
      }

and event_options =
  { capture : bool
  ; once : bool
  ; passive : bool
  }

and t = { parts : expression list }

val empty : t
val create : expression list -> t
val text : string -> t
val prop : string -> t
val computed : string -> t
val default_event_options : event_options
val event : ?options:event_options -> string -> string -> t
val slot : ?name:string -> ?fallback:string -> unit -> t
val when_ : string -> then_:t -> ?else_:t -> unit -> t
val repeat : items:string -> item:string -> ?index:string -> t -> t
val directive : string -> string list -> t
val concat : t list -> t
val class_map : (string * string) list -> t
val style_map : (string * string) list -> t
val if_defined : string -> t
val live : string -> t
val ref_directive : string -> t
val until : string -> string -> t
val render_expression : expression -> string
val render_to_lit : t -> string
val to_html_literal : t -> string
val cache : t -> t
val guard : string list -> t -> t
val render_static_expression : data:(string * Yojson.Safe.t) list -> expression -> string
val render_static : data:(string * Yojson.Safe.t) list -> t -> string

val event_options_to_json
  :  event_options
  -> [> `Assoc of (string * [> `Bool of bool ]) list ]

val expression_to_json
  :  expression
  -> ([> `Assoc of
           (string
           * [> `Assoc of (string * [> `Bool of bool | `List of 'a list ]) list
             | `List of [> `String of string ] list
             | `Null
             | `String of string
             ])
             list
      ]
      as
      'a)

val to_json
  :  t
  -> ([> `Assoc of
           (string * [> `Bool of bool | `List of [> `Assoc of (string * 'a) list ] list ])
             list
      | `List of [> `String of string ] list
      | `Null
      | `String of string
      ]
      as
      'a)
