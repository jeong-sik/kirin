type client_directive =
  | ClientLoad
  | ClientIdle
  | ClientVisible
  | ClientMedia of string
  | ClientOnly of string

type framework =
  | React
  | Vue
  | Svelte
  | Solid
  | Preact
  | Lit
  | Alpine
  | Custom of string

type t =
  { id : string
  ; component : string
  ; framework : framework
  ; directive : client_directive
  ; props : (string * Yojson.Safe.t) list
  ; slots : (string * string) list
  }

val next_id : int ref
val gen_id : unit -> string

val create
  :  component:string
  -> framework:framework
  -> ?directive:client_directive
  -> ?props:(string * Yojson.Safe.t) list
  -> ?slots:(string * string) list
  -> unit
  -> t

val react
  :  component:string
  -> ?directive:client_directive
  -> ?props:(string * Yojson.Safe.t) list
  -> ?slots:(string * string) list
  -> unit
  -> t

val vue
  :  component:string
  -> ?directive:client_directive
  -> ?props:(string * Yojson.Safe.t) list
  -> ?slots:(string * string) list
  -> unit
  -> t

val svelte
  :  component:string
  -> ?directive:client_directive
  -> ?props:(string * Yojson.Safe.t) list
  -> ?slots:(string * string) list
  -> unit
  -> t

val solid
  :  component:string
  -> ?directive:client_directive
  -> ?props:(string * Yojson.Safe.t) list
  -> ?slots:(string * string) list
  -> unit
  -> t

val preact
  :  component:string
  -> ?directive:client_directive
  -> ?props:(string * Yojson.Safe.t) list
  -> ?slots:(string * string) list
  -> unit
  -> t

val load : t -> t
val idle : t -> t
val visible : t -> t
val media : string -> t -> t
val only : string -> t -> t
val framework_to_string : framework -> string
val framework_of_string : string -> framework
val directive_to_string : client_directive -> string
val directive_of_string : string -> client_directive

val to_json
  :  t
  -> [> `Assoc of
          (string * [> `Assoc of (string * Yojson.Safe.t) list | `String of string ]) list
     ]

val render_wrapper : t -> string -> string
val render_script : t -> string
val hydration_script : unit -> string
