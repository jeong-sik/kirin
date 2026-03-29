val escape_for_html : string -> string
val serialize : Yojson.Safe.t -> string
val data_var : string
val script_tag : ?nonce:string option -> Yojson.Safe.t -> string

type route_data =
  { route_id : string
  ; data : Yojson.Safe.t
  ; uses : string list
  }

val route_data : route_id:string -> ?uses:string list -> Yojson.Safe.t -> route_data

type page_data =
  { url : string
  ; params : (string * string) list
  ; route_id : string option
  ; status : int
  ; error : string option
  ; data : Yojson.Safe.t
  ; form : Yojson.Safe.t option
  }

val page_data
  :  url:string
  -> params:(string * string) list
  -> ?route_id:string
  -> ?status:int
  -> ?error:string
  -> ?data:Yojson.Safe.t
  -> ?form:Yojson.Safe.t
  -> unit
  -> page_data

val page_data_to_json : page_data -> [> `Assoc of (string * Yojson.Safe.t) list ]

type initial_data =
  { type_ : string
  ; nodes : Yojson.Safe.t list
  ; page_data : page_data option
  }

val empty_initial_data : initial_data

val create_initial_data
  :  ?nodes:Yojson.Safe.t list
  -> ?page_data:page_data
  -> unit
  -> initial_data

val redirect_data : location:string -> status:int -> initial_data

val initial_data_to_json
  :  initial_data
  -> [> `Assoc of
          (string
          * [> `Assoc of (string * Yojson.Safe.t) list
            | `List of Yojson.Safe.t list
            | `String of string
            ])
            list
     ]

val all_scripts : ?nonce:string option -> initial_data -> string

type deferred_chunk =
  { chunk_id : string
  ; data : Yojson.Safe.t
  }

val deferred_script : deferred_chunk -> string

type action_data =
  { success : bool
  ; action_data : Yojson.Safe.t
  }

val action_data : ?success:bool -> Yojson.Safe.t -> action_data
val action_data_to_json : action_data -> [> `Assoc of (string * Yojson.Safe.t) list ]

type snapshot =
  { scroll_y : int
  ; snapshot_data : Yojson.Safe.t option
  }

val snapshot_to_json : snapshot -> [> `Assoc of (string * Yojson.Safe.t) list ]
