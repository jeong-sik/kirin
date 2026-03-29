type 'a state =
  | Active of 'a
  | Completed
  | Error of string

type message =
  | Data of Yojson.Safe.t
  | Complete
  | Err of
      { code : int
      ; message : string
      }

type sub_id = string

val generate_id : unit -> string

val encode_message
  :  id:'a
  -> message
  -> [> `Assoc of
          (string * [> `Assoc of (string * Yojson.Safe.t) list | `String of 'a ]) list
     ]

type request =
  { id : sub_id
  ; method_ : string
  ; params : Yojson.Safe.t
  }

val parse_request : [> `Assoc of (string * Yojson.Safe.t) list ] -> request option

module Registry : sig
  type cancel_fn = unit -> unit

  type t =
    { mutable subscriptions : (sub_id * cancel_fn) list
    ; mutex : Eio.Mutex.t
    }

  val create : unit -> t
  val add : t -> id:sub_id -> cancel:cancel_fn -> unit
  val remove : t -> sub_id -> bool
  val cancel_all : t -> unit
  val count : t -> int
  val has : t -> sub_id -> bool
end

val sse_event : event:string -> data:string -> string
val sse_message : Yojson.Safe.t -> string

module WsMessage : sig
  type t =
    | Subscribe of
        { id : sub_id
        ; path : string
        ; input : Yojson.Safe.t
        }
    | Unsubscribe of { id : sub_id }
    | Ping
    | Pong

  val parse
    :  [> `Assoc of
            (string
            * [> `Assoc of (string * Yojson.Safe.t) list
              | `Int of int
              | `Null
              | `String of string
              ])
              list
       ]
    -> t option

  val encode
    :  t
    -> [> `Assoc of
            (string * [> `Assoc of (string * Yojson.Safe.t) list | `String of sub_id ])
              list
       ]
end

val interval : ms:int -> f:(unit -> unit) -> (unit -> unit) * (unit -> unit)
val of_list : delay_ms:int -> Yojson.Safe.t list -> (message -> unit) -> unit
