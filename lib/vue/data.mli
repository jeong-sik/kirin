val escape_for_html : string -> string
val serialize : Yojson.Safe.t -> string
val payload_var : string
val state_var : string
type payload = {
  server_rendered : bool;
  data : (string * Yojson.Safe.t) list;
  state : (string * Yojson.Safe.t) list;
  config : Yojson.Safe.t option;
  error : Yojson.Safe.t option;
  route_path : string;
}
val empty_payload : route_path:string -> payload
val with_data : string -> Yojson.Safe.t -> payload -> payload
val with_state : string -> Yojson.Safe.t -> payload -> payload
val with_config : Yojson.Safe.t -> payload -> payload
val with_error : Yojson.Safe.t -> payload -> payload
val payload_to_json : payload -> [> `Assoc of (string * Yojson.Safe.t) list ]
val payload_script : ?nonce:string option -> payload -> string
type async_entry = {
  key : string;
  data : Yojson.Safe.t;
  pending : bool;
  error : string option;
}
val async_entry : key:string -> Yojson.Safe.t -> async_entry
val async_pending : key:string -> async_entry
val async_error : key:string -> string -> async_entry
val async_to_json :
  async_entry -> [> `Assoc of (string * Yojson.Safe.t) list ]
type island_data = {
  island_id : string;
  component : string;
  props : Yojson.Safe.t;
  priority : string;
}
val island :
  id:string ->
  component:string ->
  ?props:Yojson.Safe.t -> ?priority:string -> unit -> island_data
val island_to_json :
  island_data -> [> `Assoc of (string * Yojson.Safe.t) list ]
val island_script : island_data list -> string
type route_data = {
  route_id : string;
  params : (string * string) list;
  query : (string * string) list;
  matched : string list;
}
val route_data :
  route_id:string ->
  ?params:(string * string) list ->
  ?query:(string * string) list -> ?matched:string list -> unit -> route_data
val route_data_to_json :
  route_data ->
  [> `Assoc of
       (string *
        [> `Assoc of (string * [> `String of string ]) list
         | `List of [> `String of string ] list
         | `String of string ])
       list ]
type page_data = {
  payload : payload;
  route : route_data;
  islands : island_data list;
  async_data : async_entry list;
}
val page_data :
  payload:payload ->
  route:route_data ->
  ?islands:island_data list ->
  ?async_data:async_entry list -> unit -> page_data
val all_scripts : ?nonce:string option -> page_data -> string
