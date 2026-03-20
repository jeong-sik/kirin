type cache_options = {
  include_headers : string list;
  include_post_requests : bool;
  include_request_with_body : bool;
  filter : (string -> bool) option;
}
val default_cache_options : cache_options
type entry = {
  key : string;
  value : Yojson.Safe.t;
  timestamp : float;
  ttl : float option;
}
type t = { mutable entries : entry list; options : cache_options; }
val create : ?options:cache_options -> unit -> t
val make_http_key :
  method_:string ->
  url:string -> ?params:(string * string) list -> unit -> string
val set : t -> string -> Yojson.Safe.t -> unit
val set_with_ttl : t -> string -> Yojson.Safe.t -> float -> unit
val get : t -> string -> Yojson.Safe.t option
val has_key : t -> string -> bool
val remove : t -> string -> unit
val clear : t -> unit
val keys : t -> string list
val cache_response :
  t ->
  method_:string ->
  url:string -> ?params:(string * string) list -> Yojson.Safe.t -> unit
val get_cached_response :
  t ->
  method_:string ->
  url:string ->
  ?params:(string * string) list -> unit -> Yojson.Safe.t option
val entry_to_json : entry -> [> `Assoc of (string * Yojson.Safe.t) list ]
val to_json :
  t ->
  [> `Assoc of
       (string *
        [> `List of [> `Assoc of (string * Yojson.Safe.t) list ] list ])
       list ]
val serialize : t -> string
val script_tag : t -> string
val entry_from_json : Yojson__Safe.t -> entry
val from_json : Yojson__Safe.t -> t
