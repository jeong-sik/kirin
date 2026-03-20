type render_request = {
  url : string;
  headers : (string * string) list;
  document : string option;
  providers : string list;
}
type health_request = { include_memory : bool; }
type prerender_request = { routes : string list; output_dir : string; }
type render_result = {
  html : string;
  transfer_state : Yojson.Safe.t option;
  styles : string list;
  scripts : string list;
  title : string option;
}
type health_result = {
  status : string;
  version : string;
  memory_mb : int option;
  uptime_s : float;
}
type prerender_result = {
  rendered_count : int;
  failed_routes : string list;
  output_files : string list;
}
type response_result =
    RenderOk of render_result
  | HealthOk of health_result
  | PrerenderOk of prerender_result
type request = {
  jsonrpc : string;
  id : int;
  method_name : string;
  params : Yojson.Safe.t;
}
type rpc_error = {
  code : int;
  message : string;
  data : Yojson.Safe.t option;
}
type response =
    Success of { id : int; result : Yojson.Safe.t; }
  | Failure of { id : int option; error : rpc_error; }
val parse_error : int
val invalid_request : int
val method_not_found : int
val invalid_params : int
val internal_error : int
val hydration_mismatch : int
val zone_error : int
val di_error : int
val render_timeout : int
val render_request :
  url:string ->
  ?headers:(string * string) list ->
  ?document:string -> ?providers:string list -> unit -> render_request
val health_request : ?include_memory:bool -> unit -> health_request
val prerender_request :
  routes:string list -> output_dir:string -> prerender_request
val encode_render_request :
  render_request ->
  [> `Assoc of
       (string *
        [> `Assoc of (string * [> `String of string ]) list
         | `List of [> `String of string ] list
         | `Null
         | `String of string ])
       list ]
val encode_request : id:int -> method_name:string -> Yojson.Safe.t -> string
val encode_render : id:int -> render_request -> string
val encode_health : id:int -> health_request -> string
val encode_prerender : id:int -> prerender_request -> string
val decode_render_result : Yojson__Safe.t -> render_result
val decode_health_result : Yojson__Safe.t -> health_result
val decode_response : string -> response
val error_response :
  ?id:int ->
  code:int -> message:string -> ?data:Yojson.Safe.t -> unit -> response
val is_success : response -> bool
val get_result : response -> Yojson.Safe.t option
val get_error : response -> rpc_error option
