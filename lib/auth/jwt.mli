type algorithm = HS256 | HS384 | HS512
type header = { alg : algorithm; typ : string; }
type claims = {
  iss : string option;
  sub : string option;
  aud : string option;
  exp : float option;
  nbf : float option;
  iat : float option;
  jti : string option;
}
type t = { header : header; claims : claims; payload : Yojson.Safe.t; }
val algorithm_to_string : algorithm -> string
val algorithm_of_string : string -> algorithm option
val base64url_encode : string -> string
val base64url_decode : string -> (string, [> `Msg of string ]) result
val sign : algorithm:algorithm -> secret:string -> String.t -> string
val header_to_json :
  header -> [> `Assoc of (string * [> `String of string ]) list ]
val encode :
  ?algorithm:algorithm ->
  secret:string ->
  payload:Yojson.Safe.t ->
  ?iss:string ->
  ?sub:string ->
  ?aud:string -> ?exp:float -> ?nbf:float -> ?jti:string -> unit -> string
val claims_of_json : Yojson__Safe.t -> claims
val decode : secret:string -> string -> (t, string) result
val decode_unsafe : string -> (t, string) result
val get_claim : t -> string -> Yojson__Safe.t
val is_expired : t -> bool
val subject : t -> string option
val time_to_expiry : t -> float option
