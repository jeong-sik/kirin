(** JWT (JSON Web Token) Implementation

    RFC 7519 compliant JWT encoding, decoding, and verification.
    Supports HS256 (HMAC-SHA256) algorithm.

    {b Example:}
    {[
      let secret = "my-secret-key" in
      let token = Jwt.encode ~secret
        ~payload:(`Assoc [("sub", `String "user123"); ("role", `String "admin")])
        ~exp:(Unix.time () +. 3600.) (* 1 hour *)
        () in
      match Jwt.decode ~secret token with
      | Ok payload -> (* use payload *)
      | Error msg -> (* handle error *)
    ]}
*)

(** {1 Types} *)

(** JWT algorithm *)
type algorithm = HS256 | HS384 | HS512

(** JWT header *)
type header = {
  alg : algorithm;
  typ : string;
}

(** JWT claims (standard) *)
type claims = {
  iss : string option;  (** Issuer *)
  sub : string option;  (** Subject *)
  aud : string option;  (** Audience *)
  exp : float option;   (** Expiration time *)
  nbf : float option;   (** Not before *)
  iat : float option;   (** Issued at *)
  jti : string option;  (** JWT ID *)
}

(** Decoded JWT *)
type t = {
  header : header;
  claims : claims;
  payload : Yojson.Safe.t;  (** Full payload including custom claims *)
}

(** {1 Helpers} *)

let algorithm_to_string = function
  | HS256 -> "HS256"
  | HS384 -> "HS384"
  | HS512 -> "HS512"

let algorithm_of_string = function
  | "HS256" -> Some HS256
  | "HS384" -> Some HS384
  | "HS512" -> Some HS512
  | _ -> None

(** Base64url encode (RFC 4648) *)
let base64url_encode s =
  Base64.encode_string ~pad:false ~alphabet:Base64.uri_safe_alphabet s

(** Base64url decode *)
let base64url_decode s =
  (* Add padding if needed *)
  let padded =
    let len = String.length s in
    let pad = (4 - len mod 4) mod 4 in
    s ^ String.make pad '='
  in
  Base64.decode ~alphabet:Base64.uri_safe_alphabet padded

(** HMAC signing *)
let sign ~algorithm ~secret data =
  let module H = Digestif in
  match algorithm with
  | HS256 -> H.SHA256.hmac_string ~key:secret data |> H.SHA256.to_raw_string
  | HS384 -> H.SHA384.hmac_string ~key:secret data |> H.SHA384.to_raw_string
  | HS512 -> H.SHA512.hmac_string ~key:secret data |> H.SHA512.to_raw_string

(** {1 Encoding} *)

(** Create JWT header JSON *)
let header_to_json header =
  `Assoc [
    ("alg", `String (algorithm_to_string header.alg));
    ("typ", `String header.typ);
  ]

(** Encode JWT *)
let encode ?(algorithm = HS256) ~secret ~payload ?iss ?sub ?aud ?exp ?nbf ?jti () =
  (* Build header *)
  let header = { alg = algorithm; typ = "JWT" } in
  let header_json = header_to_json header in
  let header_b64 = base64url_encode (Yojson.Safe.to_string header_json) in

  (* Add standard claims to payload *)
  let now = Unix.gettimeofday () in
  let add_claim key value acc =
    match value with
    | None -> acc
    | Some v -> (key, v) :: acc
  in
  let standard_claims =
    []
    |> add_claim "iss" (Option.map (fun s -> `String s) iss)
    |> add_claim "sub" (Option.map (fun s -> `String s) sub)
    |> add_claim "aud" (Option.map (fun s -> `String s) aud)
    |> add_claim "exp" (Option.map (fun f -> `Float f) exp)
    |> add_claim "nbf" (Option.map (fun f -> `Float f) nbf)
    |> add_claim "iat" (Some (`Float now))
    |> add_claim "jti" (Option.map (fun s -> `String s) jti)
  in

  (* Merge with custom payload *)
  let full_payload = match payload with
    | `Assoc fields -> `Assoc (standard_claims @ fields)
    | _ -> `Assoc (("data", payload) :: standard_claims)
  in
  let payload_b64 = base64url_encode (Yojson.Safe.to_string full_payload) in

  (* Sign *)
  let signing_input = header_b64 ^ "." ^ payload_b64 in
  let signature = sign ~algorithm ~secret signing_input in
  let signature_b64 = base64url_encode signature in

  header_b64 ^ "." ^ payload_b64 ^ "." ^ signature_b64

(** {1 Decoding} *)

(** Extract claims from JSON *)
let claims_of_json json =
  let open Yojson.Safe.Util in
  let get_string key = json |> member key |> to_string_option in
  let get_float key =
    match json |> member key with
    | `Float f -> Some f
    | `Int i -> Some (float_of_int i)
    | _ -> None
  in
  {
    iss = get_string "iss";
    sub = get_string "sub";
    aud = get_string "aud";
    exp = get_float "exp";
    nbf = get_float "nbf";
    iat = get_float "iat";
    jti = get_string "jti";
  }

(** Decode and verify JWT *)
let decode ~secret token =
  match String.split_on_char '.' token with
  | [header_b64; payload_b64; signature_b64] ->
      (* Decode header *)
      (match base64url_decode header_b64 with
       | Error _ -> Error "Invalid header encoding"
       | Ok header_str ->
           let header_json = Yojson.Safe.from_string header_str in
           let open Yojson.Safe.Util in
           let alg_str = header_json |> member "alg" |> to_string in
           (match algorithm_of_string alg_str with
            | None -> Error ("Unsupported algorithm: " ^ alg_str)
            | Some algorithm ->
                (* Verify signature *)
                let signing_input = header_b64 ^ "." ^ payload_b64 in
                let expected_sig = sign ~algorithm ~secret signing_input in
                let expected_sig_b64 = base64url_encode expected_sig in
                if not (String.equal signature_b64 expected_sig_b64) then
                  Error "Invalid signature"
                else
                  (* Decode payload *)
                  (match base64url_decode payload_b64 with
                   | Error _ -> Error "Invalid payload encoding"
                   | Ok payload_str ->
                       let payload = Yojson.Safe.from_string payload_str in
                       let claims = claims_of_json payload in

                       (* Validate expiration *)
                       let now = Unix.gettimeofday () in
                       (match claims.exp with
                        | Some exp when exp < now -> Error "Token expired"
                        | _ ->
                            (* Validate not-before *)
                            (match claims.nbf with
                             | Some nbf when nbf > now -> Error "Token not yet valid"
                             | _ ->
                                 let header = { alg = algorithm; typ = "JWT" } in
                                 Ok { header; claims; payload })))))
  | _ -> Error "Invalid token format"

(** Decode without verification (for debugging) *)
let decode_unsafe token =
  match String.split_on_char '.' token with
  | [header_b64; payload_b64; _] ->
      (match base64url_decode header_b64, base64url_decode payload_b64 with
       | Ok header_str, Ok payload_str ->
           let header_json = Yojson.Safe.from_string header_str in
           let payload = Yojson.Safe.from_string payload_str in
           let open Yojson.Safe.Util in
           let alg_str = header_json |> member "alg" |> to_string in
           (match algorithm_of_string alg_str with
            | None -> Error ("Unsupported algorithm: " ^ alg_str)
            | Some algorithm ->
                let header = { alg = algorithm; typ = "JWT" } in
                let claims = claims_of_json payload in
                Ok { header; claims; payload })
       | _ -> Error "Invalid encoding")
  | _ -> Error "Invalid token format"

(** {1 Utilities} *)

(** Get a claim value from decoded token *)
let get_claim token key =
  Yojson.Safe.Util.(token.payload |> member key)

(** Check if token is expired *)
let is_expired token =
  match token.claims.exp with
  | None -> false
  | Some exp -> exp < Unix.gettimeofday ()

(** Get subject from token *)
let subject token = token.claims.sub

(** Get remaining time until expiration *)
let time_to_expiry token =
  match token.claims.exp with
  | None -> None
  | Some exp -> Some (exp -. Unix.gettimeofday ())
