(** CSRF (Cross-Site Request Forgery) Protection

    Token-based CSRF protection using the Synchronizer Token Pattern.
    Generates secure tokens and validates on form submissions.

    {b Example:}
    {[
      (* Generate token for form *)
      let token = Csrf.generate ~secret:"app-secret" ~session_id in

      (* In HTML form *)
      <input type="hidden" name="_csrf" value="{{token}}">

      (* Validate on submission *)
      match Csrf.validate ~secret:"app-secret" ~session_id req with
      | Ok () -> (* process form *)
      | Error msg -> Kirin.Response.text ~status:`Forbidden msg
    ]}
*)

(** {1 Configuration} *)

(** Default token lifetime in seconds (1 hour) *)
let default_ttl = 3600.0

(** Token form field name *)
let default_field_name = "_csrf"

(** Token header name *)
let default_header_name = "X-CSRF-Token"

(** {1 Helpers} *)

(** Convert string to hex *)
let string_to_hex s =
  let hex = Bytes.create (String.length s * 2) in
  String.iteri (fun i c ->
    let byte = Char.code c in
    let hi = byte lsr 4 in
    let lo = byte land 0x0f in
    let to_hex n = if n < 10 then Char.chr (n + 48) else Char.chr (n + 87) in
    Bytes.set hex (i * 2) (to_hex hi);
    Bytes.set hex (i * 2 + 1) (to_hex lo)
  ) s;
  Bytes.to_string hex

(** {1 Token Generation} *)

(** Generate CSRF token *)
let generate ~secret ~session_id ?(ttl = default_ttl) () =
  let timestamp = Unix.gettimeofday () in
  let expires = timestamp +. ttl in

  (* Random nonce using SHA256 of entropy *)
  let rand = Random.int 1_000_000_000 in
  let entropy = Printf.sprintf "csrf-%.6f-%d-%d" timestamp rand (Unix.getpid ()) in
  let nonce_hash = Digestif.SHA256.digest_string entropy in
  let nonce_hex = string_to_hex (String.sub (Digestif.SHA256.to_raw_string nonce_hash) 0 16) in

  (* Data to sign: session_id|expires|nonce *)
  let data = Printf.sprintf "%s|%.0f|%s" session_id expires nonce_hex in

  (* HMAC signature *)
  let signature = Digestif.SHA256.hmac_string ~key:secret data in
  let sig_hex = string_to_hex (Digestif.SHA256.to_raw_string signature) in

  (* Token format: data.signature (base64url encoded) *)
  let token_data = Printf.sprintf "%s.%s" data sig_hex in
  Base64.encode_string ~pad:false ~alphabet:Base64.uri_safe_alphabet token_data

(** {1 Token Validation} *)

(** Timing-safe string comparison *)
let constant_time_compare a b =
  let len_a = String.length a in
  let len_b = String.length b in
  let result = ref (len_a lxor len_b) in
  for i = 0 to min len_a len_b - 1 do
    result := !result lor (Char.code (String.get a i) lxor Char.code (String.get b i))
  done;
  !result = 0

(** Validate CSRF token *)
let validate_token ~secret ~session_id token =
  (* Decode token *)
  match Base64.decode ~alphabet:Base64.uri_safe_alphabet token with
  | Error _ -> Error "Invalid token encoding"
  | Ok decoded ->
      (* Split data and signature *)
      match String.rindex_opt decoded '.' with
      | None -> Error "Invalid token format"
      | Some idx ->
          let data = String.sub decoded 0 idx in
          let provided_sig = String.sub decoded (idx + 1) (String.length decoded - idx - 1) in

          (* Verify signature *)
          let expected_sig = Digestif.SHA256.hmac_string ~key:secret data in
          let expected_sig_hex = string_to_hex (Digestif.SHA256.to_raw_string expected_sig) in

          if not (constant_time_compare provided_sig expected_sig_hex) then
            Error "Invalid signature"
          else
            (* Parse data: session_id|expires|nonce *)
            match String.split_on_char '|' data with
            | [token_session; expires_str; _nonce] ->
                (* Verify session *)
                if not (constant_time_compare token_session session_id) then
                  Error "Session mismatch"
                else
                  (* Check expiration *)
                  (try
                    let expires = float_of_string expires_str in
                    if Unix.gettimeofday () > expires then
                      Error "Token expired"
                    else
                      Ok ()
                  with _ -> Error "Invalid expiration")
            | _ -> Error "Invalid token data"

(** {1 Request Validation} *)

(** Get form field from request body *)
let get_form_field name req =
  let form_data = Kirin.Request.form_body req in
  match List.assoc_opt name form_data with
  | Some (v :: _) -> Some v
  | _ -> None

(** Get token from request (form field or header) *)
let get_token_from_request ?(field_name = default_field_name)
    ?(header_name = default_header_name) req =
  (* Try header first (for AJAX) *)
  match Kirin.Request.header header_name req with
  | Some token -> Some token
  | None ->
      (* Try form field *)
      get_form_field field_name req

(** Validate request *)
let validate ~secret ~session_id ?field_name ?header_name req =
  match get_token_from_request ?field_name ?header_name req with
  | None -> Error "Missing CSRF token"
  | Some token -> validate_token ~secret ~session_id token

(** {1 Middleware} *)

(** CSRF protection middleware *)
let middleware ~secret ?(field_name = default_field_name)
    ?(header_name = default_header_name) ?(safe_methods = ["GET"; "HEAD"; "OPTIONS"])
    get_session_id =
  fun handler req ->
    let method_str = Http.Method.to_string (Kirin.Request.meth req) in

    (* Skip safe methods *)
    if List.mem method_str safe_methods then
      handler req
    else
      (* Get session ID *)
      match get_session_id req with
      | None ->
          Kirin.Response.text ~status:`Forbidden "Session required for CSRF validation"
      | Some session_id ->
          match validate ~secret ~session_id ~field_name ~header_name req with
          | Ok () -> handler req
          | Error msg ->
              Kirin.Response.text ~status:`Forbidden ("CSRF validation failed: " ^ msg)

(** {1 Template Helpers} *)

(** Generate hidden input HTML *)
let hidden_input ~secret ~session_id () =
  let token = generate ~secret ~session_id () in
  Printf.sprintf "<input type=\"hidden\" name=\"%s\" value=\"%s\">"
    default_field_name token

(** Generate meta tag for JavaScript access *)
let meta_tag ~secret ~session_id () =
  let token = generate ~secret ~session_id () in
  Printf.sprintf "<meta name=\"csrf-token\" content=\"%s\">" token
