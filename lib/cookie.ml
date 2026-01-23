(** Cookie handling module *)

(** Cookie attributes for Set-Cookie header *)
type attributes = {
  max_age : int option;         (* Max age in seconds *)
  expires : string option;      (* Expiry date string *)
  domain : string option;       (* Domain scope *)
  path : string option;         (* Path scope *)
  secure : bool;                (* HTTPS only *)
  http_only : bool;             (* No JS access *)
  same_site : [`Strict | `Lax | `None] option;
}

let default_attributes = {
  max_age = None;
  expires = None;
  domain = None;
  path = Some "/";
  secure = false;
  http_only = true;
  same_site = Some `Lax;
}

(** Parse cookies from Cookie header value *)
let parse_cookies header_value =
  String.split_on_char ';' header_value
  |> List.filter_map (fun pair ->
    let pair = String.trim pair in
    match String.index_opt pair '=' with
    | Some idx ->
      let name = String.sub pair 0 idx |> String.trim in
      let value = String.sub pair (idx + 1) (String.length pair - idx - 1) |> String.trim in
      (* URL decode the value *)
      let value = Uri.pct_decode value in
      Some (name, value)
    | None -> None
  )

(** Get a cookie value from request *)
let get name req =
  match Request.header "cookie" req with
  | Some header_value ->
    let cookies = parse_cookies header_value in
    List.assoc_opt name cookies
  | None -> None

(** Get all cookies from request *)
let get_all req =
  match Request.header "cookie" req with
  | Some header_value -> parse_cookies header_value
  | None -> []

(** Build Set-Cookie header value *)
let build_set_cookie name value attrs =
  let buf = Buffer.create 128 in
  (* Name=Value *)
  Buffer.add_string buf name;
  Buffer.add_char buf '=';
  Buffer.add_string buf (Uri.pct_encode value);

  (* Max-Age *)
  (match attrs.max_age with
  | Some age -> Buffer.add_string buf (Printf.sprintf "; Max-Age=%d" age)
  | None -> ());

  (* Expires *)
  (match attrs.expires with
  | Some exp -> Buffer.add_string buf (Printf.sprintf "; Expires=%s" exp)
  | None -> ());

  (* Domain *)
  (match attrs.domain with
  | Some d -> Buffer.add_string buf (Printf.sprintf "; Domain=%s" d)
  | None -> ());

  (* Path *)
  (match attrs.path with
  | Some p -> Buffer.add_string buf (Printf.sprintf "; Path=%s" p)
  | None -> ());

  (* Secure *)
  if attrs.secure then Buffer.add_string buf "; Secure";

  (* HttpOnly *)
  if attrs.http_only then Buffer.add_string buf "; HttpOnly";

  (* SameSite *)
  (match attrs.same_site with
  | Some `Strict -> Buffer.add_string buf "; SameSite=Strict"
  | Some `Lax -> Buffer.add_string buf "; SameSite=Lax"
  | Some `None -> Buffer.add_string buf "; SameSite=None"
  | None -> ());

  Buffer.contents buf

(** Set a cookie on response *)
let set ?(attrs = default_attributes) name value resp =
  let header_value = build_set_cookie name value attrs in
  Response.with_header "set-cookie" header_value resp

(** Delete a cookie by setting it to expire *)
let delete ?(path = "/") name resp =
  let attrs = {
    default_attributes with
    max_age = Some 0;
    expires = Some "Thu, 01 Jan 1970 00:00:00 GMT";
    path = Some path;
  } in
  set ~attrs name "" resp

(* ============================================================
   Signed Cookies (HMAC-based)
   ============================================================ *)

(** Secret key for signing (should be set by application) *)
let secret_key = ref ""

(** Set the secret key for cookie signing *)
let set_secret key =
  if String.length key < 32 then
    failwith "Cookie secret key must be at least 32 characters"
  else
    secret_key := key

(** Sign a value using HMAC-SHA256 *)
let sign value =
  if !secret_key = "" then
    failwith "Cookie secret key not set. Call Kirin.Cookie.set_secret first."
  else
    let hmac = Digestif.SHA256.hmac_string ~key:!secret_key value in
    Base64.encode_exn (Digestif.SHA256.to_raw_string hmac)

(** Verify and extract signed value *)
let verify signed_value =
  try
    match String.rindex_opt signed_value '.' with
    | Some idx ->
      let value = String.sub signed_value 0 idx in
      let signature = String.sub signed_value (idx + 1) (String.length signed_value - idx - 1) in
      let expected_sig = sign value in
      if signature = expected_sig then
        Some value
      else
        None
    | None -> None
  with _ -> None

(** Get a signed cookie value *)
let get_signed name req =
  match get name req with
  | Some signed_value -> verify signed_value
  | None -> None

(** Set a signed cookie *)
let set_signed ?(attrs = default_attributes) name value resp =
  let signed_value = value ^ "." ^ sign value in
  set ~attrs name signed_value resp
