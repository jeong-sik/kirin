(** ETag support for HTTP caching *)

(** ETag type - weak or strong *)
type t =
  | Strong of string  (* Exact match required *)
  | Weak of string    (* Semantic equivalence *)

(** Parse ETag from header value *)
let parse s =
  let s = String.trim s in
  if String.length s >= 3 && String.sub s 0 2 = "W/" then
    (* Weak ETag: W/"xxx" *)
    let inner = String.sub s 2 (String.length s - 2) in
    let unquoted = String.trim inner in
    let unquoted =
      if String.length unquoted >= 2 &&
         unquoted.[0] = '"' &&
         unquoted.[String.length unquoted - 1] = '"'
      then String.sub unquoted 1 (String.length unquoted - 2)
      else unquoted
    in
    Weak unquoted
  else
    (* Strong ETag: "xxx" *)
    let unquoted =
      if String.length s >= 2 &&
         s.[0] = '"' &&
         s.[String.length s - 1] = '"'
      then String.sub s 1 (String.length s - 2)
      else s
    in
    Strong unquoted

(** Convert ETag to header string *)
let to_string = function
  | Strong s -> Printf.sprintf "\"%s\"" s
  | Weak s -> Printf.sprintf "W/\"%s\"" s

(* SHA-256 instead of MD5 for the content fingerprint.

   ETag is nominally a cache validator, not a cryptographic identifier
   — but the validator's only job is "different content => different
   tag", which fails the moment a hash collision is achievable. MD5
   collisions are practical, so an attacker who can influence cached
   content can land two distinct bodies under the same ETag and cause
   the cache to return stale or wrong content for a subsequent
   request (cache poisoning). SHA-256 closes that without changing
   the public API: the tag is opaque to clients and longer hex digits
   are fine for any RFC-compliant ETag value. Existing cached entries
   become one-shot misses on the upgrade — bounded cost. *)

(** Generate ETag from content using SHA-256 hash *)
let generate ?(weak = false) content =
  let hash = Digestif.SHA256.digest_string content in
  let hex = Digestif.SHA256.to_hex hash in
  if weak then Weak hex else Strong hex

(** Generate weak ETag from file stats (mtime + size) *)
let generate_from_stats ~mtime ~size =
  let data = Printf.sprintf "%f-%d" mtime size in
  let hash = Digestif.SHA256.digest_string data in
  let hex = Digestif.SHA256.to_hex hash in
  Weak hex

(** Check if two ETags match
    - Strong comparison: both must be strong and identical
    - Weak comparison: values must match (ignoring weak/strong) *)
let matches ?(weak_comparison = true) etag1 etag2 =
  match etag1, etag2 with
  | Strong s1, Strong s2 -> s1 = s2
  | Weak w1, Weak w2 when weak_comparison -> w1 = w2
  | Strong s, Weak w when weak_comparison -> s = w
  | Weak w, Strong s when weak_comparison -> w = s
  | _ -> false

(** Parse If-None-Match header (comma-separated list) *)
let parse_if_none_match header_value =
  String.split_on_char ',' header_value
  |> List.map String.trim
  |> List.filter (fun s -> String.length s > 0)
  |> List.map parse

(** Check if any ETags in If-None-Match match the given ETag *)
let any_match etags target =
  List.exists (fun etag -> matches etag target) etags

(** ETag middleware for automatic caching *)
let middleware : (Request.t -> Response.t) -> (Request.t -> Response.t) =
  fun handler req ->
    let resp = handler req in
    
    match Response.body resp with
    | Response.Stream _ | Response.Producer _ -> resp (* ETag not supported for streams yet *)
    | Response.String body ->
      (* Only add ETag for successful responses with body *)
      if Response.status_code resp = 200 && String.length body > 0 then
        let etag = generate body in
        let etag_str = to_string etag in

        (* Check If-None-Match header *)
        match Request.header "if-none-match" req with
        | Some inm_value ->
          let client_etags = parse_if_none_match inm_value in
          if any_match client_etags etag then
            (* Return 304 Not Modified *)
            Response.empty `Not_modified
            |> Response.with_header "etag" etag_str
          else
            (* Add ETag to response *)
            Response.with_header "etag" etag_str resp
        | None ->
          (* Just add ETag header *)
          Response.with_header "etag" etag_str resp
      else
        resp

(** Check precondition for If-Match (for PUT/DELETE).

    RFC 7232 §3.1: If-Match MUST use the strong comparison function.
    The previous implementation reused [any_match] with its default
    [~weak_comparison:true], so a client could send [If-Match: W/"v"]
    against a server holding [Strong "v"] and pass the precondition.
    That defeats the entire purpose of the header — concurrent
    PUT/DELETE clients could race using only a weak validator, which
    by definition may not have changed when the byte content did,
    and the lost-update guard would silently let one of them through.

    Now: strong comparison is enforced explicitly. Wildcard "*" still
    matches anything per RFC §3.1. *)
let check_if_match req current_etag =
  match Request.header "if-match" req with
  | None -> `Ok  (* No precondition *)
  | Some "*" -> `Ok  (* Wildcard matches anything *)
  | Some value ->
    let client_etags = parse_if_none_match value in
    let strong_match etag = matches ~weak_comparison:false etag current_etag in
    if List.exists strong_match client_etags then
      `Ok
    else
      `Precondition_failed  (* 412 *)
