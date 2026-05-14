(** Rate limiting middleware using token bucket algorithm *)

(** Rate limit configuration *)
type config = {
  requests_per_second : float;  (* Rate of token replenishment *)
  burst_size : int;             (* Maximum tokens (bucket capacity) *)
}

(** Default configuration: 10 req/s with burst of 20 *)
let default_config = {
  requests_per_second = 10.0;
  burst_size = 20;
}

(** Bucket state *)
type bucket = {
  mutable tokens : float;
  mutable last_update : float;
}

(** Rate limit result types *)
type allowed_info = {
  remaining : int;
  limit : int;
  reset_after : float;
}

type limited_info = {
  retry_after : float;
  limit : int;
}

(** In-memory storage for rate limiting.
    All operations are protected by an Eio.Mutex to prevent data races
    under concurrent Eio fibers. Stdlib.Mutex must not be used here
    because it blocks the entire Eio thread and can cause EDEADLK. *)
module Store = struct
  let buckets : (string, bucket) Hashtbl.t = Hashtbl.create 1024
  let mu = Eio.Mutex.create ()

  let get key =
    Eio.Mutex.use_ro mu (fun () -> Hashtbl.find_opt buckets key)

  let set key bucket =
    Eio.Mutex.use_rw ~protect:true mu (fun () ->
      Hashtbl.replace buckets key bucket)

  (* Cleanup old entries periodically *)
  let cleanup ~older_than =
    Eio.Mutex.use_rw ~protect:true mu (fun () ->
      let now = Unix.gettimeofday () in
      let to_remove = ref [] in
      Hashtbl.iter (fun key bucket ->
        if now -. bucket.last_update > older_than then
          to_remove := key :: !to_remove
      ) buckets;
      List.iter (Hashtbl.remove buckets) !to_remove)
end

(* X-Forwarded-For / X-Real-IP can be set by *any* HTTP client, so
   trusting them unconditionally turns the rate limiter into a
   sieve: a request that sends
   [X-Forwarded-For: <random ip>] each time burns through unique
   buckets and bypasses the limit entirely.  Worse, a request that
   sends [X-Forwarded-For: <victim>] drains the victim's bucket
   instead of its own — targeted lockout.

   These headers are only safe when the request arrived through a
   trusted reverse proxy that overwrites them; the application has
   to opt in to that contract.  Default to [false]: ignore the
   forwarded headers entirely and fall back to ["unknown"].
   Deployments that *do* sit behind a known proxy (nginx, ALB,
   Cloudflare) can pass [~trust_forwarded:true]. *)
let trim_first_csv s =
  match String.split_on_char ',' s with
  | [] -> ""
  | first :: _ -> String.trim first

(** [get_client_id ?trust_forwarded req] extracts a client
    identifier from the request.

    When [trust_forwarded] is [false] (the default) the forwarded
    headers are ignored — they are caller-controlled and forging
    them is the rate-limit bypass surface.  When [true], the first
    address in [X-Forwarded-For] is used, falling back to
    [X-Real-IP].  Only enable [trust_forwarded] behind a reverse
    proxy that rewrites those headers itself. *)
let get_client_id ?(trust_forwarded = false) req =
  if not trust_forwarded then "unknown"
  else
    match Request.header "x-forwarded-for" req with
    | Some xff when xff <> "" ->
      let ip = trim_first_csv xff in
      if ip = "" then "unknown" else ip
    | _ ->
      match Request.header "x-real-ip" req with
      | Some ip when ip <> "" -> ip
      | _ -> "unknown"

(** Check if request is allowed and consume a token.
    The entire read-modify-write is performed under Store.mu so that
    concurrent fibers cannot see a partially-updated bucket. *)
let check_rate_limit config client_id =
  Eio.Mutex.use_rw ~protect:true Store.mu (fun () ->
    let now = Unix.gettimeofday () in

    let bucket = match Hashtbl.find_opt Store.buckets client_id with
      | Some b -> b
      | None ->
        let b = { tokens = float_of_int config.burst_size; last_update = now } in
        Hashtbl.replace Store.buckets client_id b;
        b
    in

    (* Replenish tokens based on time elapsed *)
    let elapsed = now -. bucket.last_update in
    let new_tokens = bucket.tokens +. (elapsed *. config.requests_per_second) in
    bucket.tokens <- min (float_of_int config.burst_size) new_tokens;
    bucket.last_update <- now;

    (* Try to consume one token *)
    if bucket.tokens >= 1.0 then begin
      bucket.tokens <- bucket.tokens -. 1.0;
      Hashtbl.replace Store.buckets client_id bucket;
      `Allowed {
        remaining = int_of_float bucket.tokens;
        limit = config.burst_size;
        reset_after = (1.0 -. (bucket.tokens -. floor bucket.tokens)) /. config.requests_per_second;
      }
    end else begin
      Hashtbl.replace Store.buckets client_id bucket;
      `Limited {
        retry_after = (1.0 -. bucket.tokens) /. config.requests_per_second;
        limit = config.burst_size;
      }
    end)

(** Add rate limit headers to response *)
let add_rate_limit_headers ~limit ~remaining ~reset_after resp =
  resp
  |> Response.with_header "x-ratelimit-limit" (string_of_int limit)
  |> Response.with_header "x-ratelimit-remaining" (string_of_int remaining)
  |> Response.with_header "x-ratelimit-reset" (Printf.sprintf "%.0f" reset_after)

(** Create 429 Too Many Requests response *)
let limit_exceeded_response ~retry_after ~limit =
  Response.make ~status:`Too_many_requests (`String "Too Many Requests")
  |> Response.with_header "content-type" "text/plain; charset=utf-8"
  |> Response.with_header "retry-after" (Printf.sprintf "%.0f" (ceil retry_after))
  |> Response.with_header "x-ratelimit-limit" (string_of_int limit)
  |> Response.with_header "x-ratelimit-remaining" "0"

(* Monomorphic wrapper so [middleware]'s [?get_key] signature
   stays [Request.t -> string] — exposing the [?trust_forwarded]
   default through the middleware would change its callable type
   and break callers that already pass [~get_key]. *)
let default_get_key req = get_client_id req

(** Rate limiting middleware *)
let middleware ?(config = default_config) ?(get_key = default_get_key) : (Request.t -> Response.t) -> (Request.t -> Response.t) =
  fun handler req ->
    let client_id = get_key req in
    match check_rate_limit config client_id with
    | `Allowed { remaining; limit; reset_after } ->
      let resp = handler req in
      add_rate_limit_headers ~limit ~remaining ~reset_after resp
    | `Limited { retry_after; limit } ->
      limit_exceeded_response ~retry_after ~limit

(** Create a rate limiter with custom storage (for distributed systems) *)
module type Storage = sig
  val get : string -> bucket option
  val set : string -> bucket -> unit
end

(** Advanced: Create middleware with custom storage *)
let middleware_with_storage (module S : Storage) ?(config = default_config) ?(get_key = default_get_key) =
  fun handler req ->
    let client_id = get_key req in
    let now = Unix.gettimeofday () in

    let bucket = match S.get client_id with
      | Some b -> b
      | None ->
        let b = { tokens = float_of_int config.burst_size; last_update = now } in
        S.set client_id b;
        b
    in

    let elapsed = now -. bucket.last_update in
    let new_tokens = bucket.tokens +. (elapsed *. config.requests_per_second) in
    bucket.tokens <- min (float_of_int config.burst_size) new_tokens;
    bucket.last_update <- now;

    if bucket.tokens >= 1.0 then begin
      bucket.tokens <- bucket.tokens -. 1.0;
      S.set client_id bucket;
      let resp = handler req in
      add_rate_limit_headers
        ~limit:config.burst_size
        ~remaining:(int_of_float bucket.tokens)
        ~reset_after:((1.0 -. (bucket.tokens -. floor bucket.tokens)) /. config.requests_per_second)
        resp
    end else begin
      S.set client_id bucket;
      limit_exceeded_response
        ~retry_after:((1.0 -. bucket.tokens) /. config.requests_per_second)
        ~limit:config.burst_size
    end
