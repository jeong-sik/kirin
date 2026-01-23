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

(** In-memory storage for rate limiting *)
module Store = struct
  let buckets : (string, bucket) Hashtbl.t = Hashtbl.create 1024

  let get key =
    Hashtbl.find_opt buckets key

  let set key bucket =
    Hashtbl.replace buckets key bucket

  (* Cleanup old entries periodically *)
  let cleanup ~older_than =
    let now = Unix.gettimeofday () in
    let to_remove = ref [] in
    Hashtbl.iter (fun key bucket ->
      if now -. bucket.last_update > older_than then
        to_remove := key :: !to_remove
    ) buckets;
    List.iter (Hashtbl.remove buckets) !to_remove
end

(** Get client identifier from request (IP address) *)
let get_client_id req =
  (* Try X-Forwarded-For first, then X-Real-IP, then connection IP *)
  match Request.header "x-forwarded-for" req with
  | Some xff ->
    (* Take first IP from X-Forwarded-For *)
    (match String.split_on_char ',' xff with
    | ip :: _ -> String.trim ip
    | [] -> "unknown")
  | None ->
    match Request.header "x-real-ip" req with
    | Some ip -> ip
    | None -> "unknown"

(** Check if request is allowed and consume a token *)
let check_rate_limit config client_id =
  let now = Unix.gettimeofday () in

  let bucket = match Store.get client_id with
    | Some b -> b
    | None ->
      let b = { tokens = float_of_int config.burst_size; last_update = now } in
      Store.set client_id b;
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
    Store.set client_id bucket;
    `Allowed {
      remaining = int_of_float bucket.tokens;
      limit = config.burst_size;
      reset_after = (1.0 -. (bucket.tokens -. floor bucket.tokens)) /. config.requests_per_second;
    }
  end else begin
    Store.set client_id bucket;
    `Limited {
      retry_after = (1.0 -. bucket.tokens) /. config.requests_per_second;
      limit = config.burst_size;
    }
  end

(** Add rate limit headers to response *)
let add_rate_limit_headers ~limit ~remaining ~reset_after resp =
  resp
  |> Response.with_header "x-ratelimit-limit" (string_of_int limit)
  |> Response.with_header "x-ratelimit-remaining" (string_of_int remaining)
  |> Response.with_header "x-ratelimit-reset" (Printf.sprintf "%.0f" reset_after)

(** Create 429 Too Many Requests response *)
let too_many_requests ~retry_after ~limit =
  Response.make ~status:`Too_many_requests "Too Many Requests"
  |> Response.with_header "content-type" "text/plain; charset=utf-8"
  |> Response.with_header "retry-after" (Printf.sprintf "%.0f" (ceil retry_after))
  |> Response.with_header "x-ratelimit-limit" (string_of_int limit)
  |> Response.with_header "x-ratelimit-remaining" "0"

(** Rate limiting middleware *)
let middleware ?(config = default_config) ?(get_key = get_client_id) : (Request.t -> Response.t) -> (Request.t -> Response.t) =
  fun handler req ->
    let client_id = get_key req in
    match check_rate_limit config client_id with
    | `Allowed { remaining; limit; reset_after } ->
      let resp = handler req in
      add_rate_limit_headers ~limit ~remaining ~reset_after resp
    | `Limited { retry_after; limit } ->
      too_many_requests ~retry_after ~limit

(** Create a rate limiter with custom storage (for distributed systems) *)
module type Storage = sig
  val get : string -> bucket option
  val set : string -> bucket -> unit
end

(** Advanced: Create middleware with custom storage *)
let middleware_with_storage (module S : Storage) ?(config = default_config) ?(get_key = get_client_id) =
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
      too_many_requests
        ~retry_after:((1.0 -. bucket.tokens) /. config.requests_per_second)
        ~limit:config.burst_size
    end
