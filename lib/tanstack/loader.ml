(** Route Loaders

    Data loading for routes (Remix-style). *)

open Route_def

(** {1 Loader Types} *)

(** Loader result with metadata *)
type 'a loader_result = {
  data: 'a;
  headers: (string * string) list;
  status: int;
}

(** Create a successful loader result *)
let ok ?(headers = []) ?(status = 200) data = {
  data;
  headers;
  status;
}

(** {1 Loader Utilities} *)

(** Get param from context *)
let param name (ctx : _ loader_context) =
  List.find_map (fun (n, v) -> if n = name then Some v else None) ctx.params

(** Get required param (raises if missing) *)
let param_exn name ctx =
  match param name ctx with
  | Some v -> v
  | None -> failwith (Printf.sprintf "Missing required param: %s" name)

(** Get param as int *)
let param_int name ctx =
  match param name ctx with
  | Some v -> (try Some (int_of_string v) with _ -> None)
  | None -> None

(** Get search param *)
let search_param name ctx =
  List.assoc_opt name ctx.search_params

(** Get all search params with same name *)
let search_params_all name ctx =
  List.filter_map (fun (k, v) ->
    if k = name then Some v else None
  ) ctx.search_params

(** {1 Loader Composition} *)

(** Run multiple loaders in parallel (simulated) *)
let parallel loaders ctx =
  List.map (fun loader -> loader ctx) loaders

(** Run loaders sequentially, stopping on first error *)
let sequential loaders ctx =
  let rec run acc = function
    | [] -> Ok (List.rev acc)
    | loader :: rest ->
      match loader ctx with
      | Ok data -> run (data :: acc) rest
      | Error e -> Error e
  in
  run [] loaders

(** {1 Loader Caching} *)

(** Cache key generator *)
type cache_key = string

(** Generate cache key from context *)
let cache_key (ctx : _ loader_context) =
  let params_str = List.map (fun (k, v) -> k ^ "=" ^ v) ctx.params
                   |> String.concat "&" in
  let search_str = List.map (fun (k, v) -> k ^ "=" ^ v) ctx.search_params
                   |> String.concat "&" in
  Printf.sprintf "%s?%s" params_str search_str

(** {1 Loader Helpers} *)

(** Redirect response *)
let redirect ?(status = 302) url =
  Error (Printf.sprintf "REDIRECT:%d:%s" status url)

(** Parse redirect from error *)
let parse_redirect error =
  if String.length error > 9 && String.sub error 0 9 = "REDIRECT:" then
    let rest = String.sub error 9 (String.length error - 9) in
    match String.split_on_char ':' rest with
    | [status_str; url] ->
      (try Some (int_of_string status_str, url) with _ -> None)
    | _ -> None
  else
    None

(** Not found response *)
let not_found () =
  Error "NOT_FOUND"

(** Is not found error *)
let is_not_found error =
  error = "NOT_FOUND"

(** Unauthorized response *)
let unauthorized () =
  Error "UNAUTHORIZED"

(** Is unauthorized error *)
let is_unauthorized error =
  error = "UNAUTHORIZED"

(** {1 Loader Middleware} *)

(** Loader middleware type *)
type ('ctx, 'a, 'b) middleware =
  ('ctx, 'a) loader -> ('ctx, 'b) loader

(** Add authentication check *)
let with_auth (check : 'ctx -> bool) (loader : ('ctx, 'a) loader) : ('ctx, 'a) loader =
  fun ctx ->
    if check ctx.ctx then
      loader ctx
    else
      unauthorized ()

(** Add logging *)
let with_logging (log : string -> unit) (loader : ('ctx, 'a) loader) : ('ctx, 'a) loader =
  fun ctx ->
    let key = cache_key ctx in
    log (Printf.sprintf "Loading: %s" key);
    let result = loader ctx in
    (match result with
     | Ok _ -> log (Printf.sprintf "Loaded: %s" key)
     | Error e -> log (Printf.sprintf "Failed: %s - %s" key e));
    result

(** Add timing *)
let with_timing (on_time : float -> unit) (loader : ('ctx, 'a) loader) : ('ctx, 'a) loader =
  fun ctx ->
    let start = Unix.gettimeofday () in
    let result = loader ctx in
    let elapsed = Unix.gettimeofday () -. start in
    on_time elapsed;
    result
