(** Svelte Load Functions

    SvelteKit-style data loading with +page.server.ts patterns. *)

(** {1 Load Context} *)

(** Load event context *)
type load_context = {
  url: string;
  params: (string * string) list;
  route_id: string;
  parent: unit -> Yojson.Safe.t option;  (* Get parent layout data *)
  depends: string -> unit;  (* Register dependency *)
  cookies: (string * string) list;
  request_headers: (string * string) list;
  platform: Yojson.Safe.t option;
  locals: Yojson.Safe.t;
}

(** {1 Load Results} *)

(** Load function output *)
type load_output =
  | LoadData of Yojson.Safe.t
  | LoadRedirect of int * string
  | LoadError of int * string
  | LoadNotFound

(** {1 Load Function Types} *)

(** Universal load function (runs on server and client) *)
type universal_loader = load_context -> load_output

(** Server-only load function *)
type server_loader = load_context -> load_output

(** {1 Context Builders} *)

(** Create load context *)
let create_context ~url ~params ~route_id ?(parent=fun () -> None)
    ?(cookies=[]) ?(headers=[]) ?platform ?(locals=`Assoc []) () =
  let deps = ref [] in
  {
    url;
    params;
    route_id;
    parent;
    depends = (fun dep -> deps := dep :: !deps);
    cookies;
    request_headers = headers;
    platform;
    locals;
  }

(** Get param from context *)
let param ctx name =
  List.assoc_opt name ctx.params

(** Get required param *)
let param_exn ctx name =
  match param ctx name with
  | Some v -> v
  | None -> failwith (Printf.sprintf "Missing required param: %s" name)

(** Get param as int *)
let param_int ctx name =
  match param ctx name with
  | Some v -> int_of_string_opt v
  | None -> None

(** Get cookie *)
let cookie ctx name =
  List.assoc_opt name ctx.cookies

(** Get header *)
let header ctx name =
  let name_lower = String.lowercase_ascii name in
  List.find_map (fun (k, v) ->
    if String.lowercase_ascii k = name_lower then Some v else None
  ) ctx.request_headers

(** {1 Load Result Helpers} *)

(** Return data *)
let data json = LoadData json

(** Return redirect *)
let redirect ?(status=303) location = LoadRedirect (status, location)

(** Return error *)
let error status message = LoadError (status, message)

(** Return 404 *)
let not_found = LoadNotFound

(** {1 Dependency Tracking} *)

(** Invalidation dependencies *)
type depends_on =
  | Url          (* Invalidate when URL changes *)
  | Param of string  (* Invalidate when specific param changes *)
  | Custom of string (* Custom dependency key *)

(** Register dependencies *)
let register_depends ctx deps =
  List.iter (fun dep ->
    let key = match dep with
      | Url -> "url"
      | Param name -> "param:" ^ name
      | Custom key -> "custom:" ^ key
    in
    ctx.depends key
  ) deps

(** {1 Parallel Loading} *)

(** Load multiple resources in parallel *)
let parallel loaders ctx =
  (* In real implementation, this would use Eio fibers *)
  List.map (fun loader -> loader ctx) loaders

(** Load with timeout *)
let with_timeout _seconds loader ctx =
  (* Simplified - real impl would use Eio.Time *)
  loader ctx

(** {1 Caching} *)

(** Cache control directives *)
type cache_control = {
  max_age: int option;
  s_maxage: int option;
  stale_while_revalidate: int option;
  private_cache: bool;
  no_store: bool;
}

(** Default cache control *)
let default_cache = {
  max_age = None;
  s_maxage = None;
  stale_while_revalidate = None;
  private_cache = false;
  no_store = false;
}

(** Cache for duration *)
let cache_for seconds = {
  default_cache with max_age = Some seconds
}

(** CDN cache for duration *)
let cdn_cache_for seconds = {
  default_cache with s_maxage = Some seconds
}

(** No caching *)
let no_cache = {
  default_cache with no_store = true
}

(** Cache control to header value *)
let cache_header cache =
  let parts = [] in
  let parts = match cache.max_age with
    | Some s -> Printf.sprintf "max-age=%d" s :: parts
    | None -> parts
  in
  let parts = match cache.s_maxage with
    | Some s -> Printf.sprintf "s-maxage=%d" s :: parts
    | None -> parts
  in
  let parts = match cache.stale_while_revalidate with
    | Some s -> Printf.sprintf "stale-while-revalidate=%d" s :: parts
    | None -> parts
  in
  let parts = if cache.private_cache then "private" :: parts else parts in
  let parts = if cache.no_store then "no-store" :: parts else parts in
  String.concat ", " (List.rev parts)

(** {1 Streaming} *)

(** Streaming load result *)
type stream_chunk =
  | Initial of Yojson.Safe.t  (* Initial data *)
  | Update of string * Yojson.Safe.t  (* Key, data *)
  | StreamError of string
  | StreamComplete

(** Create streaming loader *)
let streaming initial_data updates =
  let results = ref [Initial initial_data] in
  List.iter (fun (key, data) ->
    results := Update (key, data) :: !results
  ) updates;
  results := StreamComplete :: !results;
  List.rev !results

(** {1 Serialization} *)

(** Load output to JSON *)
let output_to_json = function
  | LoadData data ->
    `Assoc [("type", `String "data"); ("data", data)]
  | LoadRedirect (status, location) ->
    `Assoc [
      ("type", `String "redirect");
      ("status", `Int status);
      ("location", `String location);
    ]
  | LoadError (status, message) ->
    `Assoc [
      ("type", `String "error");
      ("status", `Int status);
      ("message", `String message);
    ]
  | LoadNotFound ->
    `Assoc [("type", `String "notFound")]

(** Context to JSON (for debugging) *)
let context_to_json ctx =
  `Assoc [
    ("url", `String ctx.url);
    ("params", `Assoc (List.map (fun (k, v) -> (k, `String v)) ctx.params));
    ("routeId", `String ctx.route_id);
  ]
