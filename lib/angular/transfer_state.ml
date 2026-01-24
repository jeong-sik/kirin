(** Angular Transfer State

    HTTP Transfer Cache for Angular SSR - transfers server-fetched data to client. *)

(** {1 Transfer State Types} *)

(** Cache options *)
type cache_options = {
  include_headers: string list;
  include_post_requests: bool;
  include_request_with_body: bool;
  filter: (string -> bool) option;
}

(** Default cache options *)
let default_cache_options = {
  include_headers = [];
  include_post_requests = false;
  include_request_with_body = false;
  filter = None;
}

(** Transfer state entry *)
type entry = {
  key: string;
  value: Yojson.Safe.t;
  timestamp: float;
  ttl: float option;
}

(** Transfer state store *)
type t = {
  mutable entries: entry list;
  options: cache_options;
}

(** {1 Store Operations} *)

(** Create transfer state *)
let create ?(options=default_cache_options) () = {
  entries = [];
  options;
}

(** Generate cache key for HTTP request *)
let make_http_key ~method_ ~url ?(params=[]) () =
  let param_str = if params = [] then ""
    else "?" ^ String.concat "&" (List.map (fun (k, v) -> k ^ "=" ^ v) params)
  in
  Printf.sprintf "%s:%s%s" method_ url param_str

(** Set value *)
let set state key value =
  let entry = {
    key;
    value;
    timestamp = Unix.gettimeofday ();
    ttl = None;
  } in
  state.entries <- entry :: List.filter (fun e -> e.key <> key) state.entries

(** Set value with TTL *)
let set_with_ttl state key value ttl =
  let entry = {
    key;
    value;
    timestamp = Unix.gettimeofday ();
    ttl = Some ttl;
  } in
  state.entries <- entry :: List.filter (fun e -> e.key <> key) state.entries

(** Get value *)
let get state key =
  let now = Unix.gettimeofday () in
  List.find_opt (fun e ->
    e.key = key &&
    match e.ttl with
    | None -> true
    | Some ttl -> now -. e.timestamp < ttl
  ) state.entries
  |> Option.map (fun e -> e.value)

(** Check if key exists *)
let has_key state key =
  Option.is_some (get state key)

(** Remove value *)
let remove state key =
  state.entries <- List.filter (fun e -> e.key <> key) state.entries

(** Clear all *)
let clear state =
  state.entries <- []

(** Get all keys *)
let keys state =
  List.map (fun e -> e.key) state.entries

(** {1 HTTP Cache Integration} *)

(** Cache HTTP response *)
let cache_response state ~method_ ~url ?(params=[]) response =
  if method_ = "GET" || method_ = "HEAD" ||
     (method_ = "POST" && state.options.include_post_requests) then
    let key = make_http_key ~method_ ~url ~params () in
    let should_cache = match state.options.filter with
      | None -> true
      | Some f -> f url
    in
    if should_cache then set state key response

(** Get cached response *)
let get_cached_response state ~method_ ~url ?(params=[]) () =
  let key = make_http_key ~method_ ~url ~params () in
  get state key

(** {1 Serialization} *)

(** Entry to JSON *)
let entry_to_json entry =
  `Assoc [
    ("key", `String entry.key);
    ("value", entry.value);
    ("timestamp", `Float entry.timestamp);
    ("ttl", match entry.ttl with Some t -> `Float t | None -> `Null);
  ]

(** Transfer state to JSON for embedding *)
let to_json state =
  `Assoc [
    ("entries", `List (List.map entry_to_json state.entries));
  ]

(** Serialize for HTML embedding *)
let serialize state =
  let json = to_json state in
  let json_str = Yojson.Safe.to_string json in
  (* Escape for safe HTML embedding *)
  json_str
  |> Str.global_replace (Str.regexp "<") "\\u003c"
  |> Str.global_replace (Str.regexp ">") "\\u003e"
  |> Str.global_replace (Str.regexp "&") "\\u0026"

(** Generate transfer state script tag *)
let script_tag state =
  let data = serialize state in
  Printf.sprintf {|<script id="ng-state" type="application/json">%s</script>|} data

(** {1 Loading} *)

(** Parse entry from JSON *)
let entry_from_json json =
  let open Yojson.Safe.Util in
  {
    key = json |> member "key" |> to_string;
    value = json |> member "value";
    timestamp = json |> member "timestamp" |> to_float;
    ttl = json |> member "ttl" |> to_float_option;
  }

(** Load transfer state from JSON *)
let from_json json =
  let open Yojson.Safe.Util in
  let entries = json |> member "entries" |> to_list |> List.map entry_from_json in
  { entries; options = default_cache_options }
