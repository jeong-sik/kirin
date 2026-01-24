(** Vue/Nuxt Data Loading

    Nuxt-style data fetching with useFetch and useAsyncData patterns. *)

(** {1 Fetch Result} *)

(** Data fetch result *)
type 'a fetch_result = {
  data: 'a option;
  pending: bool;
  error: string option;
  status: string;  (* "idle", "pending", "success", "error" *)
  refresh: unit -> unit;
  clear: unit -> unit;
}

(** Create success result *)
let success data = {
  data = Some data;
  pending = false;
  error = None;
  status = "success";
  refresh = (fun () -> ());
  clear = (fun () -> ());
}

(** Create error result *)
let error msg = {
  data = None;
  pending = false;
  error = Some msg;
  status = "error";
  refresh = (fun () -> ());
  clear = (fun () -> ());
}

(** Create pending result *)
let pending () = {
  data = None;
  pending = true;
  error = None;
  status = "pending";
  refresh = (fun () -> ());
  clear = (fun () -> ());
}

(** {1 Loader Context} *)

(** Nuxt request event context *)
type context = {
  url: string;
  method_: string;
  params: (string * string) list;
  query: (string * string) list;
  headers: (string * string) list;
  cookies: (string * string) list;
  body: Yojson.Safe.t option;
  route_name: string option;
  is_server: bool;
}

(** Create context *)
let create_context ~url ~method_ ?(params=[]) ?(query=[]) ?(headers=[])
    ?(cookies=[]) ?body ?route_name ?(is_server=true) () = {
  url;
  method_;
  params;
  query;
  headers;
  cookies;
  body;
  route_name;
  is_server;
}

(** Get param from context *)
let param ctx name =
  List.assoc_opt name ctx.params

(** Get param or fail *)
let param_exn ctx name =
  match param ctx name with
  | Some v -> v
  | None -> failwith ("Missing param: " ^ name)

(** Get query param *)
let query ctx name =
  List.assoc_opt name ctx.query

(** Get header *)
let header ctx name =
  List.assoc_opt (String.lowercase_ascii name) ctx.headers

(** Get cookie *)
let cookie ctx name =
  List.assoc_opt name ctx.cookies

(** {1 Loader Results} *)

(** Loader result type *)
type load_result =
  | LoadData of Yojson.Safe.t
  | LoadRedirect of int * string
  | LoadError of int * string
  | LoadNotFound

(** Return data *)
let data json = LoadData json

(** Return redirect *)
let redirect ?(status=302) url = LoadRedirect (status, url)

(** Return error *)
let error_result status message = LoadError (status, message)

(** Return not found *)
let not_found = LoadNotFound

(** {1 useFetch Simulation} *)

(** Fetch options *)
type fetch_options = {
  key: string option;
  method_: string;
  query: (string * string) list;
  headers: (string * string) list;
  body: Yojson.Safe.t option;
  base_url: string option;
  server: bool;
  lazy_: bool;
  immediate: bool;
  default: Yojson.Safe.t option;
  transform: (Yojson.Safe.t -> Yojson.Safe.t) option;
  pick: string list;
  watch: bool;
  deep: bool;
  dedupe: string;  (* "cancel" | "defer" *)
  timeout: int;
  retry: int;
  retry_delay: int;
  on_request: (context -> unit) option;
  on_response: (Yojson.Safe.t -> unit) option;
  on_error: (string -> unit) option;
}

(** Default fetch options *)
let default_fetch_options = {
  key = None;
  method_ = "GET";
  query = [];
  headers = [];
  body = None;
  base_url = None;
  server = true;
  lazy_ = false;
  immediate = true;
  default = None;
  transform = None;
  pick = [];
  watch = true;
  deep = true;
  dedupe = "cancel";
  timeout = 30000;
  retry = 0;
  retry_delay = 1000;
  on_request = None;
  on_response = None;
  on_error = None;
}

(** {1 useAsyncData Simulation} *)

(** Async data options *)
type async_options = {
  key: string;
  server: bool;
  lazy_: bool;
  immediate: bool;
  default: Yojson.Safe.t option;
  transform: (Yojson.Safe.t -> Yojson.Safe.t) option;
  pick: string list;
  watch: string list;  (* Keys to watch *)
  deep: bool;
  dedupe: string;
  get_cached_data: (string -> Yojson.Safe.t option) option;
}

(** Default async options *)
let default_async_options key = {
  key;
  server = true;
  lazy_ = false;
  immediate = true;
  default = None;
  transform = None;
  pick = [];
  watch = [];
  deep = true;
  dedupe = "cancel";
  get_cached_data = None;
}

(** {1 Server Handler} *)

(** Server handler type *)
type server_handler = context -> load_result

(** Wrap handler with error handling *)
let with_error_handling handler ctx =
  try
    handler ctx
  with
  | Failure msg -> LoadError (500, msg)
  | _ -> LoadError (500, "Internal server error")

(** {1 Caching} *)

(** Cache entry *)
type cache_entry = {
  value: Yojson.Safe.t;
  timestamp: float;
  ttl: int;
}

(** Check if cache entry is valid *)
let is_valid entry =
  let now = Unix.gettimeofday () in
  now -. entry.timestamp < float_of_int entry.ttl

(** {1 Serialization} *)

(** Context to JSON *)
let context_to_json ctx =
  `Assoc [
    ("url", `String ctx.url);
    ("method", `String ctx.method_);
    ("params", `Assoc (List.map (fun (k, v) -> (k, `String v)) ctx.params));
    ("query", `Assoc (List.map (fun (k, v) -> (k, `String v)) ctx.query));
    ("isServer", `Bool ctx.is_server);
  ]

(** Load result to JSON *)
let result_to_json = function
  | LoadData data ->
    `Assoc [("type", `String "data"); ("data", data)]
  | LoadRedirect (status, url) ->
    `Assoc [
      ("type", `String "redirect");
      ("status", `Int status);
      ("location", `String url);
    ]
  | LoadError (status, msg) ->
    `Assoc [
      ("type", `String "error");
      ("status", `Int status);
      ("message", `String msg);
    ]
  | LoadNotFound ->
    `Assoc [("type", `String "notFound")]

(** Fetch options to JSON *)
let fetch_options_to_json opts =
  `Assoc [
    ("method", `String opts.method_);
    ("server", `Bool opts.server);
    ("lazy", `Bool opts.lazy_);
    ("immediate", `Bool opts.immediate);
    ("dedupe", `String opts.dedupe);
    ("timeout", `Int opts.timeout);
    ("retry", `Int opts.retry);
  ]
