(** Qwik Loaders

    routeLoader$ implementation for data loading. *)

(** {1 Loader Types} *)

(** Loader result *)
type 'a result =
  | LoaderOk of 'a
  | LoaderRedirect of string
  | LoaderError of { status: int; message: string }
  | LoaderNotFound

(** Loader context *)
type context = {
  url: string;
  params: (string * string) list;
  query: (string * string) list;
  headers: (string * string) list;
  cookie: (string * string) list;
  platform: platform_context;
}

and platform_context = {
  env: (string * string) list;
}

(** Loader definition *)
type 'a t = {
  name: string;
  loader: context -> 'a result;
}

(** {1 Loader Construction} *)

(** Create a loader *)
let create ~name loader = { name; loader }

(** Create loader that returns data *)
let data ~name f =
  create ~name (fun ctx -> LoaderOk (f ctx))

(** Create loader with validation *)
let validated ~name ~validate f =
  create ~name (fun ctx ->
    match validate ctx with
    | Some error -> LoaderError { status = 400; message = error }
    | None -> LoaderOk (f ctx)
  )

(** {1 Loader Execution} *)

(** Execute loader *)
let execute loader ctx =
  loader.loader ctx

(** Execute multiple loaders in parallel (simulated) *)
let execute_parallel loaders ctx =
  List.map (fun loader -> (loader.name, execute loader ctx)) loaders

(** {1 Context Helpers} *)

(** Create context from request *)
let context_of_request ~url ~params ~query ~headers ~cookies () = {
  url;
  params;
  query;
  headers;
  cookie = cookies;
  platform = { env = [] };
}

(** Get param from context *)
let get_param ctx name =
  List.assoc_opt name ctx.params

(** Get query param from context *)
let get_query ctx name =
  List.assoc_opt name ctx.query

(** Get header from context *)
let get_header ctx name =
  List.assoc_opt (String.lowercase_ascii name) ctx.headers

(** Get cookie from context *)
let get_cookie ctx name =
  List.assoc_opt name ctx.cookie

(** {1 Result Helpers} *)

(** Map over loader result *)
let map f = function
  | LoaderOk v -> LoaderOk (f v)
  | LoaderRedirect url -> LoaderRedirect url
  | LoaderError e -> LoaderError e
  | LoaderNotFound -> LoaderNotFound

(** Bind loader results *)
let bind f = function
  | LoaderOk v -> f v
  | LoaderRedirect url -> LoaderRedirect url
  | LoaderError e -> LoaderError e
  | LoaderNotFound -> LoaderNotFound

(** Create redirect result *)
let redirect url = LoaderRedirect url

(** Create error result *)
let error ~status ~message = LoaderError { status; message }

(** Create not found result *)
let not_found = LoaderNotFound

(** {1 Serialization} *)

(** Result to JSON *)
let result_to_json value_to_json = function
  | LoaderOk v -> `Assoc [
      ("type", `String "ok");
      ("data", value_to_json v);
    ]
  | LoaderRedirect url -> `Assoc [
      ("type", `String "redirect");
      ("url", `String url);
    ]
  | LoaderError { status; message } -> `Assoc [
      ("type", `String "error");
      ("status", `Int status);
      ("message", `String message);
    ]
  | LoaderNotFound -> `Assoc [
      ("type", `String "notFound");
    ]

(** Context to JSON *)
let context_to_json ctx =
  `Assoc [
    ("url", `String ctx.url);
    ("params", `Assoc (List.map (fun (k, v) -> (k, `String v)) ctx.params));
    ("query", `Assoc (List.map (fun (k, v) -> (k, `String v)) ctx.query));
  ]

(** {1 QRL Integration} *)

(** Create loader QRL *)
let to_qrl ~chunk ~symbol loader =
  Qrl.create ~chunk ~symbol ~dev_name:loader.name ()

(** Loader with QRL *)
type 'a loader_qrl = {
  loader: 'a t;
  qrl: Qrl.t;
}

(** Create loader with QRL *)
let with_qrl ~chunk ~symbol loader = {
  loader;
  qrl = to_qrl ~chunk ~symbol loader;
}
