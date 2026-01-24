(** Remix Loaders

    Server-side data fetching for routes.
    Loaders run on the server before rendering and provide data to components. *)

(** {1 Loader Types} *)

(** Loader context from request *)
type loader_context = {
  request_url: string;
  request_method: string;
  params: (string * string) list;
  headers: (string * string) list;
  cookies: (string * string) list;
}

(** Loader result *)
type 'a loader_result =
  | Data of 'a
  | Redirect of string * int  (* url, status code *)
  | NotFound
  | ServerError of string

(** JSON loader result for serialization *)
type json_result = {
  data: Yojson.Safe.t option;
  redirect: (string * int) option;
  error: string option;
  status: int;
}

(** {1 Loader Definition} *)

(** Loader function type *)
type 'a loader = loader_context -> 'a loader_result

(** {1 Context Creation} *)

(** Create context from Kirin request *)
let context_of_request req =
  let url = Kirin.Request.path req in
  let meth = Http.Method.to_string (Kirin.Request.meth req) in
  {
    request_url = url;
    request_method = meth;
    params = [];  (* Would be populated from route matching *)
    headers = Http.Header.to_list (Kirin.Request.headers req);
    cookies = [];  (* Would parse Cookie header *)
  }

(** Add params to context *)
let with_params params ctx =
  { ctx with params }

(** Get param from context *)
let param ctx name =
  List.assoc_opt name ctx.params

(** Get header from context *)
let header ctx name =
  List.assoc_opt name ctx.headers

(** {1 Loader Combinators} *)

(** Map over loader result *)
let map f = function
  | Data x -> Data (f x)
  | Redirect (url, status) -> Redirect (url, status)
  | NotFound -> NotFound
  | ServerError msg -> ServerError msg

(** Bind loader results *)
let bind f = function
  | Data x -> f x
  | Redirect (url, status) -> Redirect (url, status)
  | NotFound -> NotFound
  | ServerError msg -> ServerError msg

(** Combine two loaders in parallel *)
let parallel loader1 loader2 ctx =
  match loader1 ctx, loader2 ctx with
  | Data a, Data b -> Data (a, b)
  | Redirect (url, status), _ -> Redirect (url, status)
  | _, Redirect (url, status) -> Redirect (url, status)
  | NotFound, _ | _, NotFound -> NotFound
  | ServerError msg, _ -> ServerError msg
  | _, ServerError msg -> ServerError msg

(** Optional loader - returns None instead of NotFound *)
let optional loader ctx =
  match loader ctx with
  | Data x -> Data (Some x)
  | NotFound -> Data None
  | Redirect (url, status) -> Redirect (url, status)
  | ServerError msg -> ServerError msg

(** {1 Common Loaders} *)

(** Always return data *)
let pure x _ctx = Data x

(** Always redirect *)
let redirect ?(status = 302) url _ctx = Redirect (url, status)

(** Always not found *)
let not_found _ctx = NotFound

(** Always error *)
let error msg _ctx = ServerError msg

(** Require authentication *)
let require_auth ~check_auth ~login_url loader ctx =
  if check_auth ctx then
    loader ctx
  else
    Redirect (login_url, 302)

(** {1 JSON Serialization} *)

(** Convert result to JSON response *)
let to_json_result = function
  | Data json ->
    { data = Some json; redirect = None; error = None; status = 200 }
  | Redirect (url, status) ->
    { data = None; redirect = Some (url, status); error = None; status }
  | NotFound ->
    { data = None; redirect = None; error = Some "Not found"; status = 404 }
  | ServerError msg ->
    { data = None; redirect = None; error = Some msg; status = 500 }

(** JSON result to Yojson *)
let json_result_to_yojson r =
  `Assoc [
    ("data", match r.data with Some d -> d | None -> `Null);
    ("redirect", match r.redirect with
      | Some (url, status) -> `Assoc [("url", `String url); ("status", `Int status)]
      | None -> `Null);
    ("error", match r.error with Some e -> `String e | None -> `Null);
    ("status", `Int r.status);
  ]

(** {1 Kirin Integration} *)

(** Run loader and return Kirin response *)
let run_loader loader ctx =
  match loader ctx with
  | Data json ->
    Kirin.Response.json json
  | Redirect (url, status) ->
    Kirin.Response.redirect ~status:(Http.Status.of_int status) url
  | NotFound ->
    Kirin.Response.not_found ()
  | ServerError msg ->
    Kirin.Response.json ~status:(Http.Status.of_int 500)
      (`Assoc [("error", `String msg)])

(** Create loader endpoint *)
let endpoint loader =
  fun req ->
    let ctx = context_of_request req in
    run_loader loader ctx

(** {1 Deferred Data} *)

(** Deferred loader for streaming *)
type 'a deferred =
  | Resolved of 'a
  | Pending of (unit -> 'a loader_result)

(** Create deferred loader *)
let defer f = Pending f

(** Resolve deferred *)
let resolve = function
  | Resolved x -> Data x
  | Pending f -> f ()

(** {1 Serialization} *)

(** Context to JSON *)
let context_to_json ctx =
  `Assoc [
    ("url", `String ctx.request_url);
    ("method", `String ctx.request_method);
    ("params", `Assoc (List.map (fun (k, v) -> (k, `String v)) ctx.params));
  ]
