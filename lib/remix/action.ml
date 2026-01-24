(** Remix Actions

    Server-side form handling and mutations.
    Actions handle POST/PUT/DELETE requests and return data or redirects. *)

(** {1 Action Types} *)

(** Form data *)
type form_data = (string * string) list

(** Action context *)
type action_context = {
  request_url: string;
  request_method: string;
  params: (string * string) list;
  headers: (string * string) list;
  form_data: form_data;
}

(** Action result *)
type 'a action_result =
  | Success of 'a
  | Redirect of string * int
  | ValidationError of (string * string) list  (* field, message *)
  | ServerError of string

(** {1 Context Creation} *)

(** Create action context from request *)
let context_of_request ~form_data req =
  let url = Kirin.Request.path req in
  let meth = Http.Method.to_string (Kirin.Request.meth req) in
  {
    request_url = url;
    request_method = meth;
    params = [];
    headers = Http.Header.to_list (Kirin.Request.headers req);
    form_data;
  }

(** Add params to context *)
let with_params params ctx =
  { ctx with params }

(** Get form field *)
let field ctx name =
  List.assoc_opt name ctx.form_data

(** Get required form field *)
let required_field ctx name =
  match List.assoc_opt name ctx.form_data with
  | Some v when v <> "" -> Some v
  | _ -> None

(** Get all values for a field (multi-select) *)
let field_values ctx name =
  ctx.form_data
  |> List.filter (fun (k, _) -> k = name)
  |> List.map snd

(** {1 Action Definition} *)

(** Action function type *)
type 'a action = action_context -> 'a action_result

(** {1 Action Combinators} *)

(** Map over action result *)
let map f = function
  | Success x -> Success (f x)
  | Redirect (url, status) -> Redirect (url, status)
  | ValidationError errs -> ValidationError errs
  | ServerError msg -> ServerError msg

(** Validate form data *)
let validate ~validators ctx =
  let errors = validators |> List.filter_map (fun (field_name, validator, message) ->
    match List.assoc_opt field_name ctx.form_data with
    | Some value when validator value -> None
    | _ -> Some (field_name, message)
  ) in
  match errors with
  | [] -> Success ctx.form_data
  | errs -> ValidationError errs

(** {1 Common Validators} *)

(** Not empty *)
let not_empty s = String.length s > 0

(** Min length *)
let min_length n s = String.length s >= n

(** Max length *)
let max_length n s = String.length s <= n

(** Matches regex *)
let matches_regex pattern s =
  try
    let _ = Str.search_forward (Str.regexp pattern) s 0 in
    true
  with Not_found -> false

(** Is email *)
let is_email s =
  matches_regex "^[^@]+@[^@]+\\.[^@]+$" s

(** Is numeric *)
let is_numeric s =
  matches_regex "^[0-9]+$" s

(** {1 Common Actions} *)

(** Always succeed *)
let succeed x _ctx = Success x

(** Always redirect *)
let redirect_action ?(status = 302) url _ctx = Redirect (url, status)

(** Always error *)
let error_action msg _ctx = ServerError msg

(** {1 CSRF Protection} *)

(** Check CSRF token *)
let check_csrf ~get_session_token ctx =
  let form_token = field ctx "_csrf" in
  let session_token = get_session_token ctx in
  match form_token, session_token with
  | Some ft, Some st when ft = st -> true
  | _ -> false

(** Require CSRF token *)
let require_csrf ~get_session_token action ctx =
  if check_csrf ~get_session_token ctx then
    action ctx
  else
    ValidationError [("_csrf", "Invalid CSRF token")]

(** {1 JSON Serialization} *)

(** Result to JSON *)
let result_to_json to_json = function
  | Success x ->
    `Assoc [
      ("ok", `Bool true);
      ("data", to_json x);
    ]
  | Redirect (url, status) ->
    `Assoc [
      ("ok", `Bool true);
      ("redirect", `Assoc [("url", `String url); ("status", `Int status)]);
    ]
  | ValidationError errors ->
    `Assoc [
      ("ok", `Bool false);
      ("errors", `Assoc (List.map (fun (f, m) -> (f, `String m)) errors));
    ]
  | ServerError msg ->
    `Assoc [
      ("ok", `Bool false);
      ("error", `String msg);
    ]

(** {1 Kirin Integration} *)

(** Run action and return response *)
let run_action ~to_json action ctx =
  match action ctx with
  | Success data ->
    Kirin.Response.json (to_json data)
  | Redirect (url, status) ->
    Kirin.Response.redirect ~status:(Http.Status.of_int status) url
  | ValidationError errors ->
    let json = `Assoc [
      ("ok", `Bool false);
      ("errors", `Assoc (List.map (fun (f, m) -> (f, `String m)) errors));
    ] in
    Kirin.Response.json ~status:(Http.Status.of_int 400) json
  | ServerError msg ->
    let json = `Assoc [("ok", `Bool false); ("error", `String msg)] in
    Kirin.Response.json ~status:(Http.Status.of_int 500) json

(** Create action endpoint *)
let endpoint ~parse_form ~to_json action =
  fun req ->
    let form_data = parse_form req in
    let ctx = context_of_request ~form_data req in
    run_action ~to_json action ctx

(** {1 Serialization} *)

(** Context to JSON *)
let context_to_json ctx =
  `Assoc [
    ("url", `String ctx.request_url);
    ("method", `String ctx.request_method);
    ("params", `Assoc (List.map (fun (k, v) -> (k, `String v)) ctx.params));
    ("formData", `Assoc (List.map (fun (k, v) -> (k, `String v)) ctx.form_data));
  ]
