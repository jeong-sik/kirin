(** Svelte Route Definitions

    SvelteKit-style route definitions with +page/+layout patterns. *)

(** {1 Route Types} *)

(** Route parameter types *)
type param_type =
  | String
  | Int
  | Uuid
  | Slug  (* Multiple segments *)
  | Optional of param_type
  | Rest  (* [...rest] catch-all *)
  | Matcher of string  (* Custom matcher name *)

(** Route parameter *)
type param = {
  name: string;
  param_type: param_type;
  required: bool;
}

(** Load function result *)
type load_result =
  | Data of Yojson.Safe.t
  | Redirect of string * int
  | Error of int * string
  | NotFound

(** Action result *)
type action_result =
  | Success of Yojson.Safe.t
  | Fail of int * Yojson.Safe.t  (* status, errors *)
  | ActionRedirect of string
  | ActionError of string

(** Form action definition *)
type action = {
  action_name: string option;  (* None = default action *)
  handler: url:string -> form_data:Yojson.Safe.t -> action_result;
}

(** Load function definition *)
type loader = {
  load_server: bool;  (* +page.server.ts vs +page.ts *)
  depends: string list;  (* invalidation dependencies *)
  handler: url:string -> params:(string * string) list -> load_result;
}

(** Layout definition *)
type layout = {
  layout_id: string;
  loader: loader option;
  reset: bool;  (* @reset layout boundary *)
}

(** Page definition *)
type page = {
  loader: loader option;
  actions: action list;
  prerender: bool option;  (* Static prerendering *)
  ssr: bool;  (* SSR enabled *)
  csr: bool;  (* CSR enabled *)
}

(** Route definition *)
type t = {
  path: string;
  pattern: string;
  params: param list;
  page: page option;
  layout: layout option;
  error: bool;  (* +error.svelte *)
  group: string option;  (* (group) folder *)
}

(** {1 Constructors} *)

(** Create a page route *)
let page ?(prerender=None) ?(ssr=true) ?(csr=true) ?loader ?(actions=[]) path =
  let page = {
    loader;
    actions;
    prerender;
    ssr;
    csr;
  } in
  {
    path;
    pattern = path;
    params = [];
    page = Some page;
    layout = None;
    error = false;
    group = None;
  }

(** Create a layout route *)
let layout ?(reset=false) ?loader path =
  let layout = {
    layout_id = path;
    loader;
    reset;
  } in
  {
    path;
    pattern = path;
    params = [];
    page = None;
    layout = Some layout;
    error = false;
    group = None;
  }

(** Create an error route *)
let error path = {
  path;
  pattern = path;
  params = [];
  page = None;
  layout = None;
  error = true;
  group = None;
}

(** {1 Route Modifiers} *)

(** Add parameter *)
let with_param name param_type route =
  let param = { name; param_type; required = true } in
  { route with params = param :: route.params }

(** Add optional parameter *)
let with_optional_param name param_type route =
  let param = { name; param_type = Optional param_type; required = false } in
  { route with params = param :: route.params }

(** Add rest parameter (catch-all) *)
let with_rest_param name route =
  let param = { name; param_type = Rest; required = false } in
  { route with params = param :: route.params }

(** Set route group *)
let in_group group route =
  { route with group = Some group }

(** {1 Load Function Helpers} *)

(** Create server-side loader *)
let server_loader ?(depends=[]) handler = {
  load_server = true;
  depends;
  handler;
}

(** Create universal loader *)
let universal_loader ?(depends=[]) handler = {
  load_server = false;
  depends;
  handler;
}

(** {1 Action Helpers} *)

(** Create default action *)
let default_action handler = {
  action_name = None;
  handler;
}

(** Create named action *)
let named_action name handler = {
  action_name = Some name;
  handler;
}

(** {1 Load Result Helpers} *)

(** Success with data *)
let data json = Data json

(** Redirect response *)
let redirect ?(status=303) url = Redirect (url, status)

(** Error response *)
let error_response status message = Error (status, message)

(** Not found *)
let not_found = NotFound

(** {1 Action Result Helpers} *)

(** Success with data *)
let success ?(data=`Assoc []) () = Success data

(** Validation failure *)
let fail ?(status=400) errors = Fail (status, errors)

(** Redirect after action *)
let action_redirect url = ActionRedirect url

(** Server error *)
let action_error msg = ActionError msg

(** {1 Serialization} *)

(** Param type to JSON *)
let rec param_type_to_json = function
  | String -> `String "string"
  | Int -> `String "int"
  | Uuid -> `String "uuid"
  | Slug -> `String "slug"
  | Optional t -> `Assoc [("optional", param_type_to_json t)]
  | Rest -> `String "rest"
  | Matcher name -> `Assoc [("matcher", `String name)]

(** Route to JSON *)
let to_json route =
  let params = `List (List.map (fun p ->
    `Assoc [
      ("name", `String p.name);
      ("type", param_type_to_json p.param_type);
      ("required", `Bool p.required);
    ]
  ) route.params) in

  let base = [
    ("path", `String route.path);
    ("pattern", `String route.pattern);
    ("params", params);
    ("error", `Bool route.error);
  ] in

  let with_group = match route.group with
    | Some g -> ("group", `String g) :: base
    | None -> base
  in

  let with_page = match route.page with
    | Some p ->
      let page_json = `Assoc [
        ("hasLoader", `Bool (Option.is_some p.loader));
        ("actionsCount", `Int (List.length p.actions));
        ("ssr", `Bool p.ssr);
        ("csr", `Bool p.csr);
      ] in
      ("page", page_json) :: with_group
    | None -> with_group
  in

  let with_layout = match route.layout with
    | Some l ->
      let layout_json = `Assoc [
        ("id", `String l.layout_id);
        ("hasLoader", `Bool (Option.is_some l.loader));
        ("reset", `Bool l.reset);
      ] in
      ("layout", layout_json) :: with_page
    | None -> with_page
  in

  `Assoc with_layout
