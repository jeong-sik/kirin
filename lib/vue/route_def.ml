(** Vue/Nuxt Route Definitions

    Nuxt-style route definitions with data fetching and middleware. *)

(** {1 Route Parameters} *)

(** Parameter type *)
type param_type =
  | String
  | Int
  | Uuid
  | Slug      (* kebab-case string *)
  | Optional of param_type

(** Route parameter definition *)
type param = {
  name: string;
  param_type: param_type;
  required: bool;
}

(** Create string parameter *)
let string_param name = { name; param_type = String; required = true }

(** Create int parameter *)
let int_param name = { name; param_type = Int; required = true }

(** Create uuid parameter *)
let uuid_param name = { name; param_type = Uuid; required = true }

(** Create optional parameter *)
let optional_param name typ = { name; param_type = Optional typ; required = false }

(** {1 Data Fetching} *)

(** Fetch options (Nuxt useFetch/useAsyncData style) *)
type fetch_options = {
  key: string option;          (* Unique key for caching *)
  server: bool;                (* Fetch on server *)
  lazy_: bool;                 (* Lazy fetch (client-side navigation) *)
  immediate: bool;             (* Fetch immediately *)
  watch: string list;          (* Reactive dependencies *)
  default: Yojson.Safe.t option;  (* Default value *)
  transform: string option;    (* Transform function name *)
  pick: string list;           (* Pick specific fields *)
  cache: bool;                 (* Cache result *)
  dedupe: string option;       (* Dedupe strategy: cancel, defer *)
}

(** Default fetch options *)
let default_fetch_options = {
  key = None;
  server = true;
  lazy_ = false;
  immediate = true;
  watch = [];
  default = None;
  transform = None;
  pick = [];
  cache = true;
  dedupe = Some "cancel";
}

(** {1 Middleware} *)

(** Middleware definition *)
type middleware = {
  name: string;
  global: bool;
  handler: string;  (* Function name or path *)
}

(** Create named middleware *)
let middleware ?(global=false) name handler = { name; global; handler }

(** Create global middleware *)
let global_middleware name handler = { name; global = true; handler }

(** {1 Page Meta} *)

(** Page meta configuration *)
type page_meta = {
  title: string option;
  description: string option;
  layout: string option;
  middleware: string list;
  keep_alive: bool;
  key: string option;
  page_transition: string option;
  layout_transition: string option;
  alias: string list;
  redirect: string option;
}

(** Default page meta *)
let default_page_meta = {
  title = None;
  description = None;
  layout = None;
  middleware = [];
  keep_alive = false;
  key = None;
  page_transition = None;
  layout_transition = None;
  alias = [];
  redirect = None;
}

(** {1 Route Types} *)

(** Loader type *)
type loader_type =
  | UseFetch of string         (* useFetch with URL *)
  | UseAsyncData of string     (* useAsyncData with key *)
  | ServerLoader               (* Server-only data fetching *)
  | UniversalLoader            (* Both server and client *)

(** Route definition *)
type t = {
  path: string;
  name: string option;
  component: string;
  params: param list;
  loader: loader_type option;
  fetch_options: fetch_options;
  middleware: middleware list;
  children: t list;
  meta: page_meta;
  is_layout: bool;
  layout_name: string option;
}

(** {1 Route Creation} *)

(** Create page route *)
let page path =
  let name = String.map (fun c ->
    if c = '/' then '-'
    else if c = '[' || c = ']' then '_'
    else c
  ) path in
  {
    path;
    name = Some (String.sub name 1 (max 0 (String.length name - 1)));
    component = path ^ ".vue";
    params = [];
    loader = None;
    fetch_options = default_fetch_options;
    middleware = [];
    children = [];
    meta = default_page_meta;
    is_layout = false;
    layout_name = None;
  }

(** Create layout route *)
let layout name =
  {
    path = "";
    name = Some ("layout-" ^ name);
    component = "layouts/" ^ name ^ ".vue";
    params = [];
    loader = None;
    fetch_options = default_fetch_options;
    middleware = [];
    children = [];
    meta = default_page_meta;
    is_layout = true;
    layout_name = Some name;
  }

(** {1 Route Modifiers} *)

(** Add parameter *)
let with_param name typ route =
  let param = { name; param_type = typ; required = true } in
  { route with params = param :: route.params }

(** Add useFetch loader *)
let with_fetch url route =
  { route with loader = Some (UseFetch url) }

(** Add useAsyncData loader *)
let with_async_data key route =
  { route with loader = Some (UseAsyncData key) }

(** Add server loader *)
let with_server_loader route =
  { route with loader = Some ServerLoader }

(** Configure fetch options *)
let with_fetch_options opts route =
  { route with fetch_options = opts }

(** Add middleware *)
let with_middleware mw route =
  { route with middleware = mw :: route.middleware }

(** Add child routes *)
let with_children children route =
  { route with children }

(** Set page meta *)
let with_meta meta route =
  { route with meta }

(** Set layout *)
let with_layout name route =
  { route with meta = { route.meta with layout = Some name } }

(** Set title *)
let with_title title route =
  { route with meta = { route.meta with title = Some title } }

(** Add alias *)
let with_alias alias route =
  { route with meta = { route.meta with alias = alias :: route.meta.alias } }

(** Set redirect *)
let with_redirect url route =
  { route with meta = { route.meta with redirect = Some url } }

(** {1 Serialization} *)

(** Param type to string *)
let rec param_type_to_string = function
  | String -> "string"
  | Int -> "int"
  | Uuid -> "uuid"
  | Slug -> "slug"
  | Optional t -> param_type_to_string t ^ "?"

(** Loader type to string *)
let loader_type_to_string = function
  | UseFetch url -> "useFetch:" ^ url
  | UseAsyncData key -> "useAsyncData:" ^ key
  | ServerLoader -> "server"
  | UniversalLoader -> "universal"

(** Route to JSON *)
let rec to_json (route : t) =
  let base = [
    ("path", `String route.path);
    ("component", `String route.component);
    ("params", `List (List.map (fun (p : param) ->
      `Assoc [
        ("name", `String p.name);
        ("type", `String (param_type_to_string p.param_type));
        ("required", `Bool p.required);
      ]
    ) route.params));
    ("isLayout", `Bool route.is_layout);
  ] in
  let with_name = match route.name with
    | Some n -> ("name", `String n) :: base
    | None -> base
  in
  let with_loader = match route.loader with
    | Some l -> ("loader", `String (loader_type_to_string l)) :: with_name
    | None -> with_name
  in
  let with_layout = match route.layout_name with
    | Some l -> ("layoutName", `String l) :: with_loader
    | None -> with_loader
  in
  let with_children = if route.children = [] then with_layout
    else ("children", `List (List.map to_json route.children)) :: with_layout
  in
  let with_middleware = if route.middleware = [] then with_children
    else ("middleware", `List (List.map (fun (m : middleware) -> `String m.name) route.middleware)) :: with_children
  in
  `Assoc with_middleware
