(** Angular Route Definition

    Angular router configuration with SSR render modes. *)

(** {1 Render Modes} *)

(** Server route render mode (Angular v19+) *)
type render_mode =
  | Client      (* CSR - Client-side rendering *)
  | Server      (* SSR - Server-side rendering per request *)
  | Prerender   (* SSG - Static generation at build time *)

(** {1 Route Parameters} *)

(** Parameter type *)
type param_type =
  | String
  | Int
  | Uuid
  | Slug        (* Multi-segment: ** *)
  | Optional of param_type

(** Route parameter *)
type param = {
  name: string;
  param_type: param_type;
}

(** {1 Route Definition} *)

(** Guard definition *)
type guard = {
  guard_name: string;
  can_activate: bool;
  can_deactivate: bool;
  can_match: bool;
}

(** Resolver definition *)
type resolver = {
  resolver_name: string;
  key: string;
}

(** Route definition *)
type t = {
  path: string;
  name: string option;
  component: string option;
  redirect_to: string option;
  path_match: string option;  (* "full" | "prefix" *)
  render_mode: render_mode;
  params: param list;
  guards: guard list;
  resolvers: resolver list;
  children: t list;
  data: (string * Yojson.Safe.t) list;
  title: string option;
  lazy_load: bool;
}

(** {1 Route Building} *)

(** Create empty route *)
let empty path = {
  path;
  name = None;
  component = None;
  redirect_to = None;
  path_match = None;
  render_mode = Server;
  params = [];
  guards = [];
  resolvers = [];
  children = [];
  data = [];
  title = None;
  lazy_load = false;
}

(** Create route with component *)
let route path component =
  { (empty path) with component = Some component }

(** Create redirect route *)
let redirect ~from ~to_ ?(path_match="full") () =
  { (empty from) with
    redirect_to = Some to_;
    path_match = Some path_match;
  }

(** Create lazy-loaded route *)
let lazy_route path load_children =
  { (empty path) with
    component = Some load_children;
    lazy_load = true;
  }

(** {1 Modifiers} *)

(** Set render mode *)
let with_render_mode mode route =
  { route with render_mode = mode }

(** Add parameter *)
let with_param name param_type route =
  { route with params = { name; param_type } :: route.params }

(** Add guard *)
let with_guard name ?(can_activate=true) ?(can_deactivate=false) ?(can_match=false) route =
  let guard = { guard_name = name; can_activate; can_deactivate; can_match } in
  { route with guards = guard :: route.guards }

(** Add resolver *)
let with_resolver ~key resolver_name route =
  let resolver = { resolver_name; key } in
  { route with resolvers = resolver :: route.resolvers }

(** Add children routes *)
let with_children children route =
  { route with children }

(** Add route data *)
let with_data key value route =
  { route with data = (key, value) :: route.data }

(** Set title *)
let with_title title route =
  { route with title = Some title }

(** Set name *)
let with_name name route =
  { route with name = Some name }

(** {1 Convenience} *)

(** Create SSR route *)
let ssr path component =
  route path component |> with_render_mode Server

(** Create SSG route *)
let ssg path component =
  route path component |> with_render_mode Prerender

(** Create CSR route *)
let csr path component =
  route path component |> with_render_mode Client

(** {1 Serialization} *)

(** Render mode to string *)
let render_mode_to_string = function
  | Client -> "client"
  | Server -> "server"
  | Prerender -> "prerender"

(** Param type to string *)
let rec param_type_to_string = function
  | String -> "string"
  | Int -> "number"
  | Uuid -> "string"
  | Slug -> "string[]"
  | Optional inner -> param_type_to_string inner ^ " | undefined"

(** Route to JSON *)
let rec to_json route =
  let base = [
    ("path", `String route.path);
    ("renderMode", `String (render_mode_to_string route.render_mode));
  ] in
  let with_component = match route.component with
    | Some c -> ("component", `String c) :: base
    | None -> base
  in
  let with_redirect = match route.redirect_to with
    | Some r -> ("redirectTo", `String r) :: with_component
    | None -> with_component
  in
  let with_path_match = match route.path_match with
    | Some pm -> ("pathMatch", `String pm) :: with_redirect
    | None -> with_redirect
  in
  let with_title = match route.title with
    | Some t -> ("title", `String t) :: with_path_match
    | None -> with_path_match
  in
  let with_data = if route.data = [] then with_title
    else ("data", `Assoc route.data) :: with_title
  in
  let with_children = if route.children = [] then with_data
    else ("children", `List (List.map to_json route.children)) :: with_data
  in
  `Assoc with_children
