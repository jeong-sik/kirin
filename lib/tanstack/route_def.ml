(** TanStack-style Route Definitions

    Type-safe route definitions with loaders and actions. *)

(** {1 Route Types} *)

(** HTTP methods for actions *)
type http_method =
  | GET
  | POST
  | PUT
  | PATCH
  | DELETE

let method_to_string = function
  | GET -> "GET"
  | POST -> "POST"
  | PUT -> "PUT"
  | PATCH -> "PATCH"
  | DELETE -> "DELETE"

(** Route parameter type *)
type param_type =
  | PString
  | PInt
  | PUuid
  | PSlug  (* alphanumeric with hyphens *)

let param_type_to_string = function
  | PString -> "string"
  | PInt -> "number"
  | PUuid -> "string"  (* UUID is string in TS *)
  | PSlug -> "string"

(** Route parameter definition *)
type param = {
  name: string;
  param_type: param_type;
  optional: bool;
}

(** Create a required string param *)
let string_param name = { name; param_type = PString; optional = false }

(** Create a required int param *)
let int_param name = { name; param_type = PInt; optional = false }

(** Create a required UUID param *)
let uuid_param name = { name; param_type = PUuid; optional = false }

(** Create a required slug param *)
let slug_param name = { name; param_type = PSlug; optional = false }

(** Create an optional param *)
let optional p = { p with optional = true }

(** {1 Loader/Action Types} *)

(** Loader context *)
type 'ctx loader_context = {
  ctx: 'ctx;
  params: (string * string) list;
  search_params: (string * string) list;
}

(** Action context *)
type 'ctx action_context = {
  ctx: 'ctx;
  params: (string * string) list;
  method_: http_method;
  body: Yojson.Safe.t option;
}

(** Loader function type *)
type ('ctx, 'data) loader = 'ctx loader_context -> ('data, string) result

(** Action function type *)
type ('ctx, 'result) action = 'ctx action_context -> ('result, string) result

(** {1 Route Definition} *)

(** Route metadata *)
type meta = {
  title: string option;
  description: string option;
  tags: string list;
}

let default_meta = {
  title = None;
  description = None;
  tags = [];
}

(** A route definition *)
type ('ctx, 'loader_data, 'action_result) t = {
  id: string;
  path: string;
  params: param list;
  meta: meta;
  loader: ('ctx, 'loader_data) loader option;
  action: ('ctx, 'action_result) action option;
  children: string list;  (* child route IDs *)
}

(** {1 Route Builders} *)

(** Create a route *)
let create ~id ~path ?(params = []) ?(meta = default_meta) ?loader ?action ?(children = []) () = {
  id;
  path;
  params;
  meta;
  loader;
  action;
  children;
}

(** Route with loader only *)
let with_loader ~id ~path ?(params = []) ?(meta = default_meta) loader = {
  id;
  path;
  params;
  meta;
  loader = Some loader;
  action = None;
  children = [];
}

(** Route with action only *)
let with_action ~id ~path ?(params = []) ?(meta = default_meta) action = {
  id;
  path;
  params;
  meta;
  loader = None;
  action = Some action;
  children = [];
}

(** Route with both loader and action *)
let with_loader_action ~id ~path ?(params = []) ?(meta = default_meta) ~loader ~action () = {
  id;
  path;
  params;
  meta;
  loader = Some loader;
  action = Some action;
  children = [];
}

(** Layout route (no loader/action, just children) *)
let layout ~id ~path ?(meta = default_meta) children = {
  id;
  path;
  params = [];
  meta;
  loader = None;
  action = None;
  children;
}

(** {1 Path Utilities} *)

(** Parse path segments *)
let parse_path path =
  String.split_on_char '/' path
  |> List.filter (fun s -> s <> "")

(** Check if segment is a param *)
let is_param_segment s =
  String.length s > 0 && String.get s 0 = ':'

(** Check if segment is a catch-all *)
let is_catchall_segment s =
  String.length s > 0 && String.get s 0 = '*'

(** Extract param name from segment *)
let param_name s =
  if is_param_segment s then
    String.sub s 1 (String.length s - 1)
  else if is_catchall_segment s then
    String.sub s 1 (String.length s - 1)
  else
    s

(** Build full path pattern *)
let full_path_pattern route =
  let segments = parse_path route.path in
  let pattern_segments = List.map (fun seg ->
    if is_param_segment seg then ":param"
    else if is_catchall_segment seg then "*"
    else seg
  ) segments in
  "/" ^ String.concat "/" pattern_segments

(** {1 Type Generation} *)

(** Generate TypeScript type for params *)
let params_to_typescript params =
  if params = [] then "Record<string, never>"
  else
    let fields = List.map (fun p ->
      let ts_type = param_type_to_string p.param_type in
      if p.optional then
        Printf.sprintf "  %s?: %s;" p.name ts_type
      else
        Printf.sprintf "  %s: %s;" p.name ts_type
    ) params in
    Printf.sprintf "{\n%s\n}" (String.concat "\n" fields)

(** Generate route ID type *)
let route_id_type routes =
  let ids = List.map (fun r -> Printf.sprintf "  | '%s'" r.id) routes in
  Printf.sprintf "type RouteId =\n%s;" (String.concat "\n" ids)
