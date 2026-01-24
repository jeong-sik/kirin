(** Qwik Route Definitions

    QwikCity-style route definitions with loaders and actions. *)

(** {1 Route Types} *)

(** Route render mode *)
type render_mode =
  | SSR             (* Server-side rendering *)
  | Static          (* Static site generation *)
  | SPA             (* Single page application (client only) *)

(** Route definition *)
type t = {
  path: string;
  name: string option;
  render_mode: render_mode;
  layout: string option;        (* Layout component *)
  loaders: string list;         (* routeLoader$ names *)
  actions: string list;         (* routeAction$ names *)
  head: head_config option;     (* Head configuration *)
  on_request: string option;    (* onRequest handler *)
  on_get: string option;        (* onGet handler *)
  on_post: string option;       (* onPost handler *)
  children: t list;
}

and head_config = {
  title: string option;
  meta: (string * string) list;
  links: link list;
}

and link = {
  rel: string;
  href: string;
}

(** {1 Route Construction} *)

(** Empty route *)
let empty path = {
  path;
  name = None;
  render_mode = SSR;
  layout = None;
  loaders = [];
  actions = [];
  head = None;
  on_request = None;
  on_get = None;
  on_post = None;
  children = [];
}

(** Create SSR route *)
let ssr path = empty path

(** Create static route *)
let static path =
  { (empty path) with render_mode = Static }

(** Create SPA route *)
let spa path =
  { (empty path) with render_mode = SPA }

(** {1 Route Modifiers} *)

(** Set route name *)
let with_name name route =
  { route with name = Some name }

(** Set layout *)
let with_layout layout route =
  { route with layout = Some layout }

(** Add loader *)
let with_loader loader route =
  { route with loaders = loader :: route.loaders }

(** Add action *)
let with_action action route =
  { route with actions = action :: route.actions }

(** Set head config *)
let with_head ?(title=None) ?(meta=[]) ?(links=[]) route =
  { route with head = Some { title; meta; links } }

(** Set onRequest handler *)
let with_on_request handler route =
  { route with on_request = Some handler }

(** Set onGet handler *)
let with_on_get handler route =
  { route with on_get = Some handler }

(** Set onPost handler *)
let with_on_post handler route =
  { route with on_post = Some handler }

(** Add children routes *)
let with_children children route =
  { route with children }

(** {1 Route Parameters} *)

(** Route parameter type *)
type param_type =
  | String
  | Int
  | Slug          (* [...catchAll] *)
  | Optional of param_type  (* [optional] *)

(** Route parameter *)
type param = {
  param_name: string;
  param_type: param_type;
}

(** Extract params from path *)
let extract_params path =
  let segments = String.split_on_char '/' path in
  List.filter_map (fun seg ->
    if String.length seg > 0 then
      if seg.[0] = '[' && seg.[String.length seg - 1] = ']' then
        let inner = String.sub seg 1 (String.length seg - 2) in
        if String.length inner > 3 && String.sub inner 0 3 = "..." then
          (* [...catchAll] *)
          Some { param_name = String.sub inner 3 (String.length inner - 3); param_type = Slug }
        else if inner.[0] = '[' && inner.[String.length inner - 1] = ']' then
          (* [[optional]] *)
          let name = String.sub inner 1 (String.length inner - 2) in
          Some { param_name = name; param_type = Optional String }
        else
          Some { param_name = inner; param_type = String }
      else
        None
    else
      None
  ) segments

(** {1 Path Matching} *)

(** Check if path matches route *)
let matches route request_path =
  (* Simple matching - in real impl would use proper router *)
  let route_segs = String.split_on_char '/' route.path |> List.filter (fun s -> s <> "") in
  let path_segs = String.split_on_char '/' request_path |> List.filter (fun s -> s <> "") in

  let rec match_segs route_segs path_segs =
    match route_segs, path_segs with
    | [], [] -> true
    | [], _ -> false
    | rs :: rrest, ps :: prest ->
      if String.length rs > 0 && rs.[0] = '[' then
        (* Dynamic segment *)
        if String.length rs > 4 && String.sub rs 1 3 = "..." then
          (* Catch-all matches rest *)
          true
        else
          match_segs rrest prest
      else if rs = ps then
        match_segs rrest prest
      else
        false
    | _ :: _, [] ->
      (* Check if remaining are optional *)
      List.for_all (fun s ->
        String.length s > 2 &&
        s.[0] = '[' && s.[1] = '[' &&
        s.[String.length s - 1] = ']' && s.[String.length s - 2] = ']'
      ) route_segs
  in
  match_segs route_segs path_segs

(** {1 Serialization} *)

(** Render mode to string *)
let render_mode_to_string = function
  | SSR -> "ssr"
  | Static -> "static"
  | SPA -> "spa"

(** Route to JSON *)
let rec to_json route =
  `Assoc [
    ("path", `String route.path);
    ("name", match route.name with Some n -> `String n | None -> `Null);
    ("renderMode", `String (render_mode_to_string route.render_mode));
    ("layout", match route.layout with Some l -> `String l | None -> `Null);
    ("loaders", `List (List.map (fun l -> `String l) route.loaders));
    ("actions", `List (List.map (fun a -> `String a) route.actions));
    ("head", match route.head with
      | Some h -> `Assoc [
          ("title", match h.title with Some t -> `String t | None -> `Null);
          ("meta", `Assoc h.meta);
          ("links", `List (List.map (fun l -> `Assoc [
            ("rel", `String l.rel);
            ("href", `String l.href);
          ]) h.links));
        ]
      | None -> `Null);
    ("children", `List (List.map to_json route.children));
  ]

(** {1 Convenience Builders} *)

(** Create index route *)
let index () = ssr ""

(** Create layout route *)
let layout name children =
  { (empty "") with layout = Some name; children }

(** Create catch-all route *)
let catch_all () = ssr "[...catchAll]"

(** Create optional segment route *)
let optional segment =
  ssr (Printf.sprintf "[[%s]]" segment)
