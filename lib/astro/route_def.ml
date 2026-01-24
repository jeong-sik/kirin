(** Astro Route Definitions

    Routes for Astro pages with islands support. *)

(** {1 Route Types} *)

(** Route parameter type *)
type param_type =
  | String     (* [slug] *)
  | Rest       (* [...slug] - catch-all *)
  | Optional   (* [...slug] with optional *)

(** Route parameter *)
type param = {
  name: string;
  param_type: param_type;
}

(** Route prerender setting *)
type prerender =
  | Static          (* Pre-render at build time *)
  | OnDemand        (* Render on each request (SSR) *)
  | Hybrid          (* Mix of static and dynamic *)

(** Route definition *)
type t = {
  path: string;
  name: string option;
  prerender: prerender;
  islands: Island.t list;
  layout: string option;
  middleware: string list;
  get_static_paths: bool;  (* Has getStaticPaths function *)
}

(** {1 Route Construction} *)

(** Create static route *)
let static path = {
  path;
  name = None;
  prerender = Static;
  islands = [];
  layout = None;
  middleware = [];
  get_static_paths = false;
}

(** Create SSR route *)
let ssr path = {
  path;
  name = None;
  prerender = OnDemand;
  islands = [];
  layout = None;
  middleware = [];
  get_static_paths = false;
}

(** Create hybrid route *)
let hybrid path = {
  path;
  name = None;
  prerender = Hybrid;
  islands = [];
  layout = None;
  middleware = [];
  get_static_paths = false;
}

(** {1 Route Modifiers} *)

(** Set route name *)
let with_name name route = { route with name = Some name }

(** Add island *)
let with_island island route =
  { route with islands = island :: route.islands }

(** Add multiple islands *)
let with_islands islands route =
  { route with islands = islands @ route.islands }

(** Set layout *)
let with_layout layout route = { route with layout = Some layout }

(** Add middleware *)
let with_middleware mw route =
  { route with middleware = mw :: route.middleware }

(** Enable getStaticPaths *)
let with_static_paths route = { route with get_static_paths = true }

(** {1 Parameter Extraction} *)

(** Extract parameters from path *)
let extract_params path =
  let segments = String.split_on_char '/' path in
  List.filter_map (fun seg ->
    if String.length seg >= 3 && seg.[0] = '[' && seg.[String.length seg - 1] = ']' then
      let inner = String.sub seg 1 (String.length seg - 2) in
      if String.length inner >= 4 && String.sub inner 0 3 = "..." then
        Some { name = String.sub inner 3 (String.length inner - 3); param_type = Rest }
      else
        Some { name = inner; param_type = String }
    else
      None
  ) segments

(** Get parameter names from route *)
let get_params (route : t) =
  let params = extract_params route.path in
  List.map (fun (p : param) -> p.name) params

(** Check if route is dynamic *)
let is_dynamic route =
  String.contains route.path '[' && String.contains route.path ']'

(** Check if route has rest params *)
let has_rest_params route =
  List.exists (fun p -> p.param_type = Rest) (extract_params route.path)

(** {1 Path Matching} *)

(** Match URL against route pattern *)
let matches route url =
  let route_segments = String.split_on_char '/' route.path |> List.filter ((<>) "") in
  let url_segments = String.split_on_char '/' url |> List.filter ((<>) "") in

  let rec match_segments routes urls params =
    match routes, urls with
    | [], [] -> Some (List.rev params)
    | [], _ -> None
    | r :: _, [] ->
      (* Check for optional/rest params *)
      if String.length r >= 5 && String.sub r 0 4 = "[..." then
        Some (List.rev params)
      else
        None
    | r :: rs, u :: us ->
      if String.length r >= 2 && r.[0] = '[' && r.[String.length r - 1] = ']' then
        let inner = String.sub r 1 (String.length r - 2) in
        if String.length inner >= 3 && String.sub inner 0 3 = "..." then
          (* Rest parameter - consume all remaining *)
          let name = String.sub inner 3 (String.length inner - 3) in
          let rest_value = String.concat "/" (u :: us) in
          Some (List.rev ((name, rest_value) :: params))
        else
          (* Regular parameter *)
          match_segments rs us ((inner, u) :: params)
      else if r = u then
        match_segments rs us params
      else
        None
  in
  match_segments route_segments url_segments []

(** {1 Serialization} *)

(** Prerender to string *)
let prerender_to_string = function
  | Static -> "static"
  | OnDemand -> "server"
  | Hybrid -> "hybrid"

(** Route to JSON *)
let to_json route =
  `Assoc [
    ("path", `String route.path);
    ("name", match route.name with Some n -> `String n | None -> `Null);
    ("prerender", `String (prerender_to_string route.prerender));
    ("islands", `List (List.map Island.to_json route.islands));
    ("layout", match route.layout with Some l -> `String l | None -> `Null);
    ("middleware", `List (List.map (fun m -> `String m) route.middleware));
    ("getStaticPaths", `Bool route.get_static_paths);
  ]
