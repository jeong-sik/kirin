(** Vue/Nuxt Route Manifest

    Build manifest for Nuxt routes. *)

(** {1 Manifest Types} *)

(** Route entry in manifest *)
type route_entry = {
  id: string;
  path: string;
  pattern: string;
  name: string option;
  component: string;
  params: (string * string) list;  (* name, type *)
  layout: string option;
  middleware: string list;
  redirect: string option;
  alias: string list;
  is_page: bool;
  is_layout: bool;
}

(** Layout entry *)
type layout_entry = {
  layout_id: string;
  name: string;
  component: string;
  is_default: bool;
}

(** Server route entry *)
type server_route_entry = {
  route_id: string;
  path: string;
  method_: string option;
  handler: string;
}

(** Plugin entry *)
type plugin_entry = {
  name: string;
  src: string;
  mode: string;  (* "all", "client", "server" *)
}

(** Complete manifest *)
type t = {
  routes: route_entry list;
  layouts: layout_entry list;
  server_routes: server_route_entry list;
  plugins: plugin_entry list;
  middleware: string list;
}

(** {1 Manifest Building} *)

(** Empty manifest *)
let empty = {
  routes = [];
  layouts = [];
  server_routes = [];
  plugins = [];
  middleware = [];
}

(** Add route to manifest *)
let add_route entry manifest =
  { manifest with routes = entry :: manifest.routes }

(** Add layout to manifest *)
let add_layout entry manifest =
  { manifest with layouts = entry :: manifest.layouts }

(** Add server route to manifest *)
let add_server_route entry manifest =
  { manifest with server_routes = entry :: manifest.server_routes }

(** Add plugin to manifest *)
let add_plugin entry manifest =
  { manifest with plugins = entry :: manifest.plugins }

(** Add middleware to manifest *)
let add_middleware name manifest =
  { manifest with middleware = name :: manifest.middleware }

(** {1 Route Helpers} *)

(** Create route entry *)
let create_route ~id ~path ~pattern ~component ?(name=None) ?(params=[])
    ?(layout=None) ?(middleware=[]) ?(redirect=None) ?(alias=[])
    ?(is_page=true) ?(is_layout=false) () = {
  id;
  path;
  pattern;
  name;
  component;
  params;
  layout;
  middleware;
  redirect;
  alias;
  is_page;
  is_layout;
}

(** Create layout entry *)
let create_layout ~name ~component ?(is_default=false) () = {
  layout_id = "layout-" ^ name;
  name;
  component;
  is_default;
}

(** Create server route entry *)
let create_server_route ~path ~handler ?method_ () = {
  route_id = "server-" ^ path;
  path;
  method_;
  handler;
}

(** {1 Route Lookup} *)

(** Find route by path *)
let find_route path (manifest : t) : route_entry option =
  List.find_opt (fun (r : route_entry) -> r.path = path) manifest.routes

(** Find route by name *)
let find_route_by_name name (manifest : t) : route_entry option =
  List.find_opt (fun (r : route_entry) -> r.name = Some name) manifest.routes

(** Find layout by name *)
let find_layout name (manifest : t) : layout_entry option =
  List.find_opt (fun (l : layout_entry) -> l.name = name) manifest.layouts

(** Get default layout *)
let default_layout manifest =
  List.find_opt (fun l -> l.is_default) manifest.layouts

(** {1 Serialization} *)

(** Route entry to JSON *)
let route_to_json entry =
  let base = [
    ("id", `String entry.id);
    ("path", `String entry.path);
    ("pattern", `String entry.pattern);
    ("component", `String entry.component);
    ("isPage", `Bool entry.is_page);
    ("isLayout", `Bool entry.is_layout);
  ] in
  let with_name = match entry.name with
    | Some n -> ("name", `String n) :: base
    | None -> base
  in
  let with_params = if entry.params = [] then with_name
    else ("params", `Assoc (List.map (fun (k, v) ->
      (k, `String v)) entry.params)) :: with_name
  in
  let with_layout = match entry.layout with
    | Some l -> ("layout", `String l) :: with_params
    | None -> with_params
  in
  let with_middleware = if entry.middleware = [] then with_layout
    else ("middleware", `List (List.map (fun m -> `String m) entry.middleware)) :: with_layout
  in
  let with_redirect = match entry.redirect with
    | Some r -> ("redirect", `String r) :: with_middleware
    | None -> with_middleware
  in
  let with_alias = if entry.alias = [] then with_redirect
    else ("alias", `List (List.map (fun a -> `String a) entry.alias)) :: with_redirect
  in
  `Assoc with_alias

(** Layout entry to JSON *)
let layout_to_json entry =
  `Assoc [
    ("id", `String entry.layout_id);
    ("name", `String entry.name);
    ("component", `String entry.component);
    ("isDefault", `Bool entry.is_default);
  ]

(** Server route entry to JSON *)
let server_route_to_json entry =
  let base = [
    ("id", `String entry.route_id);
    ("path", `String entry.path);
    ("handler", `String entry.handler);
  ] in
  let with_method = match entry.method_ with
    | Some m -> ("method", `String m) :: base
    | None -> base
  in
  `Assoc with_method

(** Plugin entry to JSON *)
let plugin_to_json entry =
  `Assoc [
    ("name", `String entry.name);
    ("src", `String entry.src);
    ("mode", `String entry.mode);
  ]

(** Manifest to JSON *)
let to_json manifest =
  `Assoc [
    ("routes", `List (List.map route_to_json manifest.routes));
    ("layouts", `List (List.map layout_to_json manifest.layouts));
    ("serverRoutes", `List (List.map server_route_to_json manifest.server_routes));
    ("plugins", `List (List.map plugin_to_json manifest.plugins));
    ("middleware", `List (List.map (fun m -> `String m) manifest.middleware));
  ]

(** {1 Loading} *)

(** Load manifest from JSON *)
let from_json json =
  let open Yojson.Safe.Util in
  let routes = json |> member "routes" |> to_list |> List.map (fun r ->
    {
      id = r |> member "id" |> to_string;
      path = r |> member "path" |> to_string;
      pattern = r |> member "pattern" |> to_string;
      name = r |> member "name" |> to_string_option;
      component = r |> member "component" |> to_string;
      params = [];  (* Simplified *)
      layout = r |> member "layout" |> to_string_option;
      middleware = [];
      redirect = r |> member "redirect" |> to_string_option;
      alias = [];
      is_page = r |> member "isPage" |> to_bool_option |> Option.value ~default:true;
      is_layout = r |> member "isLayout" |> to_bool_option |> Option.value ~default:false;
    }
  ) in
  { empty with routes }

(** Load manifest from file *)
let load_file path =
  let content = Kirin.Fs_compat.load path in
  from_json (Yojson.Safe.from_string content)

(** From discovered routes *)
let from_discovered_routes routes =
  let entries = List.map (fun (r : File_router.discovered_route) ->
    create_route
      ~id:r.route_path
      ~path:r.route_path
      ~pattern:(File_router.path_to_pattern r.route_path)
      ~component:r.file_path
      ~is_page:(not r.is_index || r.route_path = "/")
      ()
  ) routes in
  { empty with routes = entries }
