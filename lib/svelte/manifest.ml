(** Svelte Route Manifest

    Build manifest for SvelteKit routes. *)

(** {1 Manifest Types} *)

(** Route entry in manifest *)
type route_entry = {
  id: string;
  pattern: string;
  params: (string * string) list;  (* name, type *)
  page: bool;
  layout: string option;  (* Parent layout ID *)
  error: bool;
  endpoint: bool;  (* +server.ts API route *)
}

(** Layout entry *)
type layout_entry = {
  layout_id: string;
  parent: string option;
  reset: bool;
}

(** Complete manifest *)
type t = {
  routes: route_entry list;
  layouts: layout_entry list;
  matchers: string list;  (* Custom param matchers *)
  hooks: string list;  (* hooks.server.ts, hooks.client.ts *)
}

(** {1 Manifest Building} *)

(** Empty manifest *)
let empty = {
  routes = [];
  layouts = [];
  matchers = [];
  hooks = [];
}

(** Add route to manifest *)
let add_route entry manifest =
  { manifest with routes = entry :: manifest.routes }

(** Add layout to manifest *)
let add_layout entry manifest =
  { manifest with layouts = entry :: manifest.layouts }

(** Add matcher to manifest *)
let add_matcher name manifest =
  { manifest with matchers = name :: manifest.matchers }

(** Add hook to manifest *)
let add_hook name manifest =
  { manifest with hooks = name :: manifest.hooks }

(** {1 Route Helpers} *)

(** Create route entry *)
let route ~id ~pattern ?(params=[]) ?(page=true) ?layout ?(error=false) ?(endpoint=false) () = {
  id;
  pattern;
  params;
  page;
  layout;
  error;
  endpoint;
}

(** Create layout entry *)
let layout ~id ?parent ?(reset=false) () = {
  layout_id = id;
  parent;
  reset;
}

(** {1 From File Router} *)

(** Build manifest from discovered routes *)
let from_discovered_routes discovered =
  let routes = List.filter_map (fun (r : File_router.discovered_route) ->
    let has_page = List.exists (fun f ->
      match f with
      | File_router.Page | File_router.PageServer | File_router.PageTs -> true
      | _ -> false
    ) r.files in

    let has_endpoint = List.exists (fun f ->
      match f with File_router.Server -> true | _ -> false
    ) r.files in

    if not has_page && not has_endpoint then None
    else
      let params = List.filter_map (fun (seg : File_router.segment) ->
        match seg.segment_type with
        | File_router.Param name -> Some (name, "string")
        | File_router.OptionalParam name -> Some (name, "string?")
        | File_router.Rest name -> Some (name, "string[]")
        | File_router.Matcher (name, matcher) -> Some (name, matcher)
        | _ -> None
      ) r.segments in

      Some {
        id = r.dir_path;
        pattern = r.pattern;
        params;
        page = has_page;
        layout = None;  (* Would need layout resolution *)
        error = List.exists (fun f -> f = File_router.Error) r.files;
        endpoint = has_endpoint;
      }
  ) discovered in

  let layouts = List.filter_map (fun (r : File_router.discovered_route) ->
    let has_layout = List.exists (fun f ->
      match f with
      | File_router.Layout | File_router.LayoutServer | File_router.LayoutTs -> true
      | _ -> false
    ) r.files in

    if not has_layout then None
    else Some {
      layout_id = r.dir_path;
      parent = None;  (* Would need hierarchy resolution *)
      reset = false;
    }
  ) discovered in

  { empty with routes; layouts }

(** {1 Serialization} *)

(** Route entry to JSON *)
let route_to_json entry =
  let base = [
    ("id", `String entry.id);
    ("pattern", `String entry.pattern);
    ("params", `List (List.map (fun (name, typ) ->
      `Assoc [("name", `String name); ("type", `String typ)]
    ) entry.params));
    ("page", `Bool entry.page);
    ("error", `Bool entry.error);
    ("endpoint", `Bool entry.endpoint);
  ] in
  let with_layout = match entry.layout with
    | Some l -> ("layout", `String l) :: base
    | None -> base
  in
  `Assoc with_layout

(** Layout entry to JSON *)
let layout_to_json entry =
  let base = [
    ("id", `String entry.layout_id);
    ("reset", `Bool entry.reset);
  ] in
  let with_parent = match entry.parent with
    | Some p -> ("parent", `String p) :: base
    | None -> base
  in
  `Assoc with_parent

(** Manifest to JSON *)
let to_json manifest =
  `Assoc [
    ("routes", `List (List.map route_to_json manifest.routes));
    ("layouts", `List (List.map layout_to_json manifest.layouts));
    ("matchers", `List (List.map (fun m -> `String m) manifest.matchers));
    ("hooks", `List (List.map (fun h -> `String h) manifest.hooks));
  ]

(** Manifest to JSON string *)
let to_json_string ?(pretty=false) manifest =
  let json = to_json manifest in
  if pretty then
    Yojson.Safe.pretty_to_string json
  else
    Yojson.Safe.to_string json

(** {1 Parsing} *)

(** Parse manifest from JSON *)
let of_json json =
  let open Yojson.Safe.Util in
  let routes = json |> member "routes" |> to_list |> List.map (fun r ->
    {
      id = r |> member "id" |> to_string;
      pattern = r |> member "pattern" |> to_string;
      params = r |> member "params" |> to_list |> List.map (fun p ->
        (p |> member "name" |> to_string, p |> member "type" |> to_string)
      );
      page = r |> member "page" |> to_bool;
      layout = r |> member "layout" |> to_string_option;
      error = r |> member "error" |> to_bool;
      endpoint = r |> member "endpoint" |> to_bool;
    }
  ) in
  let layouts = json |> member "layouts" |> to_list |> List.map (fun l ->
    {
      layout_id = l |> member "id" |> to_string;
      parent = l |> member "parent" |> to_string_option;
      reset = l |> member "reset" |> to_bool;
    }
  ) in
  let matchers = json |> member "matchers" |> to_list |> List.map to_string in
  let hooks = json |> member "hooks" |> to_list |> List.map to_string in
  { routes; layouts; matchers; hooks }

(** Load manifest from file *)
let load_file path =
  let ic = open_in path in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  of_json (Yojson.Safe.from_string content)
