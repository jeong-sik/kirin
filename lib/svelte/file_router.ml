(** Svelte File-based Router

    SvelteKit-style file-based routing with +page/+layout conventions. *)

(** {1 File Patterns} *)

(** SvelteKit file types *)
type file_type =
  | Page           (* +page.svelte *)
  | PageServer     (* +page.server.ts *)
  | PageTs         (* +page.ts *)
  | Layout         (* +layout.svelte *)
  | LayoutServer   (* +layout.server.ts *)
  | LayoutTs       (* +layout.ts *)
  | Error          (* +error.svelte *)
  | Server         (* +server.ts - API routes *)

(** Segment type *)
type segment_type =
  | Static of string
  | Param of string         (* [id] *)
  | OptionalParam of string (* [[id]] *)
  | Rest of string          (* [...rest] *)
  | Matcher of string * string  (* [id=integer] *)
  | Group of string         (* (group) - not in URL *)

(** Parsed route segment *)
type segment = {
  raw: string;
  segment_type: segment_type;
  in_url: bool;
}

(** {1 Pattern Parsing} *)

(** Parse a single segment *)
let parse_segment raw =
  let len = String.length raw in
  if len = 0 then
    { raw; segment_type = Static ""; in_url = true }
  else if len >= 2 && raw.[0] = '(' && raw.[len-1] = ')' then
    (* Group: (admin), (marketing) *)
    let group_name = String.sub raw 1 (len - 2) in
    { raw; segment_type = Group group_name; in_url = false }
  else if len >= 4 && raw.[0] = '[' && raw.[1] = '[' && raw.[len-2] = ']' && raw.[len-1] = ']' then
    (* Optional param: [[lang]] *)
    let param_name = String.sub raw 2 (len - 4) in
    { raw; segment_type = OptionalParam param_name; in_url = true }
  else if len >= 5 && raw.[0] = '[' && raw.[1] = '.' && raw.[2] = '.' && raw.[3] = '.' then
    (* Rest param: [...rest] *)
    let param_name = String.sub raw 4 (len - 5) in
    { raw; segment_type = Rest param_name; in_url = true }
  else if len >= 2 && raw.[0] = '[' && raw.[len-1] = ']' then
    (* Param: [id] or [id=matcher] *)
    let inner = String.sub raw 1 (len - 2) in
    (match String.index_opt inner '=' with
     | Some i ->
       let param_name = String.sub inner 0 i in
       let matcher = String.sub inner (i + 1) (String.length inner - i - 1) in
       { raw; segment_type = Matcher (param_name, matcher); in_url = true }
     | None ->
       { raw; segment_type = Param inner; in_url = true })
  else
    { raw; segment_type = Static raw; in_url = true }

(** Parse file path into segments *)
let parse_path path =
  let parts = String.split_on_char '/' path in
  List.filter (fun s -> s <> "") parts
  |> List.map parse_segment

(** {1 Route Pattern Generation} *)

(** Generate URL pattern from segments *)
let pattern_from_segments segments =
  let parts = List.filter_map (fun seg ->
    if not seg.in_url then None
    else match seg.segment_type with
      | Static s -> Some s
      | Param name -> Some (":" ^ name)
      | OptionalParam name -> Some (":" ^ name ^ "?")
      | Rest name -> Some ("*" ^ name)
      | Matcher (name, _) -> Some (":" ^ name)
      | Group _ -> None
  ) segments in
  "/" ^ String.concat "/" parts

(** Generate regex from segments *)
let regex_from_segments segments =
  let parts = List.filter_map (fun seg ->
    if not seg.in_url then None
    else match seg.segment_type with
      | Static s -> Some (Str.quote s)
      | Param _ -> Some "([^/]+)"
      | OptionalParam _ -> Some "([^/]*)"
      | Rest _ -> Some "(.*)"
      | Matcher (_, matcher) ->
        (* Common matchers *)
        (match matcher with
         | "integer" -> Some "([0-9]+)"
         | "uuid" -> Some "([0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12})"
         | _ -> Some "([^/]+)")
      | Group _ -> None
  ) segments in
  "^/" ^ String.concat "/" parts ^ "$"

(** {1 File Discovery} *)

(** Check if file is a SvelteKit route file *)
let classify_file filename =
  match filename with
  | "+page.svelte" -> Some Page
  | "+page.server.ts" | "+page.server.js" -> Some PageServer
  | "+page.ts" | "+page.js" -> Some PageTs
  | "+layout.svelte" -> Some LayoutServer
  | "+layout.server.ts" | "+layout.server.js" -> Some LayoutServer
  | "+layout.ts" | "+layout.js" -> Some LayoutTs
  | "+error.svelte" -> Some Error
  | "+server.ts" | "+server.js" -> Some Server
  | _ -> None

(** Discovered route *)
type discovered_route = {
  dir_path: string;
  segments: segment list;
  pattern: string;
  regex: string;
  files: file_type list;
  group: string option;
}

(** Find group from segments *)
let find_group segments =
  List.find_map (fun seg ->
    match seg.segment_type with
    | Group name -> Some name
    | _ -> None
  ) segments

(** Discover routes from directory listing *)
let discover_route dir_path files =
  let segments = parse_path dir_path in
  let pattern = pattern_from_segments segments in
  let regex = regex_from_segments segments in
  let file_types = List.filter_map classify_file files in
  let group = find_group segments in

  if file_types = [] then None
  else Some {
    dir_path;
    segments;
    pattern;
    regex;
    files = file_types;
    group;
  }

(** {1 Route Matching} *)

(** Extract params from URL using route *)
let extract_params route url =
  let re = Str.regexp route.regex in
  if Str.string_match re url 0 then
    let param_segments = List.filter (fun seg ->
      match seg.segment_type with
      | Param _ | OptionalParam _ | Rest _ | Matcher _ -> true
      | _ -> false
    ) route.segments in

    let rec extract idx acc = function
      | [] -> Some (List.rev acc)
      | seg :: rest ->
        let value = try Str.matched_group (idx + 1) url with _ -> "" in
        let name = match seg.segment_type with
          | Param n | OptionalParam n | Rest n | Matcher (n, _) -> n
          | _ -> ""
        in
        extract (idx + 1) ((name, value) :: acc) rest
    in
    extract 0 [] param_segments
  else
    None

(** Match URL against routes *)
let match_route routes url =
  List.find_map (fun route ->
    match extract_params route url with
    | Some params -> Some (route, params)
    | None -> None
  ) routes

(** {1 Route Sorting} *)

(** Route specificity score (higher = more specific) *)
let specificity route =
  List.fold_left (fun acc seg ->
    match seg.segment_type with
    | Static _ -> acc + 100
    | Matcher _ -> acc + 50
    | Param _ -> acc + 10
    | OptionalParam _ -> acc + 5
    | Rest _ -> acc + 1
    | Group _ -> acc
  ) 0 route.segments

(** Sort routes by specificity (most specific first) *)
let sort_routes routes =
  List.sort (fun a b -> compare (specificity b) (specificity a)) routes

(** {1 Serialization} *)

(** Segment to JSON *)
let segment_to_json seg =
  let type_json = match seg.segment_type with
    | Static s -> `Assoc [("static", `String s)]
    | Param name -> `Assoc [("param", `String name)]
    | OptionalParam name -> `Assoc [("optional", `String name)]
    | Rest name -> `Assoc [("rest", `String name)]
    | Matcher (name, m) -> `Assoc [("matcher", `Assoc [("name", `String name); ("type", `String m)])]
    | Group name -> `Assoc [("group", `String name)]
  in
  `Assoc [
    ("raw", `String seg.raw);
    ("type", type_json);
    ("inUrl", `Bool seg.in_url);
  ]

(** Route to JSON *)
let route_to_json route =
  `Assoc [
    ("path", `String route.dir_path);
    ("pattern", `String route.pattern);
    ("regex", `String route.regex);
    ("segments", `List (List.map segment_to_json route.segments));
    ("files", `List (List.map (fun f ->
      `String (match f with
        | Page -> "page"
        | PageServer -> "pageServer"
        | PageTs -> "pageTs"
        | Layout -> "layout"
        | LayoutServer -> "layoutServer"
        | LayoutTs -> "layoutTs"
        | Error -> "error"
        | Server -> "server")
    ) route.files));
  ]
