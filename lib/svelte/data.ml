(** Svelte Initial Data

    XSS-safe serialization for SvelteKit data hydration. *)

(** {1 Serialization} *)

(** Escape string for safe embedding in HTML script tags *)
let escape_for_html s =
  s
  |> Str.global_replace (Str.regexp "<") "\\u003C"
  |> Str.global_replace (Str.regexp ">") "\\u003E"
  |> Str.global_replace (Str.regexp "&") "\\u0026"
  |> Str.global_replace (Str.regexp "'") "\\u0027"
  |> Str.global_replace (Str.regexp "\"") "\\u0022"

(** Serialize JSON for embedding in script *)
let serialize json =
  Yojson.Safe.to_string json |> escape_for_html

(** {1 Data Embedding} *)

(** SvelteKit data variable name *)
let data_var = "__sveltekit_data"

(** Generate data script tag *)
let script_tag ?(nonce=None) json =
  let data = serialize json in
  match nonce with
  | Some n ->
    Printf.sprintf {|<script nonce="%s">%s=%s</script>|} n data_var data
  | None ->
    Printf.sprintf {|<script>%s=%s</script>|} data_var data

(** {1 Route Data} *)

(** Data for a specific route *)
type route_data = {
  route_id: string;
  data: Yojson.Safe.t;
  uses: string list;  (* Dependencies *)
}

(** Create route data *)
let route_data ~route_id ?(uses=[]) data = {
  route_id;
  data;
  uses;
}

(** {1 Page Data} *)

(** Complete page data structure *)
type page_data = {
  url: string;
  params: (string * string) list;
  route_id: string option;
  status: int;
  error: string option;
  data: Yojson.Safe.t;
  form: Yojson.Safe.t option;
}

(** Create page data *)
let page_data ~url ~params ?route_id ?(status=200) ?error ?(data=`Assoc []) ?form () = {
  url;
  params;
  route_id;
  status;
  error;
  data;
  form;
}

(** Page data to JSON *)
let page_data_to_json pd =
  let base = [
    ("url", `String pd.url);
    ("params", `Assoc (List.map (fun (k, v) -> (k, `String v)) pd.params));
    ("status", `Int pd.status);
    ("data", pd.data);
  ] in
  let with_route = match pd.route_id with
    | Some r -> ("route", `String r) :: base
    | None -> base
  in
  let with_error = match pd.error with
    | Some e -> ("error", `String e) :: with_route
    | None -> with_route
  in
  let with_form = match pd.form with
    | Some f -> ("form", f) :: with_error
    | None -> with_error
  in
  `Assoc with_form

(** {1 Initial Data Structure} *)

(** Complete initial data for SSR *)
type initial_data = {
  type_: string;  (* "data" or "redirect" *)
  nodes: Yojson.Safe.t list;  (* Layout + page data *)
  page_data: page_data option;
}

(** Empty initial data *)
let empty_initial_data = {
  type_ = "data";
  nodes = [];
  page_data = None;
}

(** Create initial data *)
let create_initial_data ?(nodes=[]) ?page_data () = {
  type_ = "data";
  nodes;
  page_data;
}

(** Create redirect data *)
let redirect_data ~location ~status = {
  type_ = "redirect";
  nodes = [`Assoc [("location", `String location); ("status", `Int status)]];
  page_data = None;
}

(** Initial data to JSON *)
let initial_data_to_json id =
  let base = [
    ("type", `String id.type_);
    ("nodes", `List id.nodes);
  ] in
  match id.page_data with
  | Some pd -> ("page", page_data_to_json pd) :: base |> fun l -> `Assoc l
  | None -> `Assoc base

(** Generate all data scripts *)
let all_scripts ?(nonce=None) initial_data =
  let json = initial_data_to_json initial_data in
  script_tag ~nonce json

(** {1 Deferred Data} *)

(** Deferred data chunk *)
type deferred_chunk = {
  chunk_id: string;
  data: Yojson.Safe.t;
}

(** Generate deferred data script *)
let deferred_script chunk =
  Printf.sprintf {|<script>%s.resolve('%s', %s)</script>|}
    data_var
    chunk.chunk_id
    (serialize chunk.data)

(** {1 Form Data} *)

(** Action data structure *)
type action_data = {
  success: bool;
  action_data: Yojson.Safe.t;
}

(** Create action data *)
let action_data ?(success=true) data = {
  success;
  action_data = data;
}

(** Action data to JSON *)
let action_data_to_json ad =
  `Assoc [
    ("success", `Bool ad.success);
    ("data", ad.action_data);
  ]

(** {1 Snapshot} *)

(** Page snapshot (for history restore) *)
type snapshot = {
  scroll_y: int;
  snapshot_data: Yojson.Safe.t option;
}

(** Snapshot to JSON *)
let snapshot_to_json snap =
  let base = [("scrollY", `Int snap.scroll_y)] in
  match snap.snapshot_data with
  | Some d -> ("data", d) :: base |> fun l -> `Assoc l
  | None -> `Assoc base
