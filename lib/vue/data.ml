(** Vue/Nuxt Initial Data

    Nuxt-style data serialization for hydration. *)

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

(** {1 Nuxt Data Variables} *)

(** Nuxt payload variable name *)
let payload_var = "__NUXT__"

(** State variable *)
let state_var = "__NUXT_DATA__"

(** {1 Payload Structure} *)

(** Nuxt payload *)
type payload = {
  server_rendered: bool;
  data: (string * Yojson.Safe.t) list;  (* Page data by key *)
  state: (string * Yojson.Safe.t) list;  (* Pinia/Vuex state *)
  config: Yojson.Safe.t option;          (* Runtime config *)
  error: Yojson.Safe.t option;           (* Error if any *)
  route_path: string;
}

(** Create empty payload *)
let empty_payload ~route_path = {
  server_rendered = true;
  data = [];
  state = [];
  config = None;
  error = None;
  route_path;
}

(** Add data to payload *)
let with_data key value payload =
  { payload with data = (key, value) :: payload.data }

(** Add state to payload *)
let with_state key value payload =
  { payload with state = (key, value) :: payload.state }

(** Set config *)
let with_config config payload =
  { payload with config = Some config }

(** Set error *)
let with_error error payload =
  { payload with error = Some error }

(** {1 Script Generation} *)

(** Payload to JSON *)
let payload_to_json payload =
  let base = [
    ("serverRendered", `Bool payload.server_rendered);
    ("path", `String payload.route_path);
    ("data", `Assoc payload.data);
    ("state", `Assoc payload.state);
  ] in
  let with_config = match payload.config with
    | Some c -> ("config", c) :: base
    | None -> base
  in
  let with_error = match payload.error with
    | Some e -> ("error", e) :: with_config
    | None -> with_config
  in
  `Assoc with_error

(** Generate payload script tag *)
let payload_script ?(nonce=None) payload =
  let json = payload_to_json payload in
  let data = serialize json in
  match nonce with
  | Some n ->
    Printf.sprintf {|<script nonce="%s">window.%s=%s</script>|} n payload_var data
  | None ->
    Printf.sprintf {|<script>window.%s=%s</script>|} payload_var data

(** {1 Async Data} *)

(** Async data entry *)
type async_entry = {
  key: string;
  data: Yojson.Safe.t;
  pending: bool;
  error: string option;
}

(** Create async entry *)
let async_entry ~key data = {
  key;
  data;
  pending = false;
  error = None;
}

(** Create pending async entry *)
let async_pending ~key = {
  key;
  data = `Null;
  pending = true;
  error = None;
}

(** Create error async entry *)
let async_error ~key msg = {
  key;
  data = `Null;
  pending = false;
  error = Some msg;
}

(** Async entry to JSON *)
let async_to_json entry =
  let base = [
    ("key", `String entry.key);
    ("data", entry.data);
    ("pending", `Bool entry.pending);
  ] in
  let with_error = match entry.error with
    | Some e -> ("error", `String e) :: base
    | None -> base
  in
  `Assoc with_error

(** {1 Island Hydration} *)

(** Island data for partial hydration *)
type island_data = {
  island_id: string;
  component: string;
  props: Yojson.Safe.t;
  priority: string;  (* "eager", "lazy", "idle", "visible" *)
}

(** Create island data *)
let island ~id ~component ?(props=`Assoc []) ?(priority="eager") () = {
  island_id = id;
  component;
  props;
  priority;
}

(** Island to JSON *)
let island_to_json island =
  `Assoc [
    ("id", `String island.island_id);
    ("component", `String island.component);
    ("props", island.props);
    ("priority", `String island.priority);
  ]

(** Generate island hydration script *)
let island_script islands =
  let json = `List (List.map island_to_json islands) in
  Printf.sprintf {|<script>window.__NUXT_ISLANDS__=%s</script>|} (serialize json)

(** {1 Route Data} *)

(** Route-specific data *)
type route_data = {
  route_id: string;
  params: (string * string) list;
  query: (string * string) list;
  matched: string list;  (* Matched route names *)
}

(** Create route data *)
let route_data ~route_id ?(params=[]) ?(query=[]) ?(matched=[]) () = {
  route_id;
  params;
  query;
  matched;
}

(** Route data to JSON *)
let route_data_to_json rd =
  `Assoc [
    ("routeId", `String rd.route_id);
    ("params", `Assoc (List.map (fun (k, v) -> (k, `String v)) rd.params));
    ("query", `Assoc (List.map (fun (k, v) -> (k, `String v)) rd.query));
    ("matched", `List (List.map (fun m -> `String m) rd.matched));
  ]

(** {1 Full Page Data} *)

(** Complete page data for SSR *)
type page_data = {
  payload: payload;
  route: route_data;
  islands: island_data list;
  async_data: async_entry list;
}

(** Create page data *)
let page_data ~payload ~route ?(islands=[]) ?(async_data=[]) () = {
  payload;
  route;
  islands;
  async_data;
}

(** Generate all data scripts *)
let all_scripts ?(nonce=None) page =
  let payload_s = payload_script ~nonce page.payload in
  let islands_s = if page.islands = [] then ""
    else island_script page.islands
  in
  String.concat "\n" [payload_s; islands_s]
