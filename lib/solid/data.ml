(** Solid.js Initial Data

    Server data serialization for hydration. *)

(** {1 Data Serialization} *)

(** Escape string for safe embedding in HTML/JS *)
let escape_for_js str =
  str
  |> Str.global_replace (Str.regexp_string "\\") "\\\\"
  |> Str.global_replace (Str.regexp_string "\"") "\\\""
  |> Str.global_replace (Str.regexp_string "'") "\\'"
  |> Str.global_replace (Str.regexp_string "\n") "\\n"
  |> Str.global_replace (Str.regexp_string "\r") "\\r"
  |> Str.global_replace (Str.regexp_string "<") "\\u003c"
  |> Str.global_replace (Str.regexp_string ">") "\\u003e"
  |> Str.global_replace (Str.regexp_string "&") "\\u0026"

(** Serialize JSON for HTML embedding (XSS-safe) *)
let serialize json =
  let str = Yojson.Safe.to_string json in
  escape_for_js str

(** Generate script tag with initial data *)
let script_tag ?(var = "__SOLID_DATA__") json =
  Printf.sprintf {|<script>window.%s=%s;</script>|} var (serialize json)

(** Generate script tag with route data *)
let route_data_tag ~route_id json =
  let data = `Assoc [
    ("routeId", `String route_id);
    ("data", json);
    ("timestamp", `Int (int_of_float (Unix.time ())));
  ] in
  script_tag ~var:"__ROUTE_DATA__" data

(** {1 Resource Data} *)

(** Resource state for Solid's createResource *)
type 'a resource_state =
  | Pending
  | Ready of 'a
  | Errored of string
  | Refreshing of 'a  (* Has old data, fetching new *)

(** Serialize resource state *)
let serialize_resource serialize_data = function
  | Pending -> `Assoc [("state", `String "pending")]
  | Ready data -> `Assoc [("state", `String "ready"); ("data", serialize_data data)]
  | Errored msg -> `Assoc [("state", `String "errored"); ("error", `String msg)]
  | Refreshing data -> `Assoc [("state", `String "refreshing"); ("data", serialize_data data)]

(** {1 Store Data} *)

(** Store value for Solid's createStore *)
type store_value = Yojson.Safe.t

(** Create store data script *)
let store_data_tag ~store_id value =
  Printf.sprintf {|<script>window.__SOLID_STORES__=window.__SOLID_STORES__||{};window.__SOLID_STORES__["%s"]=%s;</script>|}
    store_id (serialize value)

(** {1 Signal Data} *)

(** Create signal data script *)
let signal_data_tag ~signal_id value =
  Printf.sprintf {|<script>window.__SOLID_SIGNALS__=window.__SOLID_SIGNALS__||{};window.__SOLID_SIGNALS__["%s"]=%s;</script>|}
    signal_id (serialize value)

(** {1 Hydration Markers} *)

(** Generate hydration boundary marker *)
let hydration_start ~component_id =
  Printf.sprintf {|<!--$%s-->|} component_id

(** Generate hydration end marker *)
let hydration_end ~component_id =
  Printf.sprintf {|<!--/%s-->|} component_id

(** Wrap content with hydration markers *)
let with_hydration_markers ~component_id content =
  Printf.sprintf "%s%s%s"
    (hydration_start ~component_id)
    content
    (hydration_end ~component_id)

(** {1 Suspense Data} *)

(** Suspense fallback marker *)
let suspense_fallback_marker ~suspense_id =
  Printf.sprintf {|<!--suspense:%s-->|} suspense_id

(** Suspense resolved marker *)
let suspense_resolved_marker ~suspense_id =
  Printf.sprintf {|<!--/suspense:%s-->|} suspense_id

(** {1 All Data Combination} *)

(** Combined initial data *)
type initial_data = {
  route_id: string option;
  route_data: Yojson.Safe.t option;
  stores: (string * Yojson.Safe.t) list;
  signals: (string * Yojson.Safe.t) list;
  resources: (string * Yojson.Safe.t) list;
}

let empty_initial_data = {
  route_id = None;
  route_data = None;
  stores = [];
  signals = [];
  resources = [];
}

(** Generate all data scripts *)
let all_scripts data =
  let scripts = [] in

  (* Route data *)
  let scripts = match data.route_id, data.route_data with
    | Some route_id, Some route_data ->
      route_data_tag ~route_id route_data :: scripts
    | _ -> scripts
  in

  (* Stores *)
  let scripts = List.fold_left (fun acc (store_id, value) ->
    store_data_tag ~store_id value :: acc
  ) scripts data.stores in

  (* Signals *)
  let scripts = List.fold_left (fun acc (signal_id, value) ->
    signal_data_tag ~signal_id value :: acc
  ) scripts data.signals in

  (* Resources *)
  let scripts = List.fold_left (fun acc (resource_id, value) ->
    Printf.sprintf {|<script>window.__SOLID_RESOURCES__=window.__SOLID_RESOURCES__||{};window.__SOLID_RESOURCES__["%s"]=%s;</script>|}
      resource_id (serialize value) :: acc
  ) scripts data.resources in

  String.concat "\n" (List.rev scripts)
