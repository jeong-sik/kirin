(** Initial Data Serialization for Hydration

    XSS-safe JSON serialization for embedding data in HTML.
    Prevents script injection via </script> sequences.
*)

(** Characters that need escaping in JSON embedded in HTML *)
let escape_for_html_embed s =
  let buf = Buffer.create (String.length s * 2) in
  String.iter (function
    | '<' -> Buffer.add_string buf "\\u003c"
    | '>' -> Buffer.add_string buf "\\u003e"
    | '&' -> Buffer.add_string buf "\\u0026"
    | '\'' -> Buffer.add_string buf "\\u0027"
    | c -> Buffer.add_char buf c
  ) s;
  Buffer.contents buf

(** Serialize JSON to XSS-safe string for HTML embedding *)
let serialize json =
  let json_str = Yojson.Safe.to_string json in
  escape_for_html_embed json_str

(** Generate script tag with initial data *)
let script_tag ?(var_name = "__INITIAL_DATA__") json =
  let safe_json = serialize json in
  Printf.sprintf {|<script>window.%s=%s</script>|} var_name safe_json

(** Generate script tag with custom variable name *)
let named_script_tag ~name json =
  script_tag ~var_name:name json

(** Generate multiple data scripts *)
let multi_script_tags data_list =
  data_list
  |> List.map (fun (name, json) -> script_tag ~var_name:name json)
  |> String.concat "\n"

(** Data payload configuration *)
type payload = {
  initial_data: Yojson.Safe.t option;
  dehydrated_state: Yojson.Safe.t option;  (* TanStack Query style *)
  route_data: Yojson.Safe.t option;
  user_data: Yojson.Safe.t option;
  config_data: Yojson.Safe.t option;
}

let empty_payload = {
  initial_data = None;
  dehydrated_state = None;
  route_data = None;
  user_data = None;
  config_data = None;
}

(** Render all data scripts from payload *)
let render_payload payload =
  let scripts = ref [] in
  Option.iter (fun d ->
    scripts := script_tag ~var_name:"__INITIAL_DATA__" d :: !scripts
  ) payload.initial_data;
  Option.iter (fun d ->
    scripts := script_tag ~var_name:"__DEHYDRATED_STATE__" d :: !scripts
  ) payload.dehydrated_state;
  Option.iter (fun d ->
    scripts := script_tag ~var_name:"__ROUTE_DATA__" d :: !scripts
  ) payload.route_data;
  Option.iter (fun d ->
    scripts := script_tag ~var_name:"__USER_DATA__" d :: !scripts
  ) payload.user_data;
  Option.iter (fun d ->
    scripts := script_tag ~var_name:"__CONFIG__" d :: !scripts
  ) payload.config_data;
  List.rev !scripts |> String.concat "\n"

(** Create payload from simple key-value pairs *)
let payload_from_assoc assoc =
  { empty_payload with initial_data = Some (`Assoc assoc) }

(** Merge two payloads *)
let merge_payloads p1 p2 =
  let merge_opt a b = match b with Some _ -> b | None -> a in
  {
    initial_data = merge_opt p1.initial_data p2.initial_data;
    dehydrated_state = merge_opt p1.dehydrated_state p2.dehydrated_state;
    route_data = merge_opt p1.route_data p2.route_data;
    user_data = merge_opt p1.user_data p2.user_data;
    config_data = merge_opt p1.config_data p2.config_data;
  }

(** TanStack Query style dehydration helpers *)
module QueryClient = struct
  type query = {
    query_key: string list;
    state: Yojson.Safe.t;
  }

  type dehydrated = {
    queries: query list;
    mutations: Yojson.Safe.t list;
  }

  let empty_dehydrated = { queries = []; mutations = [] }

  let add_query ~key ~data dehydrated =
    let query = { query_key = key; state = data } in
    { dehydrated with queries = query :: dehydrated.queries }

  let to_json dehydrated =
    `Assoc [
      ("queries", `List (List.map (fun q ->
        `Assoc [
          ("queryKey", `List (List.map (fun k -> `String k) q.query_key));
          ("state", `Assoc [("data", q.state)]);
        ]
      ) dehydrated.queries));
      ("mutations", `List dehydrated.mutations);
    ]

  let to_script_tag dehydrated =
    let json = to_json dehydrated in
    script_tag ~var_name:"__REACT_QUERY_STATE__" json
end

(** Nonce support for CSP *)
let script_tag_with_nonce ~nonce ?(var_name = "__INITIAL_DATA__") json =
  let safe_json = serialize json in
  Printf.sprintf {|<script nonce="%s">window.%s=%s</script>|}
    (Meta.escape_html nonce) var_name safe_json

(** Generate random nonce *)
let generate_nonce () =
  let bytes = Bytes.create 16 in
  for i = 0 to 15 do
    Bytes.set bytes i (Char.chr (Random.int 256))
  done;
  Base64.encode_string (Bytes.to_string bytes)

(** Module-level state for nonce *)
let current_nonce : string option ref = ref None

let set_nonce nonce = current_nonce := Some nonce
let get_nonce () : string option = !current_nonce
let clear_nonce () = current_nonce := None

(** Validate JSON is safe for embedding (no raw </script>) *)
let is_safe_for_embed json =
  let s = Yojson.Safe.to_string json in
  not (String.lowercase_ascii s |> fun lower ->
    try
      let _ = Str.search_forward (Str.regexp_string "</script>") lower 0 in
      true
    with Not_found -> false)
