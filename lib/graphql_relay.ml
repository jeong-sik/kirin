(** Relay Support for Kirin GraphQL
    
    This module provides helpers for building Relay-compliant GraphQL APIs,
    including Global Object Identification, Connections (Pagination), and Mutations.
*)

module Schema = Graphql.Schema

(** {1 Global Object Identification} *)

(** Convert a type name and ID to a global ID (base64 encoded) *)
let to_global_id type_name id =
  Base64.encode_string (type_name ^ ":" ^ id)

(** Decode a global ID into (type_name, id) tuple *)
let from_global_id global_id =
  match Base64.decode global_id with
  | Ok s ->
    (try
       let idx = String.index s ':' in
       Some (String.sub s 0 idx, String.sub s (idx + 1) (String.length s - idx - 1))
     with Not_found -> None)
  | Error _ -> None

(** {1 Pagination Types} *)

type page_info = {
  has_next_page : bool;
  has_previous_page : bool;
  start_cursor : string option;
  end_cursor : string option;
}

(** PageInfo GraphQL type *)
let page_info_type () =
  Schema.obj "PageInfo"
    ~fields:[
      Schema.field "hasNextPage"
        ~typ:(Schema.non_null Schema.bool)
        ~args:Schema.Arg.[]
        ~resolve:(fun _ p -> p.has_next_page);
      Schema.field "hasPreviousPage"
        ~typ:(Schema.non_null Schema.bool)
        ~args:Schema.Arg.[]
        ~resolve:(fun _ p -> p.has_previous_page);
      Schema.field "startCursor"
        ~typ:Schema.string
        ~args:Schema.Arg.[]
        ~resolve:(fun _ p -> p.start_cursor);
      Schema.field "endCursor"
        ~typ:Schema.string
        ~args:Schema.Arg.[]
        ~resolve:(fun _ p -> p.end_cursor);
    ]

type 'node edge = {
  cursor : string;
  node : 'node;
}

type 'node connection = {
  edges : 'node edge list;
  page_info : page_info;
  total_count : int option;
}

(** Create connection and edge types for a given node type *)
let connection_definitions name node_type =
  let edge_type =
    Schema.obj (name ^ "Edge")
      ~fields:[
        Schema.field "node"
          ~typ:(Schema.non_null node_type)
          ~args:Schema.Arg.[]
          ~resolve:(fun _ e -> e.node);
        Schema.field "cursor"
          ~typ:(Schema.non_null Schema.string)
          ~args:Schema.Arg.[]
          ~resolve:(fun _ e -> e.cursor);
      ]
  in
  let connection_type =
    Schema.obj (name ^ "Connection")
      ~fields:[
        Schema.field "edges"
          ~typ:(Schema.list (Schema.non_null edge_type))
          ~args:Schema.Arg.[]
          ~resolve:(fun _ c -> Some c.edges);
        Schema.field "pageInfo"
          ~typ:(Schema.non_null (page_info_type ()))
          ~args:Schema.Arg.[]
          ~resolve:(fun _ c -> c.page_info);
        Schema.field "totalCount"
          ~typ:Schema.int
          ~args:Schema.Arg.[]
          ~resolve:(fun _ c -> c.total_count);
      ]
  in
  (connection_type, edge_type)

(** {1 Pagination Helpers} *)

(** Standard Relay connection arguments *)
let args () = Schema.Arg.[
  arg "first" ~typ:int;
  arg "after" ~typ:string;
  arg "last" ~typ:int;
  arg "before" ~typ:string;
]

type connection_args = {
  first : int option;
  after : string option;
  last : int option;
  before : string option;
}

(** Extract connection arguments from resolve function args *)
let get_args first after last before =
  { first; after; last; before }

(** Default cap on [first] / [last] page size.

    Relay best practice is to bound the requested page size at the
    server. A resolver that forwards the client's [first]/[last]
    unchanged exposes an in-memory traversal whose cost the attacker
    chooses — [first: 1_000_000] against [connection_from_list] is
    a million-element [List.filteri] plus a million [encode_cursor]
    base64 allocations. 1000 is permissive enough that legitimate UI
    use is unaffected (typical paginated lists request <= 50) while
    keeping the worst case bounded. *)
let default_max_page_size = 1000

(** Create a simple connection from a list (in-memory pagination).

    Implements the Relay Connection Specification pagination algorithm:
    {ol
      {- If [after] is set, remove edges before and including that cursor.}
      {- If [before] is set, remove edges after and including that cursor.}
      {- If [first] is set, keep only the first [first] edges from the slice.}
      {- If [last] is set, keep only the last [last] edges from the slice.}
    }

    @param max_page_size caps [first] and [last] independently.
           Defaults to [default_max_page_size]. A request larger than
           the cap is silently clamped — Relay returns a smaller page
           than requested, which the spec allows.
    @raise Invalid_argument when [after] / [before] decodes to
           something that is not a valid server-issued cursor. The
           previous code fell back to index 0 on parse failure, which
           silently turned a malformed cursor into "page from the
           start" or "empty page" depending on which field carried it
           — a Relay spec violation (cursors are opaque and
           server-issued only) and a workaround-class silent
           permissive default. Loud failure lets the GraphQL layer
           surface the bad argument rather than paper over it.

    For large datasets, use database-level pagination instead.
*)
let connection_from_list ?(max_page_size = default_max_page_size)
    ?(total_count) list args =
  let count = List.length list in
  let total = Option.value total_count ~default:count in

  let decode_cursor c =
    (* Each step is allowed to fail; both produce a structural reject
       rather than a silent fallback to 0. The decoded index must
       also be in range — a negative or far-beyond-count value cannot
       be a valid server-issued cursor for [list]. *)
    match Base64.decode c with
    | Error _ ->
      invalid_arg
        "Relay.connection_from_list: invalid cursor (not base64)"
    | Ok decoded ->
      (match int_of_string_opt decoded with
       | None ->
         invalid_arg
           "Relay.connection_from_list: invalid cursor (not an integer index)"
       | Some i when i < 0 || i > count ->
         invalid_arg
           "Relay.connection_from_list: cursor out of range"
       | Some i -> i)
  in
  let encode_cursor i =
    Base64.encode_string (string_of_int i)
  in

  (* Clamp first/last to max_page_size before they reach the slice
     math. Done up front so the rest of the function operates on
     bounded values regardless of what the client requested. *)
  let clamp_opt = function
    | Some n when n > max_page_size -> Some max_page_size
    | other -> other
  in
  let first = clamp_opt args.first in
  let last = clamp_opt args.last in

  (* Step 1-2: apply after/before to narrow the index range *)
  let after_idx = match args.after with
    | Some cursor -> decode_cursor cursor + 1
    | None -> 0
  in
  let before_idx = match args.before with
    | Some cursor -> decode_cursor cursor  (* exclusive: up to but not including *)
    | None -> count
  in
  let range_start = after_idx in
  let range_end = min before_idx count in  (* clamp to list length *)

  (* Step 3: apply first to the narrowed range *)
  let range_start, range_end =
    match first with
    | Some f when range_start + f < range_end -> (range_start, range_start + f)
    | _ -> (range_start, range_end)
  in
  (* Step 4: apply last to the narrowed range *)
  let range_start, range_end =
    match last with
    | Some l when range_end - l > range_start -> (range_end - l, range_end)
    | _ -> (range_start, range_end)
  in

  let sliced =
    list
    |> List.filteri (fun i _ -> i >= range_start && i < range_end)
  in

  let edges = List.mapi (fun i node ->
    { cursor = encode_cursor (range_start + i); node }
  ) sliced in

  (* Relay spec: hasPreviousPage is true when last is set and more edges
     exist before the returned slice within the after..before range.
     hasNextPage is true when first is set and more edges exist after
     the returned slice within the after..before range. *)
  let has_previous_page =
    match args.last with
    | Some _ -> range_start > after_idx
    | None -> range_start > 0
  in
  let has_next_page =
    match args.first with
    | Some _ -> range_end < (min before_idx count)
    | None -> range_end < count
  in

  let page_info = {
    has_next_page;
    has_previous_page;
    start_cursor = (match edges with [] -> None | e :: _ -> Some e.cursor);
    end_cursor = (match List.rev edges with [] -> None | e :: _ -> Some e.cursor);
  } in

  { edges; page_info; total_count = Some total }

(** {1 Node Interface} *)

(** Create the Node interface and node field *)
let node_definitions () =
  let node_interface = Schema.interface "Node"
    ~fields:(fun _ -> [
      Schema.abstract_field "id"
        ~typ:(Schema.non_null Schema.guid)
        ~args:Schema.Arg.[]
    ])
  in
  
  let node_field resolve_fn = 
    Schema.field "node"
      ~typ:node_interface
      ~args:Schema.Arg.[
        arg "id" ~typ:(Schema.Arg.non_null Schema.Arg.guid)
      ]
      ~resolve:(fun ctx () id -> resolve_fn ctx id)
  in
  
  (node_interface, node_field)


(** {1 Mutation Helpers} *)

(*
(** Create a Relay-compliant mutation input type *)
let mutation_input name fields =
  Schema.Arg.obj name
    ~fields:(
      Schema.Arg.arg "clientMutationId" ~typ:Schema.Arg.string :: fields
    )
    ~coerce:(fun client_mutation_id rest -> (client_mutation_id, rest))

(** Create a Relay-compliant mutation payload type *)
let mutation_payload name fields =
  Schema.obj name
    ~fields:(fun _ ->
      Schema.field "clientMutationId"
        ~typ:Schema.string
        ~args:Schema.Arg.[]
        ~resolve:(fun _ (id, _) -> id) :: fields
    )
*)
