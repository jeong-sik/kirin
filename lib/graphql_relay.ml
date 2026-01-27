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

(** Create a simple connection from a list (in-memory pagination) 
    Note: For large datasets, use database-level pagination instead.
*)
let connection_from_list ?(total_count) list args =
  let count = List.length list in
  let total = Option.value total_count ~default:count in
  
  let decode_cursor c = 
    try Base64.decode_exn c |> int_of_string with _ -> 0
  in
  let encode_cursor i = 
    Base64.encode_string (string_of_int i)
  in

  let start_idx = match args.after with
    | Some cursor -> decode_cursor cursor + 1
    | None -> 0
  in
  
  let count_to_take = match args.first with
    | Some f -> f
    | None -> count
  in
  
  let sliced = 
    list 
    |> List.filteri (fun i _ -> i >= start_idx && i < start_idx + count_to_take)
  in
  
  let edges = List.mapi (fun i node ->
    { cursor = encode_cursor (start_idx + i); node }
  ) sliced in
  
  let page_info = {
    has_next_page = (start_idx + count_to_take) < count;
    has_previous_page = start_idx > 0;
    start_cursor = if edges = [] then None else Some (List.hd edges).cursor;
    end_cursor = if edges = [] then None else Some (List.rev edges |> List.hd).cursor;
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
