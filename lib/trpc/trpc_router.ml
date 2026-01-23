(** tRPC Router

    Organize procedures into a router with nested namespaces. *)

open Procedure

(** {1 Router Types} *)

(** Boxed procedure for heterogeneous storage *)
type 'ctx boxed_procedure = {
  path: string;
  proc_type: procedure_type;
  meta: meta;
  execute: 'ctx -> Yojson.Safe.t -> (Yojson.Safe.t, string) result;
  info: procedure_info;
}

(** Box a typed procedure *)
let box_procedure (proc : ('ctx, 'input, 'output) t) ~input_type ~output_type : 'ctx boxed_procedure = {
  path = proc.path;
  proc_type = proc.proc_type;
  meta = proc.meta;
  execute = (fun ctx json -> execute proc ctx json);
  info = {
    name = proc.path;
    proc_type = proc.proc_type;
    input_type;
    output_type;
    description = proc.meta.description;
  };
}

(** Router containing procedures *)
type 'ctx t = {
  prefix: string;
  procedures: 'ctx boxed_procedure list;
  children: (string * 'ctx t) list;
}

(** {1 Router Builder} *)

(** Create an empty router *)
let create ?(prefix = "") () : 'ctx t = {
  prefix;
  procedures = [];
  children = [];
}

(** Get router prefix *)
let get_prefix router = router.prefix

(** Add a procedure to the router *)
let add_procedure proc router =
  { router with procedures = proc :: router.procedures }

(** Add a query *)
let query ~path ?meta ~input ~output ~input_type ~output_type handler router =
  let proc = Procedure.query ~path ?meta ~input ~output handler in
  let boxed = box_procedure proc ~input_type ~output_type in
  add_procedure boxed router

(** Add a mutation *)
let mutation ~path ?meta ~input ~output ~input_type ~output_type handler router =
  let proc = Procedure.mutation ~path ?meta ~input ~output handler in
  let boxed = box_procedure proc ~input_type ~output_type in
  add_procedure boxed router

(** Add a subscription *)
let subscription ~path ?meta ~input ~output ~input_type ~output_type handler router =
  let proc = Procedure.subscription ~path ?meta ~input ~output handler in
  let boxed = box_procedure proc ~input_type ~output_type in
  add_procedure boxed router

(** Merge child router with prefix *)
let merge ~prefix child router =
  { router with children = (prefix, child) :: router.children }

(** {1 Procedure Lookup} *)

(** Find a procedure by path *)
let rec find_procedure path router =
  (* Check direct procedures *)
  match List.find_opt (fun p -> p.path = path) router.procedures with
  | Some proc -> Some proc
  | None ->
    (* Check nested routers *)
    let check_child (prefix, child) =
      if String.length path > String.length prefix &&
         String.sub path 0 (String.length prefix) = prefix &&
         String.get path (String.length prefix) = '.' then
        let sub_path = String.sub path (String.length prefix + 1)
            (String.length path - String.length prefix - 1) in
        find_procedure sub_path child
      else
        None
    in
    List.find_map check_child router.children

(** Get all procedures (flattened) *)
let rec all_procedures ?(prefix = "") router =
  let prefixed_path p =
    if prefix = "" then p else prefix ^ "." ^ p
  in
  let direct = List.map (fun p ->
    { p with path = prefixed_path p.path }
  ) router.procedures in
  let nested = List.concat_map (fun (child_prefix, child) ->
    let full_prefix = prefixed_path child_prefix in
    all_procedures ~prefix:full_prefix child
  ) router.children in
  direct @ nested

(** {1 Procedure Info} *)

(** Get info for all procedures *)
let all_procedure_infos router =
  all_procedures router |> List.map (fun p -> p.info)

(** {1 Execution} *)

(** Execute a procedure by path *)
let execute_procedure path ctx input router =
  match find_procedure path router with
  | Some proc -> proc.execute ctx input
  | None -> Error (Printf.sprintf "Procedure not found: %s" path)

(** {1 Introspection} *)

(** Get procedure paths *)
let paths router =
  all_procedures router |> List.map (fun p -> p.path)

(** Get queries *)
let queries router =
  all_procedures router |> List.filter (fun p -> p.proc_type = Query)

(** Get mutations *)
let mutations router =
  all_procedures router |> List.filter (fun p -> p.proc_type = Mutation)

(** Get subscriptions *)
let subscriptions router =
  all_procedures router |> List.filter (fun p -> p.proc_type = Subscription)
