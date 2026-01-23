(** tRPC Procedure Types

    Defines query, mutation, and subscription procedures. *)

(** {1 Type Definitions} *)

(** Procedure type *)
type procedure_type =
  | Query
  | Mutation
  | Subscription

let procedure_type_to_string = function
  | Query -> "query"
  | Mutation -> "mutation"
  | Subscription -> "subscription"

(** Input validator *)
type 'a validator = Yojson.Safe.t -> ('a, string) result

(** No input validator *)
let no_input : unit validator = fun _ -> Ok ()

(** JSON input (pass through) *)
let json_input : Yojson.Safe.t validator = fun j -> Ok j

(** String input *)
let string_input : string validator = function
  | `String s -> Ok s
  | _ -> Error "expected string"

(** Int input *)
let int_input : int validator = function
  | `Int i -> Ok i
  | _ -> Error "expected int"

(** Bool input *)
let bool_input : bool validator = function
  | `Bool b -> Ok b
  | _ -> Error "expected bool"

(** Optional input *)
let optional_input (v : 'a validator) : 'a option validator = function
  | `Null -> Ok None
  | json -> v json |> Result.map (fun x -> Some x)

(** List input *)
let list_input (v : 'a validator) : 'a list validator = function
  | `List items ->
    let rec validate acc = function
      | [] -> Ok (List.rev acc)
      | x :: xs ->
        match v x with
        | Ok item -> validate (item :: acc) xs
        | Error e -> Error e
    in
    validate [] items
  | _ -> Error "expected list"

(** Object field extractor *)
let field name (v : 'a validator) : 'a validator = function
  | `Assoc fields ->
    (match List.assoc_opt name fields with
     | Some value -> v value
     | None -> Error (Printf.sprintf "missing field: %s" name))
  | _ -> Error "expected object"

(** Optional field extractor *)
let field_opt name (v : 'a validator) : 'a option validator = function
  | `Assoc fields ->
    (match List.assoc_opt name fields with
     | Some `Null -> Ok None
     | Some value -> v value |> Result.map (fun x -> Some x)
     | None -> Ok None)
  | _ -> Error "expected object"

(** {1 Output Serializers} *)

type 'a serializer = 'a -> Yojson.Safe.t

(** Unit serializer *)
let unit_output : unit serializer = fun () -> `Null

(** JSON passthrough *)
let json_output : Yojson.Safe.t serializer = fun j -> j

(** String serializer *)
let string_output : string serializer = fun s -> `String s

(** Int serializer *)
let int_output : int serializer = fun i -> `Int i

(** Bool serializer *)
let bool_output : bool serializer = fun b -> `Bool b

(** List serializer *)
let list_output (s : 'a serializer) : 'a list serializer = fun items ->
  `List (List.map s items)

(** Option serializer *)
let option_output (s : 'a serializer) : 'a option serializer = function
  | Some x -> s x
  | None -> `Null

(** {1 Procedure Definition} *)

(** Procedure metadata *)
type meta = {
  description: string option;
  deprecated: bool;
  tags: string list;
}

let default_meta = {
  description = None;
  deprecated = false;
  tags = [];
}

(** A procedure with typed input and output *)
type ('ctx, 'input, 'output) t = {
  proc_type: procedure_type;
  path: string;
  meta: meta;
  validate: 'input validator;
  serialize: 'output serializer;
  handler: 'ctx -> 'input -> ('output, string) result;
}

(** {1 Procedure Builders} *)

(** Create a query procedure *)
let query ~path ?(meta = default_meta) ~input ~output handler =
  { proc_type = Query; path; meta; validate = input; serialize = output; handler }

(** Create a mutation procedure *)
let mutation ~path ?(meta = default_meta) ~input ~output handler =
  { proc_type = Mutation; path; meta; validate = input; serialize = output; handler }

(** Create a subscription procedure (handler returns stream setup) *)
let subscription ~path ?(meta = default_meta) ~input ~output handler =
  { proc_type = Subscription; path; meta; validate = input; serialize = output; handler }

(** {1 Procedure Execution} *)

(** Execute a procedure with JSON input *)
let execute (proc : ('ctx, 'input, 'output) t) ctx json_input =
  match proc.validate json_input with
  | Error e -> Error (Printf.sprintf "Input validation error: %s" e)
  | Ok input ->
    match proc.handler ctx input with
    | Error e -> Error e
    | Ok output -> Ok (proc.serialize output)

(** {1 Type Info for Codegen} *)

type type_info =
  | TString
  | TInt
  | TBool
  | TNull
  | TArray of type_info
  | TObject of (string * type_info) list
  | TOptional of type_info
  | TUnion of type_info list
  | TCustom of string

let rec type_info_to_typescript = function
  | TString -> "string"
  | TInt -> "number"
  | TBool -> "boolean"
  | TNull -> "null"
  | TArray t -> Printf.sprintf "%s[]" (type_info_to_typescript t)
  | TObject fields ->
    let field_strs = List.map (fun (name, t) ->
      Printf.sprintf "%s: %s" name (type_info_to_typescript t)
    ) fields in
    Printf.sprintf "{ %s }" (String.concat "; " field_strs)
  | TOptional t -> Printf.sprintf "%s | undefined" (type_info_to_typescript t)
  | TUnion types ->
    String.concat " | " (List.map type_info_to_typescript types)
  | TCustom name -> name

(** Procedure type info for codegen *)
type procedure_info = {
  name: string;
  proc_type: procedure_type;
  input_type: type_info;
  output_type: type_info;
  description: string option;
}
