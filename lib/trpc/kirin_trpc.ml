(** Kirin tRPC Integration

    End-to-end type-safe APIs for Kirin.

    {1 Overview}

    tRPC provides type-safe API calls between client and server.
    Unlike REST or GraphQL, tRPC generates TypeScript types directly
    from your OCaml procedure definitions.

    {1 Quick Start}

    {[
      open Kirin_trpc

      (* Define procedures *)
      let get_user = Procedure.query
        ~path:"user.get"
        ~input:Procedure.string_input
        ~output:Procedure.json_output
        (fun _ctx id -> Ok (`Assoc [("id", `String id); ("name", `String "Alice")]))

      (* Create router *)
      let router = Trpc_router.create ()
        |> Trpc_router.query ~path:"user.get"
             ~input:Procedure.string_input ~output:Procedure.json_output
             ~input_type:Procedure.TString ~output_type:(Procedure.TObject [
               ("id", Procedure.TString);
               ("name", Procedure.TString);
             ])
             (fun _ctx id -> Ok (`Assoc [("id", `String id); ("name", `String "Alice")]))

      (* Mount routes *)
      let routes = Handler.routes ~ctx_factory:Context.unit_factory router
    ]}

    {1 Features}

    - Type-safe procedures (query, mutation, subscription)
    - Automatic input validation
    - Batch request support
    - TypeScript client generation
    - JSON Schema generation
    - Zod schema generation
*)

(** {1 Core Modules} *)

(** Procedure definitions with typed input/output *)
module Procedure = Procedure

(** Router for organizing procedures *)
module Router = Trpc_router

(** Request context *)
module Context = Context

(** Batch request handling *)
module Batch = Batch

(** Kirin route handler integration *)
module Handler = Handler

(** WebSocket subscriptions *)
module Subscription = Subscription

(** TypeScript/Schema code generation *)
module Codegen = Codegen

(** {1 Type Aliases} *)

(** Procedure type *)
type procedure_type = Procedure.procedure_type =
  | Query
  | Mutation
  | Subscription

(** Type info for codegen *)
type type_info = Procedure.type_info =
  | TString
  | TInt
  | TBool
  | TNull
  | TArray of type_info
  | TObject of (string * type_info) list
  | TOptional of type_info
  | TUnion of type_info list
  | TCustom of string

(** {1 Convenience Functions} *)

(** Create a query procedure and add to router *)
let query ~path ?meta ~input ~output ~input_type ~output_type handler router =
  Trpc_router.query ~path ?meta ~input ~output ~input_type ~output_type handler router

(** Create a mutation procedure and add to router *)
let mutation ~path ?meta ~input ~output ~input_type ~output_type handler router =
  Trpc_router.mutation ~path ?meta ~input ~output ~input_type ~output_type handler router

(** Create routes for a router *)
let routes ?config ~ctx_factory router =
  Handler.routes ?config ~ctx_factory router

(** Generate TypeScript client *)
let generate_client ~name router =
  let infos = Trpc_router.all_procedure_infos router in
  Codegen.generate_client ~router_name:name infos

(** Generate JSON Schema *)
let generate_schema router =
  let infos = Trpc_router.all_procedure_infos router in
  Codegen.generate_schema infos

(** Generate Zod schemas *)
let generate_zod router =
  let infos = Trpc_router.all_procedure_infos router in
  Codegen.generate_zod_schemas infos

(** {1 Input Validators} *)

(** No input required *)
let no_input = Procedure.no_input

(** String input *)
let string_input = Procedure.string_input

(** Integer input *)
let int_input = Procedure.int_input

(** Boolean input *)
let bool_input = Procedure.bool_input

(** JSON passthrough *)
let json_input = Procedure.json_input

(** Optional input *)
let optional_input = Procedure.optional_input

(** List input *)
let list_input = Procedure.list_input

(** Extract object field *)
let field = Procedure.field

(** Extract optional object field *)
let field_opt = Procedure.field_opt

(** {1 Output Serializers} *)

(** Unit output (null) *)
let unit_output = Procedure.unit_output

(** String output *)
let string_output = Procedure.string_output

(** Integer output *)
let int_output = Procedure.int_output

(** Boolean output *)
let bool_output = Procedure.bool_output

(** JSON passthrough *)
let json_output = Procedure.json_output

(** List output *)
let list_output = Procedure.list_output

(** Optional output *)
let option_output = Procedure.option_output
