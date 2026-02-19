(** Kirin MCP - Tasks Primitive (2025-11-25)

    Async/deferred tool execution via task state machine.
    Tasks enable long-running operations that report progress
    and deliver results asynchronously.
*)

(** {1 Types} *)

(** Task state machine:
    {v
      Working ──┬── Completed
                ├── Failed
                ├── Cancelled
                └── Input_required ── Working (resume)
    v}
*)
type task_state =
  | Working
  | Input_required
  | Completed
  | Failed
  | Cancelled

(** Task record *)
type task = {
  id : string;
  mutable state : task_state;
  tool_name : string;
  mutable progress : float option;
  mutable progress_message : string option;
  mutable result : Protocol.tool_result option;
  mutable error : string option;
  created_at : float;    (** Unix timestamp when task was created *)
  mutable updated_at : float;  (** Unix timestamp of last state change *)
}

(** Task registry *)
type registry = {
  tasks : (string, task) Hashtbl.t;
  mutable next_id : int;
  mutable on_state_change : (task -> unit) option;
}

(** {1 Constructor} *)

(** Create a new task registry.
    [?on_state_change] is called after every task state transition. *)
let create_registry ?on_state_change () =
  {
    tasks = Hashtbl.create 16;
    next_id = 1;
    on_state_change;
  }

(** Set or replace the on_state_change callback *)
let set_on_state_change registry f =
  registry.on_state_change <- Some f

(** Internal: fire the state change callback if set *)
let notify registry task =
  match registry.on_state_change with
  | Some f -> f task
  | None -> ()

(** {1 Task Operations} *)

(** Create a new task for a tool execution *)
let create_task registry ~tool_name =
  let id = Printf.sprintf "task-%d" registry.next_id in
  registry.next_id <- registry.next_id + 1;
  let now = Unix.gettimeofday () in
  let task = {
    id;
    state = Working;
    tool_name;
    progress = None;
    progress_message = None;
    result = None;
    error = None;
    created_at = now;
    updated_at = now;
  } in
  Hashtbl.replace registry.tasks id task;
  task

(** Update task progress (0.0 to 1.0) *)
let update_progress registry ~id ~progress ?message () =
  match Hashtbl.find_opt registry.tasks id with
  | Some task when task.state = Working ->
    task.progress <- Some progress;
    task.progress_message <- message;
    task.updated_at <- Unix.gettimeofday ();
    notify registry task;
    Ok ()
  | Some _ -> Error "Task is not in working state"
  | None -> Error (Printf.sprintf "Task not found: %s" id)

(** Complete a task with a result *)
let complete_task registry ~id ~result =
  match Hashtbl.find_opt registry.tasks id with
  | Some task when task.state = Working ->
    task.state <- Completed;
    task.result <- Some result;
    task.progress <- Some 1.0;
    task.updated_at <- Unix.gettimeofday ();
    notify registry task;
    Ok ()
  | Some _ -> Error "Task is not in working state"
  | None -> Error (Printf.sprintf "Task not found: %s" id)

(** Fail a task with an error message *)
let fail_task registry ~id ~error =
  match Hashtbl.find_opt registry.tasks id with
  | Some task when task.state = Working ->
    task.state <- Failed;
    task.error <- Some error;
    task.updated_at <- Unix.gettimeofday ();
    notify registry task;
    Ok ()
  | Some _ -> Error "Task is not in working state"
  | None -> Error (Printf.sprintf "Task not found: %s" id)

(** Cancel a task *)
let cancel_task registry ~id =
  match Hashtbl.find_opt registry.tasks id with
  | Some task when task.state = Working || task.state = Input_required ->
    task.state <- Cancelled;
    task.updated_at <- Unix.gettimeofday ();
    notify registry task;
    Ok ()
  | Some _ -> Error "Task cannot be cancelled in current state"
  | None -> Error (Printf.sprintf "Task not found: %s" id)

(** Request input from client *)
let request_input registry ~id =
  match Hashtbl.find_opt registry.tasks id with
  | Some task when task.state = Working ->
    task.state <- Input_required;
    task.updated_at <- Unix.gettimeofday ();
    notify registry task;
    Ok ()
  | Some _ -> Error "Task is not in working state"
  | None -> Error (Printf.sprintf "Task not found: %s" id)

(** Resume a task after input received *)
let resume_task registry ~id =
  match Hashtbl.find_opt registry.tasks id with
  | Some task when task.state = Input_required ->
    task.state <- Working;
    task.updated_at <- Unix.gettimeofday ();
    notify registry task;
    Ok ()
  | Some _ -> Error "Task is not in input_required state"
  | None -> Error (Printf.sprintf "Task not found: %s" id)

(** Get a task by ID *)
let get_task registry ~id =
  Hashtbl.find_opt registry.tasks id

(** List all tasks *)
let list_tasks registry =
  Hashtbl.fold (fun _id task acc -> task :: acc) registry.tasks []

(** {1 JSON Encoding} *)

let task_state_to_string = function
  | Working -> "working"
  | Input_required -> "inputRequired"
  | Completed -> "completed"
  | Failed -> "failed"
  | Cancelled -> "cancelled"

let task_state_of_string = function
  | "working" -> Ok Working
  | "inputRequired" | "input_required" -> Ok Input_required
  | "completed" -> Ok Completed
  | "failed" -> Ok Failed
  | "cancelled" -> Ok Cancelled
  | s -> Error (Printf.sprintf "Unknown task state: %s" s)

let task_to_json task =
  let base = [
    "taskId", `String task.id;
    "state", `String (task_state_to_string task.state);
    "toolName", `String task.tool_name;
  ] in
  let with_progress = match task.progress with
    | Some p -> ("progress", `Float p) :: base
    | None -> base
  in
  let with_message = match task.progress_message with
    | Some m -> ("progressMessage", `String m) :: with_progress
    | None -> with_progress
  in
  let with_result = match task.result with
    | Some r -> ("result", Protocol.tool_result_to_json r) :: with_message
    | None -> with_message
  in
  let with_error = match task.error with
    | Some e -> ("error", `String e) :: with_result
    | None -> with_result
  in
  let with_timestamps =
    ("createdAt", `Float task.created_at)
    :: ("updatedAt", `Float task.updated_at)
    :: with_error
  in
  `Assoc with_timestamps

let task_of_json json =
  let open Yojson.Safe.Util in
  let id = json |> member "taskId" |> to_string in
  let state_str = json |> member "state" |> to_string in
  let tool_name = json |> member "toolName" |> to_string in
  match task_state_of_string state_str with
  | Ok state ->
    Ok {
      id;
      state;
      tool_name;
      progress = (match json |> member "progress" with
        | `Float f -> Some f
        | `Int i -> Some (Float.of_int i)
        | _ -> None);
      progress_message = (match json |> member "progressMessage" with
        | `String s -> Some s
        | _ -> None);
      result = None;  (* Result is parsed separately via tasks/result *)
      error = (match json |> member "error" with
        | `String s -> Some s
        | _ -> None);
      created_at = (match json |> member "createdAt" with
        | `Float f -> f
        | `Int i -> Float.of_int i
        | _ -> 0.0);
      updated_at = (match json |> member "updatedAt" with
        | `Float f -> f
        | `Int i -> Float.of_int i
        | _ -> 0.0);
    }
  | Error msg -> Error msg
