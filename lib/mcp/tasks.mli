type task_state = Working | Input_required | Completed | Failed | Cancelled
type task = {
  id : string;
  mutable state : task_state;
  tool_name : string;
  mutable progress : float option;
  mutable progress_message : string option;
  mutable result : Protocol.tool_result option;
  mutable error : string option;
  created_at : float;
  mutable updated_at : float;
}
type registry = {
  tasks : (string, task) Hashtbl.t;
  mutable next_id : int;
  mutable on_state_change : (task -> unit) option;
}
val create_registry : ?on_state_change:(task -> unit) -> unit -> registry
val set_on_state_change : registry -> (task -> unit) -> unit
val notify : registry -> task -> unit
val create_task : registry -> tool_name:string -> task
val update_progress :
  registry ->
  id:string ->
  progress:float -> ?message:string -> unit -> (unit, string) result
val complete_task :
  registry ->
  id:string ->
  result:Protocol.tool_result -> (unit, string) result
val fail_task :
  registry -> id:string -> error:string -> (unit, string) result
val cancel_task : registry -> id:string -> (unit, string) result
val request_input : registry -> id:string -> (unit, string) result
val resume_task : registry -> id:string -> (unit, string) result
val get_task : registry -> id:string -> task option
val list_tasks : registry -> task list
val task_state_to_string : task_state -> string
val task_state_of_string : string -> (task_state, string) result
val task_to_json :
  task ->
  [> `Assoc of
       (string *
        [> `Assoc of (string * Yojson.Safe.t) list
         | `Float of float
         | `String of string ])
       list ]
val task_of_json : Yojson__Safe.t -> (task, string) result
