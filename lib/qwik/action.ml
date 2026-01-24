(** Qwik Actions

    routeAction$ and globalAction$ implementation. *)

(** {1 Action Types} *)

(** Action result *)
type 'a result =
  | ActionOk of 'a
  | ActionFail of { status: int; errors: (string * string) list }
  | ActionRedirect of string

(** File info *)
type file_info = {
  name: string;
  content_type: string;
  size: int;
}

(** Field value *)
type field_value =
  | Text of string
  | Multiple of string list
  | File of file_info

(** Form data *)
type form_data = {
  fields: (string * field_value) list;
}

(** Action context *)
type context = {
  url: string;
  params: (string * string) list;
  headers: (string * string) list;
  cookie: (string * string) list;
  form_data: form_data;
}

(** Action scope *)
type scope =
  | RouteAction      (* Only accessible on route *)
  | GlobalAction     (* Accessible anywhere *)

(** Action definition *)
type 'a t = {
  name: string;
  scope: scope;
  action: context -> 'a result;
}

(** {1 Action Construction} *)

(** Create route action *)
let route_action ~name action = {
  name;
  scope = RouteAction;
  action;
}

(** Create global action *)
let global_action ~name action = {
  name;
  scope = GlobalAction;
  action;
}

(** {1 Action Execution} *)

(** Execute action *)
let execute action ctx =
  action.action ctx

(** {1 Form Data Helpers} *)

(** Get text field *)
let get_field form name =
  match List.assoc_opt name form.fields with
  | Some (Text v) -> Some v
  | Some (Multiple (v :: _)) -> Some v
  | _ -> None

(** Get multiple field *)
let get_multiple form name =
  match List.assoc_opt name form.fields with
  | Some (Multiple vs) -> vs
  | Some (Text v) -> [v]
  | _ -> []

(** Get file field *)
let get_file form name =
  match List.assoc_opt name form.fields with
  | Some (File f) -> Some f
  | _ -> None

(** Check if field exists *)
let has_field form name =
  List.mem_assoc name form.fields

(** Empty form data *)
let empty_form = { fields = [] }

(** {1 Validation} *)

(** Validation result *)
type validation_result =
  | Valid
  | Invalid of (string * string) list

(** Validate required field *)
let required name form =
  match get_field form name with
  | None | Some "" -> Invalid [(name, "required")]
  | Some _ -> Valid

(** Validate email field *)
let email name form =
  match get_field form name with
  | None -> Invalid [(name, "required")]
  | Some v ->
    if String.contains v '@' then Valid
    else Invalid [(name, "invalid email")]

(** Validate min length *)
let min_length min name form =
  match get_field form name with
  | None -> Invalid [(name, "required")]
  | Some v ->
    if String.length v >= min then Valid
    else Invalid [(name, Printf.sprintf "minimum %d characters" min)]

(** Combine validations *)
let validate_all validations form =
  let errors = List.filter_map (fun v ->
    match v form with
    | Valid -> None
    | Invalid errs -> Some errs
  ) validations in
  match List.concat errors with
  | [] -> Valid
  | errs -> Invalid errs

(** {1 Context Helpers} *)

(** Create context from request *)
let context_of_request ~url ~params ~headers ~cookies ~form () = {
  url;
  params;
  headers;
  cookie = cookies;
  form_data = form;
}

(** Get header *)
let get_header ctx name =
  List.assoc_opt (String.lowercase_ascii name) ctx.headers

(** Get cookie *)
let get_cookie ctx name =
  List.assoc_opt name ctx.cookie

(** {1 Result Helpers} *)

(** Create success result *)
let ok value = ActionOk value

(** Create fail result *)
let fail ?(status=400) errors = ActionFail { status; errors }

(** Create redirect result *)
let redirect url = ActionRedirect url

(** {1 Serialization} *)

(** Field value to JSON *)
let field_value_to_json = function
  | Text v -> `String v
  | Multiple vs -> `List (List.map (fun v -> `String v) vs)
  | File { name; content_type; size } -> `Assoc [
      ("name", `String name);
      ("contentType", `String content_type);
      ("size", `Int size);
    ]

(** Form data to JSON *)
let form_data_to_json form =
  `Assoc (List.map (fun (k, v) -> (k, field_value_to_json v)) form.fields)

(** Result to JSON *)
let result_to_json value_to_json = function
  | ActionOk v -> `Assoc [
      ("type", `String "ok");
      ("data", value_to_json v);
    ]
  | ActionFail { status; errors } -> `Assoc [
      ("type", `String "fail");
      ("status", `Int status);
      ("errors", `Assoc (List.map (fun (k, v) -> (k, `String v)) errors));
    ]
  | ActionRedirect url -> `Assoc [
      ("type", `String "redirect");
      ("url", `String url);
    ]

(** {1 QRL Integration} *)

(** Create action QRL *)
let to_qrl ~chunk ~symbol action =
  Qrl.create ~chunk ~symbol ~dev_name:action.name ()

(** Action with QRL *)
type 'a action_qrl = {
  action: 'a t;
  qrl: Qrl.t;
}

(** Create action with QRL *)
let with_qrl ~chunk ~symbol action = {
  action;
  qrl = to_qrl ~chunk ~symbol action;
}
