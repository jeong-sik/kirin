(** Svelte Form Actions

    SvelteKit-style form handling with named actions. *)

(** {1 Action Context} *)

(** Form action context *)
type action_context = {
  url: string;
  request_headers: (string * string) list;
  cookies: (string * string) list;
  locals: Yojson.Safe.t;
  route_id: string;
}

(** {1 Form Data} *)

(** File upload data *)
type file_data = {
  filename: string;
  content_type: string;
  data: string;
}

(** Form field value *)
type field_value =
  | Text of string
  | File of file_data
  | Multiple of field_value list

(** Form data *)
type form_data = (string * field_value) list

(** Get text field *)
let get_text form name =
  match List.assoc_opt name form with
  | Some (Text s) -> Some s
  | Some (Multiple (Text s :: _)) -> Some s
  | _ -> None

(** Get required text field *)
let get_text_exn form name =
  match get_text form name with
  | Some v -> v
  | None -> failwith (Printf.sprintf "Missing required field: %s" name)

(** Get all values for field *)
let get_all form name =
  match List.assoc_opt name form with
  | Some (Multiple vs) ->
    List.filter_map (function Text s -> Some s | _ -> None) vs
  | Some (Text s) -> [s]
  | _ -> []

(** Get file field *)
let get_file form name =
  match List.assoc_opt name form with
  | Some (File f) -> Some f
  | Some (Multiple (File f :: _)) -> Some f
  | _ -> None

(** {1 Action Results} *)

(** Action output *)
type action_output =
  | ActionSuccess of Yojson.Safe.t
  | ActionFail of int * Yojson.Safe.t  (* status, data with errors *)
  | ActionRedirect of int * string
  | ActionError of int * string

(** {1 Result Helpers} *)

(** Success response *)
let success ?(data=`Assoc []) () = ActionSuccess data

(** Fail with validation errors *)
let fail ?(status=400) data = ActionFail (status, data)

(** Redirect after action *)
let redirect ?(status=303) location = ActionRedirect (status, location)

(** Server error *)
let error status message = ActionError (status, message)

(** {1 Validation Helpers} *)

(** Validation error *)
type validation_error = {
  field: string;
  message: string;
}

(** Validation result *)
type 'a validation_result =
  | Valid of 'a
  | Invalid of validation_error list

(** Create validation error *)
let validation_error field message = { field; message }

(** Validate required field *)
let required form name =
  match get_text form name with
  | Some v when String.trim v <> "" -> Valid v
  | _ -> Invalid [validation_error name "This field is required"]

(** Validate email format *)
let email form name =
  match get_text form name with
  | None -> Invalid [validation_error name "Email is required"]
  | Some v ->
    let re = Str.regexp "^[^@]+@[^@]+\\.[^@]+$" in
    if Str.string_match re v 0 then Valid v
    else Invalid [validation_error name "Invalid email format"]

(** Validate min length *)
let min_length len form name =
  match get_text form name with
  | None -> Invalid [validation_error name "This field is required"]
  | Some v ->
    if String.length v >= len then Valid v
    else Invalid [validation_error name (Printf.sprintf "Must be at least %d characters" len)]

(** Combine validations *)
let combine_validations validations =
  let errors = List.filter_map (function
    | Invalid es -> Some es
    | Valid _ -> None
  ) validations in
  if errors = [] then
    Valid (List.filter_map (function Valid v -> Some v | _ -> None) validations)
  else
    Invalid (List.concat errors)

(** Validation errors to JSON *)
let errors_to_json errors =
  `Assoc (List.map (fun e -> (e.field, `String e.message)) errors)

(** Fail with validation errors *)
let fail_validation errors =
  ActionFail (400, `Assoc [
    ("errors", errors_to_json errors);
  ])

(** {1 Named Actions} *)

(** Action handler type *)
type action_handler = action_context -> form_data -> action_output

(** Named action definition *)
type named_action = {
  name: string option;  (* None = default action *)
  handler: action_handler;
}

(** Create default action *)
let default handler = {
  name = None;
  handler;
}

(** Create named action *)
let named name handler = {
  name = Some name;
  handler;
}

(** Find action by name *)
let find_action actions name =
  List.find_opt (fun a ->
    match a.name, name with
    | None, None -> true
    | Some n1, Some n2 -> n1 = n2
    | _ -> false
  ) actions

(** {1 Context Builders} *)

(** Create action context *)
let create_context ~url ?(headers=[]) ?(cookies=[]) ?(locals=`Assoc []) ~route_id () = {
  url;
  request_headers = headers;
  cookies;
  locals;
  route_id;
}

(** Get cookie from context *)
let cookie ctx name =
  List.assoc_opt name ctx.cookies

(** Get header from context *)
let header ctx name =
  let name_lower = String.lowercase_ascii name in
  List.find_map (fun (k, v) ->
    if String.lowercase_ascii k = name_lower then Some v else None
  ) ctx.request_headers

(** {1 Serialization} *)

(** Action output to JSON *)
let output_to_json = function
  | ActionSuccess data ->
    `Assoc [("type", `String "success"); ("data", data)]
  | ActionFail (status, data) ->
    `Assoc [
      ("type", `String "fail");
      ("status", `Int status);
      ("data", data);
    ]
  | ActionRedirect (status, location) ->
    `Assoc [
      ("type", `String "redirect");
      ("status", `Int status);
      ("location", `String location);
    ]
  | ActionError (status, message) ->
    `Assoc [
      ("type", `String "error");
      ("status", `Int status);
      ("message", `String message);
    ]

(** Form data to JSON *)
let form_to_json form =
  let rec value_to_json = function
    | Text s -> `String s
    | File f -> `Assoc [
        ("filename", `String f.filename);
        ("contentType", `String f.content_type);
      ]
    | Multiple vs -> `List (List.map value_to_json vs)
  in
  `Assoc (List.map (fun (k, v) -> (k, value_to_json v)) form)
