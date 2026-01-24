(** Vue/Nuxt Server Actions

    Nuxt server API routes and form actions. *)

(** {1 HTTP Methods} *)

(** HTTP method for server routes *)
type http_method =
  | GET
  | POST
  | PUT
  | PATCH
  | DELETE
  | HEAD
  | OPTIONS

(** Method to string *)
let method_to_string = function
  | GET -> "GET"
  | POST -> "POST"
  | PUT -> "PUT"
  | PATCH -> "PATCH"
  | DELETE -> "DELETE"
  | HEAD -> "HEAD"
  | OPTIONS -> "OPTIONS"

(** {1 Action Context} *)

(** H3 event context (Nuxt server) *)
type event_context = {
  url: string;
  method_: http_method;
  params: (string * string) list;
  query: (string * string) list;
  headers: (string * string) list;
  body: Yojson.Safe.t option;
  raw_body: string option;
}

(** Create event context *)
let create_context ~url ~method_ ?(params=[]) ?(query=[]) ?(headers=[])
    ?body ?raw_body () = {
  url;
  method_;
  params;
  query;
  headers;
  body;
  raw_body;
}

(** Get param *)
let get_param ctx name =
  List.assoc_opt name ctx.params

(** Get query param *)
let get_query ctx name =
  List.assoc_opt name ctx.query

(** Get header *)
let get_header ctx name =
  List.assoc_opt (String.lowercase_ascii name) ctx.headers

(** Read JSON body *)
let read_body ctx =
  ctx.body

(** Read raw body *)
let read_raw_body ctx =
  ctx.raw_body

(** {1 Action Results} *)

(** Action result type *)
type action_result =
  | ActionData of Yojson.Safe.t
  | ActionRedirect of int * string
  | ActionError of int * string
  | ActionEmpty
  | ActionStream of string  (* Stream identifier *)

(** Return JSON data *)
let json data = ActionData data

(** Return redirect *)
let redirect ?(status=302) url = ActionRedirect (status, url)

(** Return error *)
let error status message = ActionError (status, message)

(** Return empty (204) *)
let empty = ActionEmpty

(** Return stream *)
let stream id = ActionStream id

(** {1 Server Route Definition} *)

(** Server route handler *)
type handler = event_context -> action_result

(** Server route definition *)
type server_route = {
  path: string;
  method_: http_method option;  (* None = all methods *)
  handler: handler;
  middleware: string list;
}

(** API route for code generation (simplified) *)
type api_route = {
  path: string;
  method_: http_method;
}

(** Create server route *)
let route path handler = {
  path;
  method_ = None;
  handler;
  middleware = [];
}

(** Create GET route *)
let get path handler = {
  path;
  method_ = Some GET;
  handler;
  middleware = [];
}

(** Create POST route *)
let post path handler = {
  path;
  method_ = Some POST;
  handler;
  middleware = [];
}

(** Create PUT route *)
let put path handler = {
  path;
  method_ = Some PUT;
  handler;
  middleware = [];
}

(** Create DELETE route *)
let delete path handler = {
  path;
  method_ = Some DELETE;
  handler;
  middleware = [];
}

(** Add middleware to route *)
let with_middleware mw route =
  { route with middleware = mw :: route.middleware }

(** {1 Form Handling} *)

(** File info *)
type file_info = {
  file_name: string;
  content_type: string;
  file_size: int;
}

(** Form field value *)
type form_value =
  | Text of string
  | Number of float
  | Bool of bool
  | File of file_info
  | Multiple of form_value list

(** Form data *)
type form_data = (string * form_value) list

(** Get text field *)
let get_text form name =
  match List.assoc_opt name form with
  | Some (Text s) -> Some s
  | _ -> None

(** Get text field or fail *)
let get_text_exn form name =
  match get_text form name with
  | Some v -> v
  | None -> failwith ("Missing form field: " ^ name)

(** Get number field *)
let get_number form name =
  match List.assoc_opt name form with
  | Some (Number n) -> Some n
  | Some (Text s) -> (try Some (float_of_string s) with _ -> None)
  | _ -> None

(** Get bool field *)
let get_bool form name =
  match List.assoc_opt name form with
  | Some (Bool b) -> Some b
  | Some (Text "true") -> Some true
  | Some (Text "false") -> Some false
  | Some (Text "on") -> Some true
  | Some (Text "") -> Some false
  | _ -> None

(** Get file field *)
let get_file form name =
  match List.assoc_opt name form with
  | Some (File f) -> Some f
  | _ -> None

(** {1 Validation} *)

(** Validation error *)
type validation_error = {
  field: string;
  message: string;
  code: string;
}

(** Create validation error *)
let validation_error ~field ~message ?(code="invalid") () =
  { field; message; code }

(** Validate required *)
let required form name =
  match get_text form name with
  | Some s when String.length s > 0 -> Ok s
  | _ -> Error (validation_error ~field:name ~message:"Required" ~code:"required" ())

(** Validate email *)
let email form name =
  match get_text form name with
  | Some s when String.contains s '@' && String.contains s '.' -> Ok s
  | Some _ -> Error (validation_error ~field:name ~message:"Invalid email" ~code:"email" ())
  | None -> Error (validation_error ~field:name ~message:"Required" ~code:"required" ())

(** Validate min length *)
let min_length len form name =
  match get_text form name with
  | Some s when String.length s >= len -> Ok s
  | Some _ -> Error (validation_error ~field:name
      ~message:(Printf.sprintf "Minimum %d characters" len) ~code:"minLength" ())
  | None -> Error (validation_error ~field:name ~message:"Required" ~code:"required" ())

(** {1 Response Helpers} *)

(** Set response header (returns modified result) *)
let set_header _name _value result =
  (* In real implementation, this would modify response headers *)
  result

(** Set status code *)
let set_status code result =
  match result with
  | ActionData d -> ActionData d  (* Status handled separately *)
  | ActionRedirect (_, url) -> ActionRedirect (code, url)
  | ActionError (_, msg) -> ActionError (code, msg)
  | ActionEmpty -> ActionEmpty
  | ActionStream id -> ActionStream id

(** {1 Serialization} *)

(** Action result to JSON *)
let result_to_json = function
  | ActionData data ->
    `Assoc [("type", `String "data"); ("data", data)]
  | ActionRedirect (status, url) ->
    `Assoc [
      ("type", `String "redirect");
      ("status", `Int status);
      ("location", `String url);
    ]
  | ActionError (status, msg) ->
    `Assoc [
      ("type", `String "error");
      ("status", `Int status);
      ("message", `String msg);
    ]
  | ActionEmpty ->
    `Assoc [("type", `String "empty")]
  | ActionStream id ->
    `Assoc [("type", `String "stream"); ("id", `String id)]

(** Validation error to JSON *)
let validation_error_to_json err =
  `Assoc [
    ("field", `String err.field);
    ("message", `String err.message);
    ("code", `String err.code);
  ]

(** Server route to JSON *)
let route_to_json (route : server_route) =
  `Assoc [
    ("path", `String route.path);
    ("method", match route.method_ with
      | Some m -> `String (method_to_string m)
      | None -> `Null);
    ("middleware", `List (List.map (fun m -> `String m) route.middleware));
  ]
