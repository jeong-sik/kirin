type form_data = (string * string) list
type action_context = {
  request_url : string;
  request_method : string;
  params : (string * string) list;
  headers : (string * string) list;
  form_data : form_data;
}
type 'a action_result =
    Success of 'a
  | Redirect of string * int
  | ValidationError of (string * string) list
  | ServerError of string
val context_of_request :
  form_data:form_data -> Kirin.Request.t -> action_context
val with_params : (string * string) list -> action_context -> action_context
val field : action_context -> string -> string option
val required_field : action_context -> string -> string option
val field_values : action_context -> string -> string list
type 'a action = action_context -> 'a action_result
val map : ('a -> 'b) -> 'a action_result -> 'b action_result
val validate :
  validators:(string * (string -> bool) * string) list ->
  action_context -> form_data action_result
val not_empty : string -> bool
val min_length : int -> string -> bool
val max_length : int -> string -> bool
val matches_regex : string -> string -> bool
val is_email : string -> bool
val is_numeric : string -> bool
val succeed : 'a -> 'b -> 'a action_result
val redirect_action : ?status:int -> string -> 'a -> 'b action_result
val error_action : string -> 'a -> 'b action_result
val check_csrf :
  get_session_token:(action_context -> string option) ->
  action_context -> bool
val require_csrf :
  get_session_token:(action_context -> string option) ->
  (action_context -> 'a action_result) -> action_context -> 'a action_result
val result_to_json :
  ('a ->
   ([> `Assoc of (string * [> `Int of int | `String of string ]) list
     | `Bool of bool
     | `String of string ]
    as 'b)) ->
  'a action_result -> [> `Assoc of (string * 'b) list ]
val run_action :
  to_json:('a -> Yojson.Safe.t) ->
  ('b -> 'a action_result) -> 'b -> Kirin.Response.t
val endpoint :
  parse_form:(Kirin.Request.t -> form_data) ->
  to_json:('a -> Yojson.Safe.t) ->
  (action_context -> 'a action_result) -> Kirin.Request.t -> Kirin.Response.t
val context_to_json :
  action_context ->
  [> `Assoc of
       (string *
        [> `Assoc of (string * [> `String of string ]) list
         | `String of string ])
       list ]
