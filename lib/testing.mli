(** @since 1.0.0
    @status stable *)

module Test_request :
  sig
    type t =
      Testing_request.t = {
      meth : Http.Method.t;
      path : string;
      headers : (string * string) list;
      body : string;
      query : (string * string) list;
    }
    val empty : t
    val with_method : Http.Method.t -> t -> t
    val with_path : string -> t -> t
    val with_header : string -> string -> t -> t
    val with_headers : (string * string) list -> t -> t
    val with_body : string -> t -> t
    val with_json_body : Yojson.Safe.t -> t -> t
    val with_query : string -> string -> t -> t
    val with_queries : (string * string) list -> t -> t
    val get :
      ?headers:(string * string) list ->
      ?query:(string * string) list -> string -> t
    val post :
      ?headers:(string * string) list ->
      ?body:string -> ?content_type:string -> string -> t
    val post_json :
      ?headers:(string * string) list -> string -> Yojson.Safe.t -> t
    val put :
      ?headers:(string * string) list ->
      ?body:string -> ?content_type:string -> string -> t
    val put_json :
      ?headers:(string * string) list -> string -> Yojson.Safe.t -> t
    val delete : ?headers:(string * string) list -> string -> t
    val patch :
      ?headers:(string * string) list -> ?body:string -> string -> t
    val with_bearer_token : string -> t -> t
    val with_basic_auth : string -> string -> t -> t
    val with_accept : string -> t -> t
    val with_cookie : string -> string -> t -> t
  end
module Test_response :
  sig
    type t =
      Testing_response.t = {
      status : Http.Status.t;
      headers : (string * string) list;
      body : string;
    }
    val status : t -> Http.Status.t
    val status_code : t -> int
    val header : string -> t -> string option
    val headers : t -> (string * string) list
    val body : t -> string
    val json : t -> Yojson.Safe.t option
    val is_success : t -> bool
    val is_redirect : t -> bool
    val is_client_error : t -> bool
    val is_server_error : t -> bool
  end
module Json_path :
  sig
    val get :
      string ->
      ([> `Assoc of (string * 'a) list | `List of 'a list ] as 'a) ->
      'a option
    val exists :
      string ->
      ([> `Assoc of (string * 'a) list | `List of 'a list ] as 'a) -> bool
    val get_string :
      string ->
      ([> `Assoc of (string * 'a) list | `List of 'a list | `String of 'b ]
       as 'a) ->
      'b option
    val get_int :
      string ->
      ([> `Assoc of (string * 'a) list | `Int of 'b | `List of 'a list ]
       as 'a) ->
      'b option
    val get_bool :
      string ->
      ([> `Assoc of (string * 'a) list | `Bool of 'b | `List of 'a list ]
       as 'a) ->
      'b option
    val get_list :
      string ->
      ([> `Assoc of (string * 'a) list | `List of 'a list ] as 'a) ->
      'a list option
  end
module Assert :
  sig
    exception Assertion_error of string
    val fail : string -> 'a
    val equal : ?msg:string -> Yojson.Safe.t -> Yojson.Safe.t -> unit
    val is_true : ?msg:string -> bool -> unit
    val is_false : ?msg:string -> bool -> unit
    val status : Http.Status.t -> Testing_response.t -> unit
    val status_code : int -> Testing_response.t -> unit
    val success : Testing_response.t -> unit
    val header_exists : string -> Testing_response.t -> unit
    val header : string -> string -> Testing_response.t -> unit
    val header_contains :
      string -> string -> Testing_response.t -> unit
    val body : string -> Testing_response.t -> unit
    val body_contains : string -> Testing_response.t -> unit
    val json : Yojson.Safe.t -> Testing_response.t -> unit
    val json_path :
      string -> Yojson.Safe.t -> Testing_response.t -> unit
    val json_path_exists : string -> Testing_response.t -> unit
    val json_path_string :
      string -> string -> Testing_response.t -> unit
    val json_path_int : string -> int -> Testing_response.t -> unit
    val json_path_bool : string -> bool -> Testing_response.t -> unit
    val redirect_to : string -> Testing_response.t -> unit
    val content_type : string -> Testing_response.t -> unit
    val is_json : Testing_response.t -> unit
    val is_html : Testing_response.t -> unit
  end
module Mock :
  sig
    type endpoint =
      Testing_mock.endpoint = {
      meth : Http.Method.t option;
      path : string;
      response : Testing_response.t;
      mutable call_count : int;
    }
    type t =
      Testing_mock.t = {
      mutable endpoints : endpoint list;
      mutable unmatched_requests : Testing_request.t list;
    }
    val create : unit -> t
    val on :
      ?meth:Http.Method.t option ->
      path:string ->
      status:Http.Status.t ->
      ?headers:(string * string) list -> ?body:string -> t -> t
    val on_json :
      ?meth:Http.Method.t option ->
      path:string ->
      status:Http.Status.t ->
      ?headers:(string * string) list -> Yojson.Safe.t -> t -> t
    val handle : t -> Testing_request.t -> Testing_response.t
    val call_count : path:string -> t -> int
    val unmatched : t -> Testing_request.t list
    val reset : t -> unit
    val assert_called : path:string -> t -> unit
    val assert_called_times : path:string -> times:int -> t -> unit
    val assert_no_unmatched : t -> unit
  end
val run_test : string -> (unit -> unit) -> bool
val run_tests : (string * (unit -> unit)) list -> bool
val random_string : ?length:int -> unit -> string
val random_email : unit -> string
val random_int : min:int -> max:int -> unit -> int
val with_temp_dir : (string -> 'a) -> 'a
val with_temp_file : ?content:string -> (string -> 'a) -> 'a
