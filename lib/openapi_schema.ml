(** OpenAPI Schema Types and Builders

    Type definitions and builder functions for constructing OpenAPI 3.0.3
    specifications. JSON encoding and UI rendering are in {!Openapi}
    and {!Openapi_ui} respectively.
*)

(** {1 Schema Types} *)

(** JSON Schema type *)
type schema_type =
  | String
  | Integer
  | Number
  | Boolean
  | Array
  | Object
  | Null

(** JSON Schema definition *)
type schema =
  { schema_type : schema_type option
  ; format : string option
  ; title : string option
  ; description : string option
  ; default : Yojson.Safe.t option
  ; enum : Yojson.Safe.t list option
  ; items : schema option
  ; properties : (string * schema) list option
  ; required : string list option
  ; minimum : float option
  ; maximum : float option
  ; min_length : int option
  ; max_length : int option
  ; pattern : string option
  ; nullable : bool
  ; example : Yojson.Safe.t option
  ; ref_path : string option (* $ref *)
  }

(** {1 Parameter Types} *)

(** Parameter location *)
type param_in =
  | Path
  | Query
  | Header
  | Cookie

(** API parameter *)
type parameter =
  { name : string
  ; param_in : param_in
  ; description : string option
  ; required : bool
  ; schema : schema
  ; example : Yojson.Safe.t option
  ; deprecated : bool
  }

(** {1 Request/Response Types} *)

(** Media type content *)
type media_type =
  { media_schema : schema option
  ; media_example : Yojson.Safe.t option
  ; media_examples : (string * Yojson.Safe.t) list option
  }

(** Request body *)
type request_body =
  { body_description : string option
  ; body_content : (string * media_type) list
  ; body_required : bool
  }

(** Response definition *)
type response =
  { response_description : string
  ; response_content : (string * media_type) list option
  ; response_headers : (string * parameter) list option
  }

(** {1 Operation Types} *)

(** Security requirement *)
type security_requirement = (string * string list) list

(** API operation *)
type operation =
  { summary : string option
  ; description : string option
  ; operation_id : string option
  ; tags : string list
  ; parameters : parameter list
  ; request_body : request_body option
  ; responses : (int * response) list
  ; deprecated : bool
  ; security : security_requirement list option
  }

(** Path item *)
type path_item =
  { path_summary : string option
  ; path_description : string option
  ; get : operation option
  ; post : operation option
  ; put : operation option
  ; delete : operation option
  ; patch : operation option
  ; options : operation option
  ; head : operation option
  ; path_parameters : parameter list
  }

(** {1 Security Scheme Types} *)

(** Security scheme type *)
type security_scheme_type =
  | ApiKey of
      { name : string
      ; scheme_in : param_in
      }
  | Http of
      { scheme : string
      ; bearer_format : string option
      }
  | OAuth2 of { flows : oauth_flows }
  | OpenIdConnect of { openid_connect_url : string }

and oauth_flows =
  { implicit : oauth_flow option
  ; password : oauth_flow option
  ; client_credentials : oauth_flow option
  ; authorization_code : oauth_flow option
  }

and oauth_flow =
  { authorization_url : string option
  ; token_url : string option
  ; refresh_url : string option
  ; scopes : (string * string) list
  }

(** Security scheme *)
type security_scheme =
  { scheme_type : security_scheme_type
  ; scheme_description : string option
  }

(** {1 Info Types} *)

(** Contact information *)
type contact =
  { contact_name : string option
  ; contact_url : string option
  ; contact_email : string option
  }

(** License information *)
type license =
  { license_name : string
  ; license_url : string option
  }

(** API info *)
type info =
  { title : string
  ; version : string
  ; description : string option
  ; terms_of_service : string option
  ; contact : contact option
  ; license : license option
  }

(** Server definition *)
type server =
  { url : string
  ; server_description : string option
  ; variables : (string * server_variable) list option
  }

and server_variable =
  { var_default : string
  ; var_enum : string list option
  ; var_description : string option
  }

(** Tag definition *)
type tag =
  { tag_name : string
  ; tag_description : string option
  ; external_docs : external_doc option
  }

and external_doc =
  { doc_description : string option
  ; doc_url : string
  }

(** {1 OpenAPI Spec} *)

(** Components (reusable definitions) *)
type components =
  { schemas : (string * schema) list
  ; security_schemes : (string * security_scheme) list
  ; parameters : (string * parameter) list
  ; responses : (string * response) list
  ; request_bodies : (string * request_body) list
  }

(** OpenAPI specification *)
type t =
  { openapi : string (* Always "3.0.3" *)
  ; info : info
  ; servers : server list
  ; paths : (string * path_item) list
  ; components : components option
  ; security : security_requirement list option
  ; tags : tag list
  ; external_docs : external_doc option
  }

(** {1 Schema Builders} *)

let empty_schema =
  { schema_type = None
  ; format = None
  ; title = None
  ; description = None
  ; default = None
  ; enum = None
  ; items = None
  ; properties = None
  ; required = None
  ; minimum = None
  ; maximum = None
  ; min_length = None
  ; max_length = None
  ; pattern = None
  ; nullable = false
  ; example = None
  ; ref_path = None
  }
;;

let string
      ?format
      ?description
      ?default
      ?enum
      ?min_length
      ?max_length
      ?pattern
      ?example
      ()
  =
  { empty_schema with
    schema_type = Some String
  ; format
  ; description
  ; default
  ; enum
  ; min_length
  ; max_length
  ; pattern
  ; example
  }
;;

let integer ?format ?description ?default ?minimum ?maximum ?example () =
  { empty_schema with
    schema_type = Some Integer
  ; format
  ; description
  ; default
  ; minimum
  ; maximum
  ; example
  }
;;

let number ?format ?description ?default ?minimum ?maximum ?example () =
  { empty_schema with
    schema_type = Some Number
  ; format
  ; description
  ; default
  ; minimum
  ; maximum
  ; example
  }
;;

let boolean ?description ?default ?example () =
  { empty_schema with schema_type = Some Boolean; description; default; example }
;;

let array ?description ?example items =
  { empty_schema with schema_type = Some Array; description; items = Some items; example }
;;

let object_ ?description ?required ?example properties =
  { empty_schema with
    schema_type = Some Object
  ; description
  ; properties = Some properties
  ; required
  ; example
  }
;;

let nullable schema = { schema with nullable = true }
let ref_ path = { empty_schema with ref_path = Some path }

(** {1 Parameter Builders} *)

let path_param ~name ?description ?(schema = string ()) ?example () =
  { name
  ; param_in = Path
  ; description
  ; required = true
  ; schema
  ; example
  ; deprecated = false
  }
;;

let query_param ~name ?description ?(required = false) ?(schema = string ()) ?example () =
  { name; param_in = Query; description; required; schema; example; deprecated = false }
;;

let header_param ~name ?description ?(required = false) ?(schema = string ()) ?example () =
  { name; param_in = Header; description; required; schema; example; deprecated = false }
;;

(** {1 Request Body Builders} *)

let json_body ?description ?(required = true) schema =
  { body_description = description
  ; body_content =
      [ ( "application/json"
        , { media_schema = Some schema; media_example = None; media_examples = None } )
      ]
  ; body_required = required
  }
;;

let form_body ?description ?(required = true) schema =
  { body_description = description
  ; body_content =
      [ ( "application/x-www-form-urlencoded"
        , { media_schema = Some schema; media_example = None; media_examples = None } )
      ]
  ; body_required = required
  }
;;

let multipart_body ?description ?(required = true) schema =
  { body_description = description
  ; body_content =
      [ ( "multipart/form-data"
        , { media_schema = Some schema; media_example = None; media_examples = None } )
      ]
  ; body_required = required
  }
;;

(** {1 Response Builders} *)

let response ~description ?content () =
  let response_content =
    match content with
    | None -> None
    | Some schema ->
      Some
        [ ( "application/json"
          , { media_schema = Some schema; media_example = None; media_examples = None } )
        ]
  in
  { response_description = description; response_content; response_headers = None }
;;

let empty_response description =
  { response_description = description; response_content = None; response_headers = None }
;;

(** {1 Operation Builder} *)

let operation
      ?summary
      ?description
      ?operation_id
      ?(tags = [])
      ?(parameters = [])
      ?request_body
      ?(responses = [])
      ?(deprecated = false)
      ?security
      ()
  =
  { summary
  ; description
  ; operation_id
  ; tags
  ; parameters
  ; request_body
  ; responses
  ; deprecated
  ; security
  }
;;

(** {1 Spec Builder} *)

let empty_components =
  { schemas = []
  ; security_schemes = []
  ; parameters = []
  ; responses = []
  ; request_bodies = []
  }
;;

let empty_path_item =
  { path_summary = None
  ; path_description = None
  ; get = None
  ; post = None
  ; put = None
  ; delete = None
  ; patch = None
  ; options = None
  ; head = None
  ; path_parameters = []
  }
;;

let create
      ~title
      ~version
      ?description
      ?terms_of_service
      ?contact
      ?license
      ?(servers = [])
      ?(tags = [])
      ?external_docs
      ?components
      ?security
      ()
  =
  { openapi = "3.0.3"
  ; info = { title; version; description; terms_of_service; contact; license }
  ; servers
  ; paths = []
  ; components
  ; security
  ; tags
  ; external_docs
  }
;;

(** Add path to spec *)
let add_path spec ~path path_item = { spec with paths = (path, path_item) :: spec.paths }

(** Add schema to components *)
let add_schema spec ~name schema =
  let components =
    match spec.components with
    | None -> { empty_components with schemas = [ name, schema ] }
    | Some c -> { c with schemas = (name, schema) :: c.schemas }
  in
  { spec with components = Some components }
;;

(** Add security scheme *)
let add_security_scheme spec ~name scheme =
  let components =
    match spec.components with
    | None -> { empty_components with security_schemes = [ name, scheme ] }
    | Some c -> { c with security_schemes = (name, scheme) :: c.security_schemes }
  in
  { spec with components = Some components }
;;

(** Convenience: create a (name, schema) field pair for use in [object_] *)
let field name schema = name, schema
