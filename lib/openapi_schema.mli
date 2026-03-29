(** OpenAPI Schema Types and Builders

    Type definitions and builder functions for constructing OpenAPI 3.0.3
    specifications. JSON encoding and UI rendering are in {!Openapi}
    and {!Openapi_ui} respectively. *)

(** {1 Schema Types} *)

(** JSON Schema type. *)
type schema_type =
  | String
  | Integer
  | Number
  | Boolean
  | Array
  | Object
  | Null

(** JSON Schema definition. *)
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
  ; ref_path : string option
  }

(** {1 Parameter Types} *)

(** Parameter location. *)
type param_in =
  | Path
  | Query
  | Header
  | Cookie

(** API parameter. *)
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

(** Media type content. *)
type media_type =
  { media_schema : schema option
  ; media_example : Yojson.Safe.t option
  ; media_examples : (string * Yojson.Safe.t) list option
  }

(** Request body. *)
type request_body =
  { body_description : string option
  ; body_content : (string * media_type) list
  ; body_required : bool
  }

(** Response definition. *)
type response =
  { response_description : string
  ; response_content : (string * media_type) list option
  ; response_headers : (string * parameter) list option
  }

(** {1 Operation Types} *)

(** Security requirement. *)
type security_requirement = (string * string list) list

(** API operation. *)
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

(** Path item. *)
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

(** Security scheme type. *)
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

(** Security scheme. *)
type security_scheme =
  { scheme_type : security_scheme_type
  ; scheme_description : string option
  }

(** {1 Info Types} *)

(** Contact information. *)
type contact =
  { contact_name : string option
  ; contact_url : string option
  ; contact_email : string option
  }

(** License information. *)
type license =
  { license_name : string
  ; license_url : string option
  }

(** API info. *)
type info =
  { title : string
  ; version : string
  ; description : string option
  ; terms_of_service : string option
  ; contact : contact option
  ; license : license option
  }

(** Server definition. *)
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

(** Tag definition. *)
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

(** Components (reusable definitions). *)
type components =
  { schemas : (string * schema) list
  ; security_schemes : (string * security_scheme) list
  ; parameters : (string * parameter) list
  ; responses : (string * response) list
  ; request_bodies : (string * request_body) list
  }

(** OpenAPI specification. *)
type t =
  { openapi : string
  ; info : info
  ; servers : server list
  ; paths : (string * path_item) list
  ; components : components option
  ; security : security_requirement list option
  ; tags : tag list
  ; external_docs : external_doc option
  }

(** {1 Schema Builders} *)

(** Empty schema with all fields set to None/false. *)
val empty_schema : schema

(** [string ?format ?description ?default ?enum ?min_length ?max_length ?pattern ?example ()]
    creates a string schema. *)
val string
  :  ?format:string
  -> ?description:string
  -> ?default:Yojson.Safe.t
  -> ?enum:Yojson.Safe.t list
  -> ?min_length:int
  -> ?max_length:int
  -> ?pattern:string
  -> ?example:Yojson.Safe.t
  -> unit
  -> schema

(** [integer ?format ?description ?default ?minimum ?maximum ?example ()]
    creates an integer schema. *)
val integer
  :  ?format:string
  -> ?description:string
  -> ?default:Yojson.Safe.t
  -> ?minimum:float
  -> ?maximum:float
  -> ?example:Yojson.Safe.t
  -> unit
  -> schema

(** [number ?format ?description ?default ?minimum ?maximum ?example ()]
    creates a number schema. *)
val number
  :  ?format:string
  -> ?description:string
  -> ?default:Yojson.Safe.t
  -> ?minimum:float
  -> ?maximum:float
  -> ?example:Yojson.Safe.t
  -> unit
  -> schema

(** [boolean ?description ?default ?example ()] creates a boolean schema. *)
val boolean
  :  ?description:string
  -> ?default:Yojson.Safe.t
  -> ?example:Yojson.Safe.t
  -> unit
  -> schema

(** [array ?description ?example items] creates an array schema. *)
val array : ?description:string -> ?example:Yojson.Safe.t -> schema -> schema

(** [object_ ?description ?required ?example properties] creates an object schema. *)
val object_
  :  ?description:string
  -> ?required:string list
  -> ?example:Yojson.Safe.t
  -> (string * schema) list
  -> schema

(** [nullable schema] marks a schema as nullable. *)
val nullable : schema -> schema

(** [ref_ path] creates a $ref schema. *)
val ref_ : string -> schema

(** {1 Parameter Builders} *)

(** [path_param ~name ?description ?schema ?example ()] creates a path parameter (always required). *)
val path_param
  :  name:string
  -> ?description:string
  -> ?schema:schema
  -> ?example:Yojson.Safe.t
  -> unit
  -> parameter

(** [query_param ~name ?description ?required ?schema ?example ()] creates a query parameter. *)
val query_param
  :  name:string
  -> ?description:string
  -> ?required:bool
  -> ?schema:schema
  -> ?example:Yojson.Safe.t
  -> unit
  -> parameter

(** [header_param ~name ?description ?required ?schema ?example ()] creates a header parameter. *)
val header_param
  :  name:string
  -> ?description:string
  -> ?required:bool
  -> ?schema:schema
  -> ?example:Yojson.Safe.t
  -> unit
  -> parameter

(** {1 Request Body Builders} *)

(** [json_body ?description ?required schema] creates a JSON request body. *)
val json_body : ?description:string -> ?required:bool -> schema -> request_body

(** [form_body ?description ?required schema] creates a form-urlencoded request body. *)
val form_body : ?description:string -> ?required:bool -> schema -> request_body

(** [multipart_body ?description ?required schema] creates a multipart request body. *)
val multipart_body : ?description:string -> ?required:bool -> schema -> request_body

(** {1 Response Builders} *)

(** [response ~description ?content ()] creates a response definition. *)
val response : description:string -> ?content:schema -> unit -> response

(** [empty_response description] creates a response with no content. *)
val empty_response : string -> response

(** {1 Operation Builder} *)

(** [operation ?summary ?description ?operation_id ?tags ?parameters
    ?request_body ?responses ?deprecated ?security ()] creates an API operation. *)
val operation
  :  ?summary:string
  -> ?description:string
  -> ?operation_id:string
  -> ?tags:string list
  -> ?parameters:parameter list
  -> ?request_body:request_body
  -> ?responses:(int * response) list
  -> ?deprecated:bool
  -> ?security:security_requirement list
  -> unit
  -> operation

(** {1 Spec Builder} *)

(** Empty components. *)
val empty_components : components

(** Empty path item. *)
val empty_path_item : path_item

(** [create ~title ~version ?description ?terms_of_service ?contact
    ?license ?servers ?tags ?external_docs ?components ?security ()]
    creates a new OpenAPI specification. *)
val create
  :  title:string
  -> version:string
  -> ?description:string
  -> ?terms_of_service:string
  -> ?contact:contact
  -> ?license:license
  -> ?servers:server list
  -> ?tags:tag list
  -> ?external_docs:external_doc
  -> ?components:components
  -> ?security:security_requirement list
  -> unit
  -> t

(** [add_path spec ~path path_item] adds a path to the spec. *)
val add_path : t -> path:string -> path_item -> t

(** [add_schema spec ~name schema] adds a schema to the spec's components. *)
val add_schema : t -> name:string -> schema -> t

(** [add_security_scheme spec ~name scheme] adds a security scheme to the spec's components. *)
val add_security_scheme : t -> name:string -> security_scheme -> t

(** [field name schema] creates a (name, schema) pair for use in [object_]. *)
val field : string -> schema -> string * schema
