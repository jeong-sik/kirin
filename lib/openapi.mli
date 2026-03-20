(** OpenAPI/Swagger Support

    Automatic API documentation generation with Swagger UI integration.
    Re-exports all types and builders from {!Openapi_schema}.

    @since 1.0.0
    @status stable

    Type definitions and builder functions are in {!Openapi_schema}.
    UI rendering is in {!Openapi_ui}. *)

(** {1 Re-exported types from Openapi_schema} *)

include module type of Openapi_schema

(** {1 JSON Encoding} *)

(** [schema_type_to_string t] converts a schema type to its JSON string representation. *)
val schema_type_to_string : schema_type -> string

(** [param_in_to_string p] converts a parameter location to its JSON string representation. *)
val param_in_to_string : param_in -> string

(** [schema_to_json schema] converts a schema to JSON. *)
val schema_to_json : schema -> Yojson.Safe.t

(** [parameter_to_json param] converts a parameter to JSON. *)
val parameter_to_json : parameter -> Yojson.Safe.t

(** [media_type_to_json mt] converts a media type to JSON. *)
val media_type_to_json : media_type -> Yojson.Safe.t

(** [request_body_to_json rb] converts a request body to JSON. *)
val request_body_to_json : request_body -> Yojson.Safe.t

(** [response_to_json resp] converts a response to JSON. *)
val response_to_json : response -> Yojson.Safe.t

(** [operation_to_json op] converts an operation to JSON. *)
val operation_to_json : operation -> Yojson.Safe.t

(** [path_item_to_json pi] converts a path item to JSON. *)
val path_item_to_json : path_item -> Yojson.Safe.t

(** [security_scheme_to_json scheme] converts a security scheme to JSON. *)
val security_scheme_to_json : security_scheme -> Yojson.Safe.t

(** [components_to_json comp] converts components to JSON. *)
val components_to_json : components -> Yojson.Safe.t

(** [info_to_json info] converts API info to JSON. *)
val info_to_json : info -> Yojson.Safe.t

(** [server_to_json srv] converts a server to JSON. *)
val server_to_json : server -> Yojson.Safe.t

(** [tag_to_json t] converts a tag to JSON. *)
val tag_to_json : tag -> Yojson.Safe.t

(** [to_json spec] converts the full OpenAPI spec to JSON. *)
val to_json : t -> Yojson.Safe.t

(** [to_json_string ?pretty spec] converts the spec to a JSON string.
    @param pretty Pretty-print the output (default: true) *)
val to_json_string : ?pretty:bool -> t -> string

(** {1 YAML Export} *)

(** [to_yaml_string spec] converts to YAML string (JSON-compatible YAML). *)
val to_yaml_string : t -> string

(** {1 Route Integration} *)

(** [spec_json spec] returns the JSON spec as a Yojson value (for Kirin route handlers). *)
val spec_json : t -> Yojson.Safe.t

(** [swagger_ui ?spec_url spec] returns Swagger UI HTML.
    @param spec_url URL for the OpenAPI JSON spec (default: "/openapi.json") *)
val swagger_ui : ?spec_url:string -> t -> string

(** [redoc ?spec_url spec] returns ReDoc HTML.
    @param spec_url URL for the OpenAPI JSON spec (default: "/openapi.json") *)
val redoc : ?spec_url:string -> t -> string
