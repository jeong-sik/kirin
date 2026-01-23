(** OpenAPI/Swagger Support (Phase 14)

    Automatic API documentation generation with Swagger UI integration.

    {b Quick Start:}
    {[
      (* Create OpenAPI spec *)
      let spec = Openapi.create
        ~title:"My API"
        ~version:"1.0.0"
        ~description:"Example API" ()

      (* Add schema *)
      let user_schema = Openapi.(object_ [
        field "name" (string ());
        field "email" (string ~format:"email" ());
      ] ~required:["name"; "email"])

      (* Kirin integration *)
      let docs_routes = [
        Router.get "/openapi.json" (fun _ ->
          Response.json (Openapi.spec_json spec));
        Router.get "/docs" (fun _ ->
          Response.html (Openapi.swagger_ui spec));
      ]
    ]}
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
type schema = {
  schema_type : schema_type option;
  format : string option;
  title : string option;
  description : string option;
  default : Yojson.Safe.t option;
  enum : Yojson.Safe.t list option;
  items : schema option;
  properties : (string * schema) list option;
  required : string list option;
  minimum : float option;
  maximum : float option;
  min_length : int option;
  max_length : int option;
  pattern : string option;
  nullable : bool;
  example : Yojson.Safe.t option;
  ref_path : string option;  (* $ref *)
}

(** {1 Parameter Types} *)

(** Parameter location *)
type param_in = Path | Query | Header | Cookie

(** API parameter *)
type parameter = {
  name : string;
  param_in : param_in;
  description : string option;
  required : bool;
  schema : schema;
  example : Yojson.Safe.t option;
  deprecated : bool;
}

(** {1 Request/Response Types} *)

(** Media type content *)
type media_type = {
  media_schema : schema option;
  media_example : Yojson.Safe.t option;
  media_examples : (string * Yojson.Safe.t) list option;
}

(** Request body *)
type request_body = {
  body_description : string option;
  body_content : (string * media_type) list;
  body_required : bool;
}

(** Response definition *)
type response = {
  response_description : string;
  response_content : (string * media_type) list option;
  response_headers : (string * parameter) list option;
}

(** {1 Operation Types} *)

(** Security requirement *)
type security_requirement = (string * string list) list

(** API operation *)
type operation = {
  summary : string option;
  description : string option;
  operation_id : string option;
  tags : string list;
  parameters : parameter list;
  request_body : request_body option;
  responses : (int * response) list;
  deprecated : bool;
  security : security_requirement list option;
}

(** Path item *)
type path_item = {
  path_summary : string option;
  path_description : string option;
  get : operation option;
  post : operation option;
  put : operation option;
  delete : operation option;
  patch : operation option;
  options : operation option;
  head : operation option;
  path_parameters : parameter list;
}

(** {1 Security Scheme Types} *)

(** Security scheme type *)
type security_scheme_type =
  | ApiKey of { name : string; scheme_in : param_in }
  | Http of { scheme : string; bearer_format : string option }
  | OAuth2 of { flows : oauth_flows }
  | OpenIdConnect of { openid_connect_url : string }

and oauth_flows = {
  implicit : oauth_flow option;
  password : oauth_flow option;
  client_credentials : oauth_flow option;
  authorization_code : oauth_flow option;
}

and oauth_flow = {
  authorization_url : string option;
  token_url : string option;
  refresh_url : string option;
  scopes : (string * string) list;
}

(** Security scheme *)
type security_scheme = {
  scheme_type : security_scheme_type;
  scheme_description : string option;
}

(** {1 Info Types} *)

(** Contact information *)
type contact = {
  contact_name : string option;
  contact_url : string option;
  contact_email : string option;
}

(** License information *)
type license = {
  license_name : string;
  license_url : string option;
}

(** API info *)
type info = {
  title : string;
  version : string;
  description : string option;
  terms_of_service : string option;
  contact : contact option;
  license : license option;
}

(** Server definition *)
type server = {
  url : string;
  server_description : string option;
  variables : (string * server_variable) list option;
}

and server_variable = {
  var_default : string;
  var_enum : string list option;
  var_description : string option;
}

(** Tag definition *)
type tag = {
  tag_name : string;
  tag_description : string option;
  external_docs : external_doc option;
}

and external_doc = {
  doc_description : string option;
  doc_url : string;
}

(** {1 OpenAPI Spec} *)

(** Components (reusable definitions) *)
type components = {
  schemas : (string * schema) list;
  security_schemes : (string * security_scheme) list;
  parameters : (string * parameter) list;
  responses : (string * response) list;
  request_bodies : (string * request_body) list;
}

(** OpenAPI specification *)
type t = {
  openapi : string;  (* Always "3.0.3" *)
  info : info;
  servers : server list;
  paths : (string * path_item) list;
  components : components option;
  security : security_requirement list option;
  tags : tag list;
  external_docs : external_doc option;
}

(** {1 Schema Builders} *)

let empty_schema = {
  schema_type = None;
  format = None;
  title = None;
  description = None;
  default = None;
  enum = None;
  items = None;
  properties = None;
  required = None;
  minimum = None;
  maximum = None;
  min_length = None;
  max_length = None;
  pattern = None;
  nullable = false;
  example = None;
  ref_path = None;
}

let string ?format ?description ?default ?enum ?min_length ?max_length ?pattern ?example () =
  { empty_schema with
    schema_type = Some String;
    format;
    description;
    default;
    enum;
    min_length;
    max_length;
    pattern;
    example;
  }

let integer ?format ?description ?default ?minimum ?maximum ?example () =
  { empty_schema with
    schema_type = Some Integer;
    format;
    description;
    default;
    minimum;
    maximum;
    example;
  }

let number ?format ?description ?default ?minimum ?maximum ?example () =
  { empty_schema with
    schema_type = Some Number;
    format;
    description;
    default;
    minimum;
    maximum;
    example;
  }

let boolean ?description ?default ?example () =
  { empty_schema with
    schema_type = Some Boolean;
    description;
    default;
    example;
  }

let array ?description ?example items =
  { empty_schema with
    schema_type = Some Array;
    description;
    items = Some items;
    example;
  }

let object_ ?description ?required ?example properties =
  { empty_schema with
    schema_type = Some Object;
    description;
    properties = Some properties;
    required;
    example;
  }

let nullable schema =
  { schema with nullable = true }

let ref_ path =
  { empty_schema with ref_path = Some path }

(** {1 Parameter Builders} *)

let path_param ~name ?description ?(schema = string ()) ?example () =
  { name; param_in = Path; description; required = true; schema; example; deprecated = false }

let query_param ~name ?description ?(required = false) ?(schema = string ()) ?example () =
  { name; param_in = Query; description; required; schema; example; deprecated = false }

let header_param ~name ?description ?(required = false) ?(schema = string ()) ?example () =
  { name; param_in = Header; description; required; schema; example; deprecated = false }

(** {1 Request Body Builders} *)

let json_body ?description ?(required = true) schema =
  {
    body_description = description;
    body_content = [
      "application/json", { media_schema = Some schema; media_example = None; media_examples = None }
    ];
    body_required = required;
  }

let form_body ?description ?(required = true) schema =
  {
    body_description = description;
    body_content = [
      "application/x-www-form-urlencoded", { media_schema = Some schema; media_example = None; media_examples = None }
    ];
    body_required = required;
  }

let multipart_body ?description ?(required = true) schema =
  {
    body_description = description;
    body_content = [
      "multipart/form-data", { media_schema = Some schema; media_example = None; media_examples = None }
    ];
    body_required = required;
  }

(** {1 Response Builders} *)

let response ~description ?content () =
  let response_content = match content with
    | None -> None
    | Some schema -> Some [
        "application/json", { media_schema = Some schema; media_example = None; media_examples = None }
      ]
  in
  { response_description = description; response_content; response_headers = None }

let empty_response description =
  { response_description = description; response_content = None; response_headers = None }

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
    () =
  { summary; description; operation_id; tags; parameters; request_body; responses; deprecated; security }

(** {1 Spec Builder} *)

let empty_components = {
  schemas = [];
  security_schemes = [];
  parameters = [];
  responses = [];
  request_bodies = [];
}

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
    () =
  {
    openapi = "3.0.3";
    info = { title; version; description; terms_of_service; contact; license };
    servers;
    paths = [];
    components;
    security;
    tags;
    external_docs;
  }

(** Add path to spec *)
let add_path spec ~path path_item =
  { spec with paths = (path, path_item) :: spec.paths }

(** Add schema to components *)
let add_schema spec ~name schema =
  let components = match spec.components with
    | None -> { empty_components with schemas = [(name, schema)] }
    | Some c -> { c with schemas = (name, schema) :: c.schemas }
  in
  { spec with components = Some components }

(** Add security scheme *)
let add_security_scheme spec ~name scheme =
  let components = match spec.components with
    | None -> { empty_components with security_schemes = [(name, scheme)] }
    | Some c -> { c with security_schemes = (name, scheme) :: c.security_schemes }
  in
  { spec with components = Some components }

(** {1 JSON Encoding} *)

let schema_type_to_string = function
  | String -> "string"
  | Integer -> "integer"
  | Number -> "number"
  | Boolean -> "boolean"
  | Array -> "array"
  | Object -> "object"
  | Null -> "null"

let param_in_to_string = function
  | Path -> "path"
  | Query -> "query"
  | Header -> "header"
  | Cookie -> "cookie"

let rec schema_to_json schema =
  match schema.ref_path with
  | Some ref_path -> `Assoc [("$ref", `String ref_path)]
  | None ->
    let fields = [] in
    let fields = match schema.schema_type with
      | None -> fields
      | Some t -> ("type", `String (schema_type_to_string t)) :: fields
    in
    let fields = match schema.format with
      | None -> fields
      | Some f -> ("format", `String f) :: fields
    in
    let fields = match schema.title with
      | None -> fields
      | Some t -> ("title", `String t) :: fields
    in
    let fields = match schema.description with
      | None -> fields
      | Some d -> ("description", `String d) :: fields
    in
    let fields = match schema.default with
      | None -> fields
      | Some d -> ("default", d) :: fields
    in
    let fields = match schema.enum with
      | None -> fields
      | Some e -> ("enum", `List e) :: fields
    in
    let fields = match schema.items with
      | None -> fields
      | Some i -> ("items", schema_to_json i) :: fields
    in
    let fields = match schema.properties with
      | None -> fields
      | Some props ->
        let prop_json = `Assoc (List.map (fun (n, s) -> (n, schema_to_json s)) props) in
        ("properties", prop_json) :: fields
    in
    let fields = match schema.required with
      | None -> fields
      | Some r -> ("required", `List (List.map (fun s -> `String s) r)) :: fields
    in
    let fields = match schema.minimum with
      | None -> fields
      | Some m -> ("minimum", `Float m) :: fields
    in
    let fields = match schema.maximum with
      | None -> fields
      | Some m -> ("maximum", `Float m) :: fields
    in
    let fields = match schema.min_length with
      | None -> fields
      | Some m -> ("minLength", `Int m) :: fields
    in
    let fields = match schema.max_length with
      | None -> fields
      | Some m -> ("maxLength", `Int m) :: fields
    in
    let fields = match schema.pattern with
      | None -> fields
      | Some p -> ("pattern", `String p) :: fields
    in
    let fields = if schema.nullable then ("nullable", `Bool true) :: fields else fields in
    let fields = match schema.example with
      | None -> fields
      | Some e -> ("example", e) :: fields
    in
    `Assoc (List.rev fields)

let parameter_to_json param =
  let fields = [
    ("name", `String param.name);
    ("in", `String (param_in_to_string param.param_in));
    ("required", `Bool param.required);
    ("schema", schema_to_json param.schema);
  ] in
  let fields = match param.description with
    | None -> fields
    | Some d -> ("description", `String d) :: fields
  in
  let fields = match param.example with
    | None -> fields
    | Some e -> ("example", e) :: fields
  in
  let fields = if param.deprecated then ("deprecated", `Bool true) :: fields else fields in
  `Assoc fields

let media_type_to_json mt =
  let fields = [] in
  let fields = match mt.media_schema with
    | None -> fields
    | Some s -> ("schema", schema_to_json s) :: fields
  in
  let fields = match mt.media_example with
    | None -> fields
    | Some e -> ("example", e) :: fields
  in
  let fields = match mt.media_examples with
    | None -> fields
    | Some ex -> ("examples", `Assoc (List.map (fun (n, v) -> (n, v)) ex)) :: fields
  in
  `Assoc fields

let request_body_to_json rb =
  let fields = [
    ("content", `Assoc (List.map (fun (ct, mt) -> (ct, media_type_to_json mt)) rb.body_content));
    ("required", `Bool rb.body_required);
  ] in
  let fields = match rb.body_description with
    | None -> fields
    | Some d -> ("description", `String d) :: fields
  in
  `Assoc fields

let response_to_json resp =
  let fields = [("description", `String resp.response_description)] in
  let fields = match resp.response_content with
    | None -> fields
    | Some content ->
      ("content", `Assoc (List.map (fun (ct, mt) -> (ct, media_type_to_json mt)) content)) :: fields
  in
  `Assoc fields

let operation_to_json op =
  let fields = [] in
  let fields = match op.summary with
    | None -> fields
    | Some s -> ("summary", `String s) :: fields
  in
  let fields = match op.description with
    | None -> fields
    | Some d -> ("description", `String d) :: fields
  in
  let fields = match op.operation_id with
    | None -> fields
    | Some id -> ("operationId", `String id) :: fields
  in
  let fields = if op.tags = [] then fields
    else ("tags", `List (List.map (fun t -> `String t) op.tags)) :: fields
  in
  let fields = if op.parameters = [] then fields
    else ("parameters", `List (List.map parameter_to_json op.parameters)) :: fields
  in
  let fields = match op.request_body with
    | None -> fields
    | Some rb -> ("requestBody", request_body_to_json rb) :: fields
  in
  let fields =
    let responses_json = `Assoc (List.map (fun (code, resp) ->
      (string_of_int code, response_to_json resp)
    ) op.responses) in
    ("responses", responses_json) :: fields
  in
  let fields = if op.deprecated then ("deprecated", `Bool true) :: fields else fields in
  let fields = match op.security with
    | None -> fields
    | Some sec ->
      let sec_json = `List (List.map (fun req ->
        `Assoc (List.map (fun (name, scopes) ->
          (name, `List (List.map (fun s -> `String s) scopes))
        ) req)
      ) sec) in
      ("security", sec_json) :: fields
  in
  `Assoc (List.rev fields)

let path_item_to_json pi =
  let fields = [] in
  let fields = match pi.path_summary with
    | None -> fields
    | Some s -> ("summary", `String s) :: fields
  in
  let fields = match pi.path_description with
    | None -> fields
    | Some d -> ("description", `String d) :: fields
  in
  let fields = match pi.get with
    | None -> fields
    | Some op -> ("get", operation_to_json op) :: fields
  in
  let fields = match pi.post with
    | None -> fields
    | Some op -> ("post", operation_to_json op) :: fields
  in
  let fields = match pi.put with
    | None -> fields
    | Some op -> ("put", operation_to_json op) :: fields
  in
  let fields = match pi.delete with
    | None -> fields
    | Some op -> ("delete", operation_to_json op) :: fields
  in
  let fields = match pi.patch with
    | None -> fields
    | Some op -> ("patch", operation_to_json op) :: fields
  in
  let fields = match pi.options with
    | None -> fields
    | Some op -> ("options", operation_to_json op) :: fields
  in
  let fields = match pi.head with
    | None -> fields
    | Some op -> ("head", operation_to_json op) :: fields
  in
  let fields = if pi.path_parameters = [] then fields
    else ("parameters", `List (List.map parameter_to_json pi.path_parameters)) :: fields
  in
  `Assoc fields

let security_scheme_to_json scheme =
  let type_fields = match scheme.scheme_type with
    | ApiKey { name; scheme_in } ->
      [("type", `String "apiKey"); ("name", `String name); ("in", `String (param_in_to_string scheme_in))]
    | Http { scheme; bearer_format } ->
      let fields = [("type", `String "http"); ("scheme", `String scheme)] in
      (match bearer_format with
       | None -> fields
       | Some bf -> ("bearerFormat", `String bf) :: fields)
    | OAuth2 { flows = _ } ->
      [("type", `String "oauth2")]  (* Simplified for now *)
    | OpenIdConnect { openid_connect_url } ->
      [("type", `String "openIdConnect"); ("openIdConnectUrl", `String openid_connect_url)]
  in
  let fields = match scheme.scheme_description with
    | None -> type_fields
    | Some d -> ("description", `String d) :: type_fields
  in
  `Assoc fields

let components_to_json comp =
  let fields = [] in
  let fields = if comp.schemas = [] then fields
    else ("schemas", `Assoc (List.map (fun (n, s) -> (n, schema_to_json s)) comp.schemas)) :: fields
  in
  let fields = if comp.security_schemes = [] then fields
    else ("securitySchemes", `Assoc (List.map (fun (n, s) -> (n, security_scheme_to_json s)) comp.security_schemes)) :: fields
  in
  let fields = if comp.parameters = [] then fields
    else ("parameters", `Assoc (List.map (fun (n, p) -> (n, parameter_to_json p)) comp.parameters)) :: fields
  in
  let fields = if comp.responses = [] then fields
    else ("responses", `Assoc (List.map (fun (n, r) -> (n, response_to_json r)) comp.responses)) :: fields
  in
  `Assoc fields

let info_to_json info =
  let fields = [
    ("title", `String info.title);
    ("version", `String info.version);
  ] in
  let fields = match info.description with
    | None -> fields
    | Some d -> ("description", `String d) :: fields
  in
  let fields = match info.terms_of_service with
    | None -> fields
    | Some t -> ("termsOfService", `String t) :: fields
  in
  let fields = match info.contact with
    | None -> fields
    | Some c ->
      let cf = [] in
      let cf = match c.contact_name with None -> cf | Some n -> ("name", `String n) :: cf in
      let cf = match c.contact_url with None -> cf | Some u -> ("url", `String u) :: cf in
      let cf = match c.contact_email with None -> cf | Some e -> ("email", `String e) :: cf in
      ("contact", `Assoc cf) :: fields
  in
  let fields = match info.license with
    | None -> fields
    | Some l ->
      let lf = [("name", `String l.license_name)] in
      let lf = match l.license_url with None -> lf | Some u -> ("url", `String u) :: lf in
      ("license", `Assoc lf) :: fields
  in
  `Assoc fields

let server_to_json srv =
  let fields = [("url", `String srv.url)] in
  let fields = match srv.server_description with
    | None -> fields
    | Some d -> ("description", `String d) :: fields
  in
  `Assoc fields

let tag_to_json t =
  let fields = [("name", `String t.tag_name)] in
  let fields = match t.tag_description with
    | None -> fields
    | Some d -> ("description", `String d) :: fields
  in
  `Assoc fields

let to_json spec =
  let fields = [
    ("openapi", `String spec.openapi);
    ("info", info_to_json spec.info);
  ] in
  let fields = if spec.servers = [] then fields
    else ("servers", `List (List.map server_to_json spec.servers)) :: fields
  in
  let fields =
    let paths_json = `Assoc (List.map (fun (p, pi) -> (p, path_item_to_json pi)) (List.rev spec.paths)) in
    ("paths", paths_json) :: fields
  in
  let fields = match spec.components with
    | None -> fields
    | Some c -> ("components", components_to_json c) :: fields
  in
  let fields = if spec.tags = [] then fields
    else ("tags", `List (List.map tag_to_json spec.tags)) :: fields
  in
  `Assoc (List.rev fields)

let to_json_string ?(pretty = true) spec =
  let json = to_json spec in
  if pretty then Yojson.Safe.pretty_to_string json
  else Yojson.Safe.to_string json

(** {1 YAML Export} *)

(** Convert to YAML string (simplified, JSON-compatible YAML) *)
let to_yaml_string spec =
  (* For simplicity, output JSON which is valid YAML *)
  to_json_string ~pretty:true spec

(** {1 Swagger UI} *)

(** Swagger UI HTML template *)
let swagger_ui_html ~spec_url ~title =
  Printf.sprintf {|<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>%s - Swagger UI</title>
  <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/swagger-ui-dist@5/swagger-ui.css">
</head>
<body>
  <div id="swagger-ui"></div>
  <script src="https://cdn.jsdelivr.net/npm/swagger-ui-dist@5/swagger-ui-bundle.js"></script>
  <script>
    window.onload = function() {
      SwaggerUIBundle({
        url: "%s",
        dom_id: '#swagger-ui',
        presets: [
          SwaggerUIBundle.presets.apis,
          SwaggerUIBundle.SwaggerUIStandalonePreset
        ],
        layout: "BaseLayout"
      });
    };
  </script>
</body>
</html>|} title spec_url

(** ReDoc HTML template *)
let redoc_html ~spec_url ~title =
  Printf.sprintf {|<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>%s - API Documentation</title>
  <link href="https://fonts.googleapis.com/css?family=Roboto:300,400,700" rel="stylesheet">
  <style>body { margin: 0; padding: 0; }</style>
</head>
<body>
  <redoc spec-url="%s"></redoc>
  <script src="https://cdn.redoc.ly/redoc/latest/bundles/redoc.standalone.js"></script>
</body>
</html>|} title spec_url

(** {1 Route Integration} *)

(** Empty path item *)
let empty_path_item = {
  path_summary = None;
  path_description = None;
  get = None;
  post = None;
  put = None;
  delete = None;
  patch = None;
  options = None;
  head = None;
  path_parameters = [];
}

(** Get JSON spec handler (for Kirin integration)
    Usage: Router.get "/openapi.json" (fun _ -> Response.json (Openapi.to_json spec))
*)
let spec_json spec = to_json spec

(** Get Swagger UI handler (for Kirin integration)
    Usage: Router.get "/docs" (fun _ -> Response.html (Openapi.swagger_ui spec))
*)
let swagger_ui ?(spec_url = "/openapi.json") spec =
  swagger_ui_html ~spec_url ~title:spec.info.title

(** Get ReDoc handler (for Kirin integration)
    Usage: Router.get "/docs/redoc" (fun _ -> Response.html (Openapi.redoc spec))
*)
let redoc ?(spec_url = "/openapi.json") spec =
  redoc_html ~spec_url ~title:spec.info.title
